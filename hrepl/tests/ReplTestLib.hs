-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE RecordWildCards #-}
-- | Library for building integration tests for hrepl.
--
-- This test is built to run somewhat hermetically, running with a temporary
-- output_base and against a given source client (passed via the
-- "HREPL_TEST_CLIENT" environment variable.  The reason for such
-- requirements is a gigantic set of bzl and Haskell toolchain
-- dependencies that are not factored into publicly visible
-- targets. Moreover, the dependencies are likely unstable as they are
-- implementation artifacts and not designed interfaces.
module ReplTestLib
    ( TestScript(..)
    , hreplTest
    ) where

import Prelude hiding (readFile)

import Control.Monad (unless)
import Bazel(BazelOpts(..), bazelShutdown, defBazelOpts)
import qualified Bazel.Runfiles as Runfiles
import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, hPutStrLn)
import System.IO.Strict (readFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process ( CreateProcess(..), StdStream(..), proc, waitForProcess
                      , withCreateProcess
                      )
import Test.HUnit (assertEqual)
import Test.HUnit.Lang (Assertion)

newtype TestDirs = TestDirs
    { baseDir :: FilePath
    }

-- | Runs the test with the given script and verifies the output
-- printed to argv[0] matches the expected.
hreplTest :: TestScript -> String -> Assertion
hreplTest t expected = do
    dirs <- TestDirs <$> getEnv "HREPL_TEST_CLIENT"
    got <- runHrepl t dirs
    assertEqual "Unexpected result" expected got

-- Parameters for runTest.
data TestScript = TestScript
    { tsUserArgs :: [String]
    , tsStdin :: FilePath -> String  -- The parameter is the result file name
    }

-- | Convenient options for running bazel in isolation.
testBazelOpts :: TestDirs -> FilePath -> BazelOpts
testBazelOpts testDirs tmpD =
    defBazelOpts
    { bazelPre = bazelPre defBazelOpts ++
                 [ -- Isolates from any config settings.
                   "--bazelrc=/dev/null"
                   -- Redirect outputs to a temporary location
                 , "--output_base=" ++ tmpD </> "output-base"
                 ]
    , bazelPost = bazelPost defBazelOpts ++
                [ "--noshow_progress"
                -- TODO(judahjacobson): This was used to work around
                -- b/111500525; see if we can remove it and have the test
                -- still be robust.
                , "--features=-layering_check"
                , "--verbose_failures"
                  -- Don't output symlinks, since the SrcFS dir is read-only.
                , "--symlink_prefix=/"
                ]
    , bazelShowCommands = True
    }

runHrepl :: TestScript -> TestDirs -> IO String
runHrepl TestScript{..} testDirs =
    withSystemTempDirectory "hrepl_test." $ \tmpD -> do
        rfiles <- Runfiles.create
        path <- getEnv "PATH"
        let hrepl = Runfiles.rlocation rfiles "hrepl/hrepl/hrepl"
        let bazelOpts = testBazelOpts testDirs tmpD
            args = [ "--bazel"
                   , bazelBin bazelOpts
                   , "--bazel-pre-args"
                   , unwords $ bazelPre bazelOpts
                   , "--bazel-args"
                   , unwords $ bazelPost bazelOpts
                   , "--show-commands"
                   ]
                   ++ tsUserArgs
            cp = (proc hrepl args) { cwd = Just (baseDir testDirs)
                                   -- TODO: don't pass PATH through.
                                   -- It's currently needed for ghc_bindist to find the
                                   -- `python` executable.
                                   , env = Just [("HOME", tmpD), ("PATH", path)]
                                   , std_in = CreatePipe
                                   }
        withCreateProcess cp $ \(Just stdin) _ _ ph -> do
            -- Uses a distinct explicit result file instead of stdout or stderr.
            -- Both of these are polluted by hrepl, bazel, and ghci.
            let output = tmpD </> "result"
            hPutStrLn stdin (tsStdin output)
            hPutStrLn stdin ":quit"
            hClose stdin

            exitCode <- waitForProcess ph
            unless (exitCode == ExitSuccess) $
                error $ "Hrepl failed with: " ++ show exitCode
            -- Shut down the async bazel process to prevent test flakiness
            -- and zombie bazel processes.
            bazelShutdown bazelOpts

            readFile output
