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
--
-- You may also share the same output directory across runs by setting
-- HREPL_TEST_OUTPUT.  This setting is useful in particular for caching
-- build outputs.
module ReplTestLib
    ( TestScript(..)
    , hreplTest
    ) where

import Prelude hiding (readFile)

import Control.Exception (bracket)
import Control.Monad (unless)
import Bazel(BazelOpts(..), bazelClean, bazelShutdown, defBazelOpts)
import qualified Bazel.Runfiles as Runfiles
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnv, lookupEnv)
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

data TestDirs = TestDirs
    { clientDir :: FilePath
    , outputDir :: FilePath
    }

withTestDirs :: (TestDirs -> IO a) -> IO a
withTestDirs act = do
    client <- getEnv "HREPL_TEST_CLIENT"
    let run output = act TestDirs { clientDir = client, outputDir = output }
    maybeOutputDir <- lookupEnv "HREPL_TEST_OUTPUT"
    case maybeOutputDir of
        Nothing -> withSystemTempDirectory "hrepl_test_output" run
        Just o -> run o

-- | Runs the test with the given script and verifies the output
-- printed to argv[0] matches the expected.
hreplTest :: TestScript -> String -> Assertion
hreplTest t expected = do
    got <- withTestDirs (runHrepl t)
    assertEqual "Unexpected result" expected got

-- Parameters for runTest.
data TestScript = TestScript
    { tsUserArgs :: [String]
    , tsStdin :: FilePath -> String  -- The parameter is the result file name
    }

-- | Convenient options for running bazel in isolation.
testBazelOpts :: TestDirs -> BazelOpts
testBazelOpts testDirs =
    defBazelOpts
    { bazelPre = bazelPre defBazelOpts ++
                 [ -- Isolates from any config settings.
                   "--bazelrc=/dev/null"
                   -- Redirect outputs to a temporary location
                 , "--output_base=" ++ outputDir testDirs </> "output-base"
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
runHrepl TestScript{..} testDirs = do
    -- Share the same disk cache across runs in the same HREPL_OUTPUT_BASE.
    writeFile (outputDir testDirs </> ".bazelrc")
        $ "build --disk_cache=" ++ outputDir testDirs </> "cache"
    rfiles <- Runfiles.create
    path <- getEnv "PATH"
    let hrepl = Runfiles.rlocation rfiles "hrepl/hrepl/hrepl"
    let bazelOpts = testBazelOpts testDirs
        args = [ "--bazel"
                , bazelBin bazelOpts
                , "--bazel-pre-args"
                , unwords $ bazelPre bazelOpts
                , "--bazel-args"
                , unwords $ bazelPost bazelOpts
                , "--show-commands"
                ]
                ++ tsUserArgs
        cp = (proc hrepl args)
                { cwd = Just (clientDir testDirs)
                -- TODO: don't pass PATH through.
                -- It's currently needed for ghc_bindist to find the
                -- `python` executable.
                , env = Just [("HOME", outputDir testDirs), ("PATH", path)]
                , std_in = CreatePipe
                }
    -- Clean up any previous builds (such as when sharing HREPL_OUTPUT_BASE)
    bracket getCurrentDirectory setCurrentDirectory
        $ const $ do
            setCurrentDirectory (clientDir testDirs)
            bazelClean bazelOpts
    let output = outputDir testDirs </> "result"
    -- If the output file already exists, overwrite any previous values.
    -- If it doesn't exist, prevent confusing "file not found" errors in case the
    -- test fails and doesn't append anything to it.
    writeFile output ""
    withCreateProcess cp $ \(Just stdin) _ _ ph -> do
        -- Uses a distinct explicit result file instead of stdout or stderr.
        -- Both of these are polluted by hrepl, bazel, and ghci.
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
