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

{-# LANGUAGE OverloadedStrings #-}

-- | Functions for invoking "bazel" as part of other command-line tools.
--
-- These functions attempt to remove noise from the output of Bazel
-- and to invoke it with parameters that make it faster.
module Bazel
  ( BazelOpts (..),
    defBazelOpts,
    bazelCmd,
    bazelCmd_,
    bazelBuild,
    bazelQuery,
    fileToHsTargets,
    BinDir (..),
    getBinDir,
    GenfilesDir (..),
    getGenfilesDir,
    bazelInfo,
    bazelShutdown,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception (ErrorCall (..), bracket, catch)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Bazel.Query as Q
import Bazel.Name (Label (..), parseLabel)
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr)
import qualified System.IO as IO
import System.Process (CreateProcess (..), StdStream (CreatePipe))
import qualified System.Process as Process

-- | Customization of bazel command line.
data BazelOpts
  = BazelOpts
      { -- | The command to use to invoke bazel.
        bazelBin :: String,
        -- | Options (e.g. --batch) preceding the command (e.g. build)
        bazelPre :: [String],
        -- | Options (e.g. --c opt) following the command (e.g. build)
        bazelPost :: [String],
        bazelShowCommands :: Bool,
        -- | Working directory to start bazel commands from.
        bazelCwd :: Maybe FilePath
      }
  deriving (Show)

-- | Defaults to use when special customization is not needed.
defBazelOpts :: BazelOpts
defBazelOpts =
  BazelOpts
    "bazel"
    []
    -- Speed up invocations of Bazel by setting some defaults to omit things
    -- we don't use.
    [ "--nostamp"
    , "--workspace_status_command=true"
    ]
    False -- Don't show commands
    Nothing

-- | Run Bazel with the given command and arguments, and capture its stdout.
bazelCmd :: BazelOpts -> String -> [String] -> IO ByteString
bazelCmd opts cmd args = do
  allArgs <- getBazelArgs opts cmd args
  Process.withCreateProcess
    (Process.proc (bazelBin opts) allArgs)
      { std_out = CreatePipe,
        std_err = CreatePipe,
        cwd = bazelCwd opts
      }
    $ \Nothing (Just hout) (Just herr) ph -> do
      ((), out) <-
        concurrently
          (filterInfo herr)
          (B.hGetContents hout)
      ec <- Process.waitForProcess ph
      when (ec /= ExitSuccess)
        $ error
        $ "bazelCmd: Failed running: "
          ++ Process.showCommandForUser
            (bazelBin opts)
            allArgs
      return out

-- | Run Bazel with the given command and arguments, while sharing stdout.
bazelCmd_ :: BazelOpts -> String -> [String] -> IO ()
bazelCmd_ opts cmd args = do
  allArgs <- getBazelArgs opts cmd args
  Process.withCreateProcess
    (Process.proc (bazelBin opts) allArgs)
      { std_err = CreatePipe,
        cwd = bazelCwd opts
      }
    $ \Nothing Nothing (Just herr) ph -> do
      filterInfo herr
      ec <- Process.waitForProcess ph
      when (ec /= ExitSuccess)
        $ error
        $ "bazelCmd_: Failed running: "
          ++ Process.showCommandForUser
            (bazelBin opts)
            allArgs

-- | Continually reads text from the given Handle and outputs it to stderr.
-- Attempts to drop "INFO:" lines from the output.
-- Unfortunately, there's no way to directly tell Bazel to drop them while
-- still preserving Bazel's useful progress messages:
-- https://groups.google.com/forum/#!topic/bazel-discuss/JVCotYRXeyk
-- https://groups.google.com/forum/#!topic/bazel-discuss/JVCotYRXeyk
-- Filtering *all* of stderr would be too aggressive; we still want to show
-- Bazel's progress messages, to avoid looking like we've hung:
filterInfo :: Handle -> IO ()
filterInfo h = do
  -- Assume that the output from Bazel is utf8-encoded, and set input/output
  -- handles appropriately.
  -- (When run from within a test, the LANG environment variable isn't set,
  -- and the default encoding is ASCII which errors on any nontrivial
  -- Unicode.)
  IO.hSetEncoding h IO.utf8
  bracket
    (IO.hGetEncoding IO.stderr)
    (IO.hSetEncoding IO.stderr . fromMaybe IO.char8)
    $ const
    $ do
      IO.hSetEncoding stderr IO.utf8
      loop False
  where
    -- Drop every line that contains "INFO:", with the following caveat:
    -- If the line contains "INFO:" and the previous one did not,
    -- keep the initial contents of that line, since it
    -- may contain control sequences that clean up output from previous lines.
    loop prevInfo = do
      eof <- IO.hIsEOF h
      unless eof $ T.hGetLine h >>= putLine prevInfo >>= loop
    putLine prevInfo l = case T.breakOn info l of
      (l', l'')
        | T.null l'' -> do
          -- Not an INFO: line: just print it.
          T.hPutStr stderr l
          -- The last line of output from Bazel doesn't need a trailing
          -- newline.
          eof <- IO.hIsEOF h
          unless eof $ IO.hPutChar stderr '\n'
          pure False
        | otherwise -> do
          -- An INFO: line: if it's the first consecutive one, print the
          -- contents before the "INFO:".
          unless prevInfo $ T.hPutStr stderr l'
          pure True
    info = "INFO:"

getBazelArgs :: BazelOpts -> String -> [String] -> IO [String]
getBazelArgs opts cmd args = do
  isTerm <- IO.hIsTerminalDevice stderr
  let allArgs =
        bazelPre opts <> [cmd] <> args <> bazelPost opts
          -- Tell Bazel whether we're running in a terminal.
          -- Since we're capturing stderr, Bazel can't detect it automatically.
          <> if isTerm then ["--curses=yes", "--color=yes"] else []
  when (bazelShowCommands opts)
    $ IO.hPutStrLn stderr
    $ "Running: " ++ Process.showCommandForUser (bazelBin opts) allArgs
  return allArgs

-- | Run "bazel build" with the given arguments.
bazelBuild :: BazelOpts -> [String] -> IO ()
bazelBuild opts = bazelCmd_ opts "build"

-- | Run "bazel cquery" with the given query, and parse the resulting list
-- of targets.
bazelQuery :: BazelOpts -> Q.Query -> IO [Label]
bazelQuery opts query = case Q.nonEmptyQuery query of
    Nothing -> return []  -- Empty query
    Just query' -> do
        -- --build_manual_tests is needed for "bazel cquery" to find test targets
        -- tagged with "manual".
        -- TODO(jinwoo): Delete --build_manual_tests after the fix of b/145818695
        -- becomes live.
        let opts' = opts {bazelPost = "--build_manual_tests" : bazelPost opts}
        map (parseLabel . firstWord) . T.lines . T.decodeUtf8
            <$> bazelCmd opts' "cquery" [Q.renderQueryStr query']
        where
            -- "bazel cquery" outputs in the format "//path/to:rule (hash-of-config)".
            -- For our purposes, we only want the first word in the line.
            firstWord = T.takeWhile (/= ' ')

-- | The location of Bazel output files.
newtype BinDir = BinDir {binDirPath :: FilePath}

-- | Retrieve the location of Bazel output files.
getBinDir :: BazelOpts -> IO BinDir
getBinDir opts = BinDir <$> bazelInfo opts "bazel-bin"

-- | The location of Bazel genfiles directory.
newtype GenfilesDir = GenfilesDir {genfilesPath :: FilePath}

-- | Retrieve the location of Bazel genfiles directory.
getGenfilesDir :: BazelOpts -> IO GenfilesDir
getGenfilesDir opts = GenfilesDir <$> bazelInfo opts "bazel-genfiles"

-- | Look up the bazel-info value for a particular key.
bazelInfo :: BazelOpts -> String -> IO String
bazelInfo opts key =
  takeWhile (/= '\n') . T.unpack . T.decodeUtf8
    <$> bazelCmd opts "info" [key]

bazelShutdown :: BazelOpts -> IO ()
bazelShutdown opts = bazelCmd_ opts' "shutdown" []
  where
    -- "bazel shutdown" doesn't take the arguments of "bazel  build".
    -- Still pass it the "startup" commands, for example so it can disambiguate
    -- between "output_bases".
    opts' = opts {bazelPost = []}

-- | Returns Haskell build targets that have the given file as one of their
-- source files. An empty list is returned when the given file doesn't have an
-- associated BUILD file.
-- 'file' can be either
--   - an absolute path in the current workspace directory, or in the execroot
--   - a relative path to the current directory
fileToHsTargets :: BazelOpts -> FilePath -> IO [Label]
fileToHsTargets opts file =
      regularFileToHsTargets opts file
    -- TODO(b/143647290): More disciplined error handling.
    `catch` ( \e@(ErrorCall _) -> do
                IO.hPutStrLn IO.stderr $ "fileToHsTargets: " ++ show e
                return []
            )

-- | Returns Haskell build targets for the given regular source file.
regularFileToHsTargets :: BazelOpts -> FilePath -> IO [Label]
regularFileToHsTargets opts file = do
  -- Get the build label notation for the given file.
  srcLabels <- fileToLabels opts file
  srcsToTargets opts srcLabels

-- | Converts a source file path to build labels.
fileToLabels :: BazelOpts -> FilePath -> IO [Label]
fileToLabels opts f = bazelQuery opts (Q.strWord f)

-- | Finds build targets for the given source labels.
srcsToTargets :: BazelOpts -> [Label] -> IO [Label]
srcsToTargets opts srcLabels =
  concat
    <$> mapM
      ( \lbl@(Label _ pkg _) ->
          -- Get all the build targets for the given file.
          bazelQuery opts
            $ Q.kind "haskell_"
            $ Q.attr
              "srcs"
              (labelToRegex lbl)
              (Q.strWord $ show pkg ++ ":*")
      )
      srcLabels

-- | Converts a build label to a regex form for an exact match.
-- See https://docs.bazel.build/versions/master/query.html#regex
labelToRegex :: Label -> T.Text
labelToRegex lbl = mconcat ["\"", T.pack . show $ lbl, "($|,|\\]$)", "\""]
