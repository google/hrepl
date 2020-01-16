{-# LANGUAGE OverloadedStrings #-}
module RuleInfo.ExecutionRoot
    ( ExecutionRoot
    , executionRootPath
    , getExecutionRoot
    , fixFilePathFlags
    , toAbsolute
    , toAbsoluteT
    , fakeExecutionRoot -- For testing
    ) where

import Control.Monad (guard)
import Data.List.Extra (firstJust)
import qualified Data.Text as T
import Data.Text (Text)
import Bazel (BazelOpts, bazelInfo)
import System.FilePath ((</>))

newtype ExecutionRoot = ExecutionRoot FilePath

getExecutionRoot :: BazelOpts -> IO ExecutionRoot
getExecutionRoot opts = ExecutionRoot <$> bazelInfo opts "execution_root"

-- | Construct an execution root from a filepath.  Should be used for testing.
fakeExecutionRoot :: FilePath -> ExecutionRoot
fakeExecutionRoot = ExecutionRoot

executionRootPath :: ExecutionRoot -> FilePath
executionRootPath (ExecutionRoot f) = f

-- | Some flags include google3-relative file paths, and they don't work with
-- ghcide when invoked from a subdirectory of google3. Prepends them with the
-- execution root directory.
fixFilePathFlags :: ExecutionRoot -> [Text] -> [Text]
fixFilePathFlags _ [] = []
-- Handles cases like "-foo" "bar".
fixFilePathFlags root (f : arg : rest)
  | Just (prefix, path) <- filePathToFix f arg =
    f : prefix <> toAbsoluteT root path : fixFilePathFlags root rest
-- Handles cases like "-foo=bar".
fixFilePathFlags root (x : xs)
  | not (T.null arg),
    Just (prefix, path) <- filePathToFix f (T.drop 1 arg) =
    mconcat [f, "=", prefix, toAbsoluteT root path] : fixFilePathFlags root xs
  | otherwise = x : fixFilePathFlags root xs
  where
    (f, arg) = T.break (== '=') x

-- | Converts the file path argument to an absolute path form.
toAbsolute :: ExecutionRoot -> FilePath -> FilePath
toAbsolute (ExecutionRoot root) f = root </> f

-- | Converts the file path argument to an absolute path form.
toAbsoluteT :: ExecutionRoot -> Text -> Text
toAbsoluteT root = T.pack .toAbsolute root . T.unpack

filePathToFix :: Text -> Text -> Maybe (Text, Text)
filePathToFix curr next = firstJust matchFlag filePathFlags
  where
    matchFlag :: FilePathFlag -> Maybe (Text, Text)
    matchFlag (FPF s) = guard (curr == s) >> return ("", next)
    matchFlag (FPFPrefixed s prefix) = do
      guard (curr == s)
      next' <- T.stripPrefix prefix next
      return (prefix, next')

data FilePathFlag
  = -- | Next flag is a file path.
    -- E.g., -pgmP some/path/clang ==> FPF "-pgmP"
    FPF Text
  | -- | Next flag is a file path prefixed with the second argument.
    -- E.g., -optP-isystem -optPsome/header/file.h
    --       ==> FPFPrefixed "-optP-isystem" "-optP"
    FPFPrefixed Text Text
  deriving (Show)

-- | Flags that accept a file path as their argument.
filePathFlags :: [FilePathFlag]
filePathFlags =
  [ FPF "-opta--sysroot",
    FPF "-optc--sysroot",
    FPF "-optl--sysroot",
    FPF "-package-db",
    FPF "-pgmP",
    FPF "-pgma",
    FPF "-pgmc",
    FPF "-pgml",
    FPFPrefixed "-optP-isystem" "-optP",
    FPFPrefixed "-optP-iquote" "-optP"
  ]
