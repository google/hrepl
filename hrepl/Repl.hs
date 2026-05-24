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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl (runWith, ReplOptions(..)) where

import Control.Monad (forM_, when, unless)
import Data.List (isPrefixOf, isSuffixOf, partition)
import Data.Map.Strict (Map)
import Data.ProtoLens.Labels ()
import Data.Semigroup (Semigroup(..))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Lens.Micro ((^.))
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.Process (CreateProcess(..))
import qualified System.Process as Process
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import qualified System.IO as IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files.ByteString (createSymbolicLink)

import Bazel (BazelOpts (..), bazelQuery, fileToHsTargets)
import qualified Bazel.Query as Q
import Bazel.BuildOutput (collectBuildOutputs)
import RuleInfo
    ( BuildOptions(..)
    , buildDependentPackages
    , buildOptionsOutput
    , GhcConfig
    , getGhcConfig
    , transitivePackageSetFlags
    )
import RuleInfo.ExecutionRoot
    ( ExecutionRoot
    , fixFilePathFlags
    , getExecutionRoot
    , toAbsolute
    , toAbsoluteT
    )
import ModuleName
    ( parseModuleNameFromFile
    , ModuleName
    , ParseResult(..)
    , prettyPrint
    )
import Bazel.Name (Label, PackageName)

-- | Expands the given targets to include all in-between targets.
-- Also includes any haskell_library dependencies that are under one of the
-- "interpretDeps" packages.
--
-- This is needed so all those targets are managed by GHCi by source. Otherwise
-- code changes to intermediate targets wouldn't be picked up on reload.
getAllIntermediateTargets :: BazelOpts -> [PackageName] -> [Label] -> IO [Label]
getAllIntermediateTargets bazelOpts interpretDeps targetLabels =
    bazelQuery bazelOpts queryAllPaths
  where
    queryAllPaths =
      Q.kind "\"haskell_binary|haskell_library|haskell_test\"" $
        let targetQueries = map Q.labelToQuery targetLabels
         in Q.letIn ("ts", Q.union targetQueries) $
            Q.letIn ("tds", Q.var "ts"
                              Q.<+> packageNameFilter interpretDeps
                                      (Q.kind "haskell_library"
                                          (Q.deps (Q.var "ts")))) $
            Q.allpaths (Q.var "tds") (Q.var "tds")

-- | Returns a query function that will only match targets in the given
-- packages.  If the list of directories is empty, the result will always
-- evaluate to the empty query.
--
-- Assumes that the given paths are relative to the current directory.
packageNameFilter :: [PackageName] -> Q.Query -> Q.Query
packageNameFilter [] = const Q.empty
packageNameFilter ps =
    -- A naive implementation would be: "$ds ^ (PKG/... + PKG/...)"
    -- However, PKG/... evaluates every BUILD file in all of PKG's
    -- subdirectories, which is too slow:
    -- https://docs.bazel.build/versions/master/query.html#filter
    -- Instead, we'll use "filter(PATTERN, $ds)"
    -- where PATTERN is "PKG(/|:)|PKG(/|:)"
    let packagePattern =
          "'"
          <> Text.intercalate "|"
                [Text.pack (show p ++ "(/|:)") | p <- ps]
          <> "'"
    in Q.filterQ packagePattern

-- | Use Bazel to expand the given list of target patterns (e.g. potentially
-- including wildcards) into a list of labels of single targets.
bazelExpandTargetsToLabels :: BazelOpts -> [String] -> IO [Label]
bazelExpandTargetsToLabels bazelOpts strs = do
    let (hsFiles, nonHsFiles) = partition (".hs" `isSuffixOf`) strs
    resultss <- mapM (fileToHsTargets bazelOpts) hsFiles
    results <- bazelQuery bazelOpts . Q.union . map Q.strWord $ nonHsFiles
    pure $ concat resultss <> results

-- | Write the given GHCi script to a file.
writeGhciScript :: Text -> FilePath -> IO FilePath
writeGhciScript contents dir = do
    Text.writeFile filePath contents
    return filePath
  where filePath = dir </> "repl.ghci"

data ReplOptions = ReplOptions
  { replBazelOpts :: BazelOpts
  , replCompiledTargets :: [String]
  , replRtsOpts :: String
  , replInterpretDeps :: [PackageName]
  , replUserArgs :: [String]
  , replTargets :: [String]
  } deriving Show

-- | Main entrypoint of repl.
runWith :: ReplOptions -> IO ()
runWith ReplOptions
    { replBazelOpts = bazelOpts
    , replTargets = targets
    , replRtsOpts = rtsOpts
    , replInterpretDeps = interpretDeps
    , replUserArgs = userArgs
    , replCompiledTargets = compiledTargets } = do
    -- Build the GHC config first.  It will trigger the GHC repository rule,
    -- which can print junk to stdout and corrupt the output of "bazel query".
    ghcConfig <- getGhcConfig bazelOpts

    targetLabels <- bazelExpandTargetsToLabels bazelOpts targets
    allTargets <- getAllIntermediateTargets bazelOpts interpretDeps targetLabels
    compiledLabels <- bazelExpandTargetsToLabels bazelOpts compiledTargets

    -- The "execution root" directory contains all the source and output
    -- files from Bazel.  It's usually symlinked from "bazel-{workspace}".
    execRoot <- getExecutionRoot bazelOpts

    -- Build dependencies first, then non-root targets, then the root.
    -- The execution_root (bazel-{repo}) will only contain source files
    -- used transitively by the last invocation of "bazel build".
    -- So start at the bottom of the build graph and then work our way up.
    -- (We can't build them all at once because they need different
    -- output_groups.)
    -- TODO: make this logic obsolete by not using the bazel-{repo}
    -- directory (b/118790965).
    dependencies <- buildDependentPackages bazelOpts
                                        compiledLabels allTargets
    let depFlags = transitivePackageSetFlags execRoot dependencies
    buildOptions <- mconcat <$>
        collectBuildOutputs bazelOpts buildOptionsOutput allTargets

    -- Run GHC, linking in the FFI shared library via the "ffi-deps" package.
    -- BuildOptions contains the module names for all built targets; we
    -- only want to import modules for the targets from the command line.
    let filesIn label =
                M.findWithDefault S.empty label (sourceFiles buildOptions)
        allSourceFiles = M.foldl' S.union S.empty (sourceFiles buildOptions)
    explicitTargetModules
        <- readTargetModules $ map (toAbsolute execRoot . Text.unpack)
                $ S.toList $ foldMap filesIn targetLabels

    -- Generate a GHCi script to load all source files and then import the
    -- modules for targets that were explicitly specified on the command-line.
    -- Using ":module +" makes GHCi expose the contents of one of the inputs.
    -- It's arbitrary, but better than the other options:
    -- - If we try to expose *none* of the modules, on :reload GHCi will expose
    --   one of them anyway, which is a confusing user experience.
    -- - In the common case of a single module, that module's contents will be
    --   exposed, which is probably what the user wants.
    let spaceDelimit = Text.intercalate " " . S.toList
        adds    = ":add "    <> spaceDelimit
                                (S.map (toAbsoluteT execRoot) allSourceFiles)
        imports = ":module +" <> spaceDelimit (S.map (Text.pack . prettyPrint)
                                                  explicitTargetModules)
        scriptContents = Text.unlines [adds, imports]
    -- Parent directory for all temporary files.
    withSystemTempDirectory "repl" $ \tmpDir -> do
        -- Temporary directory for output files (in particular, FFI "*.stub.h"),
        -- and runfiles.
        let outputDir = tmpDir </> "output"
            runfilesDir = tmpDir </> "runfiles"
        forM_ [outputDir, runfilesDir] $ createDirectoryIfMissing True
        -- Collect the runfiles into a temporary directory.
        makeRunfilesDir runfilesDir execRoot (runfiles buildOptions)

        scriptFile <- writeGhciScript scriptContents tmpDir

        let rtsOptions = words rtsOpts
            rtsArgs = if null rtsOptions
                         then []
                         else ["+RTS"] ++ rtsOptions ++ ["-RTS"]

        externalEnv <- getEnvironment
        let newEnv = ("RUNFILES_DIR", runfilesDir) : externalEnv
            ghc = toAbsolute execRoot $ Text.unpack $ ghcConfig ^. #ghc
            libdir = ghcConfig ^. #libraryRoot
            allArgs = [ "-B" ++ toAbsolute execRoot (Text.unpack libdir)
                      | not (Text.null libdir)] ++
                      [ "--interactive"
                      , "-outputdir", outputDir
                      , "-ghci-script", scriptFile
                      ] ++ ghcBuildArgs execRoot ghcConfig buildOptions
                      ++ rtsArgs
                      ++ depFlags
                      ++ userArgs
        when (bazelShowCommands bazelOpts) $
            IO.hPutStrLn IO.stderr $
            "Running: " ++ Process.showCommandForUser ghc allArgs
        Process.withCreateProcess (Process.proc (toAbsolute execRoot ghc) allArgs)
            { env = Just newEnv
            , delegate_ctlc = True
            }
            $ \_ _ _ ph -> do
              ec <- Process.waitForProcess ph
              unless (ec == ExitSuccess) $
                  IO.hPutStrLn IO.stderr
                      $ if bazelShowCommands bazelOpts
                          then "Error running GHCi: "
                                  ++ Process.showCommandForUser ghc allArgs
                          else "Error running GHCi."

-- | Arguments to pass to GHC, extracted from the accumulated buildOptions
-- for each target we're interpreting
ghcBuildArgs :: ExecutionRoot -> GhcConfig -> BuildOptions -> [String]
ghcBuildArgs execRoot ghcConfig buildOptions =
    filter suitableOption (map Text.unpack
                            $ fixFilePathFlags execRoot
                            $ ghcConfig ^. #commonOptions
                              ++ ghcOptions buildOptions)
    ++ concatMap (sharedLibArgs . Text.unpack)
            (S.toList $ transitiveCcLibs buildOptions)
    -- Don't pick up any source files unless they're explicitly passed on the
    -- command-line (i.e., specified in a "srcs" attribute).
    ++ ["-i"]
    -- Hide all transitive dependencies unless they're explicitly requested.
    -- TODO(#4): Consider whether to expose more by default.
    -- But *do* always expose the "base" package, since ghci breaks otherwise.
    ++ ["-hide-all-packages", "-package=base"]
    -- Override previous flags: don't treat warnings as errors.
    ++ ["-Wwarn"]
  where
    -- Links directly against the shared libraries.
    sharedLibArgs f =
        [ "-L" ++ toAbsolute execRoot (takeDirectory f)
        , "-l" ++ sharedLibName f
        , "-optl-Wl,-rpath=" ++ toAbsolute execRoot (takeDirectory f)
        ]
    -- Convert "foo/.../libbar.so" to "bar"
    sharedLibName = drop 3 . takeBaseName

-- | Whether an option is appropriate for repl.
suitableOption :: String -> Bool
-- It's confusing when users don't see ghci's interactive output; it
-- looks like the script is stalled (b/78289945)
suitableOption "-v0" = False
-- For simplicity, don't distinguish between regular deps (-package-id) and
-- plugins (which otherwise would require a separate -plugin-package-id flag).
suitableOption "-hide-all-plugin-packages" = False
-- rules_haskell emits "-i" flags which can make hrepl incorrectly interpret targets
-- (instead of compiling them as expected).
suitableOption ('-':'i':_) = False
-- When repl loads multiple targets, their CURRENT_PACKAGE_KEYs will conflict
-- and register a warning.
suitableOption p
    | "-optP-DCURRENT_PACKAGE_KEY" `isPrefixOf` p = False
    -- ghci ignores "-O", "-O2", etc. and prints a warning.
    -- That flag might get passed as part of "-c opt" builds.
    | "-O" `isPrefixOf` p = False
    | otherwise = True


-- | Parse the module names from the given files.  If a file can't be parsed,
-- print a warning but don't fail outright.
readTargetModules :: [FilePath] -> IO (Set ModuleName)
readTargetModules files = S.fromList . catMaybes <$>
       mapM parseModuleNameFromFileOrWarn files

parseModuleNameFromFileOrWarn :: FilePath -> IO (Maybe ModuleName)
parseModuleNameFromFileOrWarn f = do
    result <- parseModuleNameFromFile f
    case result of
        ParseOk x -> return $ Just x
        ParseFailed loc e -> do
            -- Don't explicitly print the filepath; it's included when =
            -- showing the exception.
            putStrLn $ "WARNING: " ++ prettyPrint loc ++
                        ": Unable to detect module name: " ++ show e
            return Nothing

-- | Symlink all runfiles into a temporary directory, referenced by their
-- short_paths.  (E.g., "bazel-out/k8-fastbuild/foo/bar.txt" will be symlinked
-- to "{tmpdir}/foo/bar.txt".
makeRunfilesDir :: FilePath -> ExecutionRoot -> Map Text Text -> IO ()
makeRunfilesDir dir execRoot files = do
    let prefixDir = Text.pack (dir ++ "/")
    forM_ (M.toList files) $ \(shortPath, fullPath) -> do
        createDirectoryIfMissing True
            $ takeDirectory $ Text.unpack $ prefixDir <> shortPath
        createSymbolicLink (encodeUtf8 $ toAbsoluteT execRoot fullPath)
            (encodeUtf8 $ prefixDir <> shortPath)
