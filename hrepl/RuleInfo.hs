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

-- | Collecting metadata from Haskell build rule output groups.
--
-- This module uses 'BuildOutput' in a way specific to our Haskell build rules.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleInfo (
    -- * Building and reading GhcConfig
    GhcConfig,
    getGhcConfig,
    -- * PackageSet
    PackageSet(..),
    packageSetOutput,
    immediatePackageSetFlags,
    transitivePackageSetFlags,
    -- * BuildOptions
    BuildOptions(..),
    buildOptionsOutput,
    -- * Raw access to the CompileInfo
    -- Note that depending on which output groups you specify, not all fields of
    -- the CompileInfo may be useful (e.g., point to usable files).
    -- TODO(b/37618590): These exports are currently used by ghaddock.
    -- Remove them once we've change ghaddock to use Starlark to build
    -- individual targets.
    CompileInfo,
    compileInfo,
    sourceFilesGroup,
    transitiveDepsGroup,
    -- * Dependencies
    buildDependentPackages,
    ) where

import Proto.RuleInfo
    ( CompileInfo
    , LibraryInfo
    , GhcConfig
    )
import qualified Data.ByteString as B
import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import Data.ProtoLens (decodeMessageOrDie)
import Data.ProtoLens.Labels ()
import qualified Data.Text as Text
import Lens.Micro ((^.), (^..), to, _1)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>), joinPath)
import Bazel.Name
    ( PackageName(..)
    , TargetName(..)
    , Workspace(..)
    , Label(..)
    )
import Bazel
import qualified Bazel.Query as Q
import BuildOutput
    ( BuildOutput
    , collectBuildOutputs
    , outputGroup
    , outputFileContents
    , outputLabel
    )
import RuleInfo.ExecutionRoot (ExecutionRoot, toAbsolute)

-- | A file containing a CompileInfo proto message.
compileInfo :: BuildOutput CompileInfo
compileInfo = outputGroup "haskell_compile_info"
                *> fmap decodeMessageOrDie (outputFileContents compileInfoPath)
  where
    compileInfoPath (BinDir binDir) label =
        binDir </> targetPath label <.> "HaskellCompile.pb"

-- | A file containing a LibraryInfo proto message.
libraryInfo :: BuildOutput LibraryInfo
libraryInfo = outputGroup "haskell_library_info"
                *> fmap decodeMessageOrDie (outputFileContents compileInfoPath)
  where
    compileInfoPath (BinDir binDir) label =
        binDir </> targetPath label <.> "HaskellLibrary.pb"

-- | Output groups as defined by the Haskell build rules.
cdepsSharedLibGroup, runfilesGroup, sourceFilesGroup, transitiveDepsGroup
    :: BuildOutput ()
cdepsSharedLibGroup = outputGroup "haskell_cdeps_shared_lib"
runfilesGroup = outputGroup "haskell_runfiles"
sourceFilesGroup = outputGroup "haskell_source_files"
transitiveDepsGroup = outputGroup "haskell_transitive_deps"

targetPath :: Label -> String
targetPath (Label w (PackageName comps) n)
    = workspacePath </> joinPath (map Text.unpack comps) </> Text.unpack (unTargetName n)
  where
    -- TODO: is this logic correct?
    workspacePath = case w of
        CurrentWorkspace -> ""
        Workspace t -> "external" </> Text.unpack t

-- | Get the GHC configuration for the current working directory.
--
-- Also implicitly ensures that GHC will be staged in the execution root.
getGhcConfig :: BazelOpts -> IO GhcConfig
getGhcConfig opts = do
    bazelCmd_ opts "build"
        [ "@rules_haskell//haskell:toolchain_info"
        , "--show_result=0" -- Don't print the list of output files.
        ]
    BinDir binDir <- getBinDir opts
    pbFile <- findToolchainPb binDir
    decodeMessageOrDie <$> B.readFile pbFile
  where
    -- Usually the outputs of @rules_haskell//... live
    -- in bazel-bin/external/rules_haskell/....  However, when we're building
    -- in the rules_haskell repository itself, they're just in bazel-bin/...
    findToolchainPb binDir = do
    let externalPb =
            binDir </> "external/rules_haskell/haskell/toolchain_info.pb"
        internalPb = binDir </> "haskell/toolchain_info.pb"
    externalExists <- doesFileExist externalPb
    if externalExists
        then return externalPb
        else do
            internalExists <- doesFileExist internalPb
            if internalExists
                then return internalPb
                else error "Unable to find the toolchain info file."

-- | Information about a set of compiled libraries.
data PackageSet = PackageSet
    { immediatePackageIds :: Set Text
        -- ^ The package IDs of the immediate dependencies.
        -- Includes these libraries, but not any of their dependencies.
        -- Can be used to expose immediate (but not transitive) dependencies,
        -- similar to the behavior of the Bazel build rules.
    , transitivePackageIds :: Set Text
        -- ^ The IDs of all transitive dependencies.
        -- For user convenience, in hrepl we allow import any transitive
        -- dependency. If needed, "-XPackageImports" can disambiguate
        -- between multiple deps of the same name.
    , transitivePackageDbs :: Set Text
        -- ^ What package DBs (transitively) are being used, and need to
        -- be compiled ahead of time (--package-db)
    }

packageSetOutput :: BuildOutput PackageSet
packageSetOutput = transitiveDepsGroup *> libraryInfo <&> \c -> PackageSet
  { immediatePackageIds = S.singleton $ c ^. #packageId
  , transitivePackageIds = S.fromList $ c ^. #transitivePackageIds
  , transitivePackageDbs = S.fromList $ c ^. #transitivePackageDbs
  }

instance Semigroup PackageSet where
    setA <> setB = PackageSet
        { immediatePackageIds = (mappend `on` immediatePackageIds) setA setB
        , transitivePackageIds = (mappend `on` transitivePackageIds) setA setB
        , transitivePackageDbs = (mappend `on` transitivePackageDbs) setA setB
        }

instance Monoid PackageSet where
    mempty = PackageSet mempty mempty mempty

-- | The command-line flags to expose the immediate dependencies.
-- This is how the Bazel build rules behave.
immediatePackageSetFlags :: ExecutionRoot -> PackageSet -> [String]
immediatePackageSetFlags execRoot d =
    [ "-package-db=" ++ toAbsolute execRoot (Text.unpack t)
    | t <- S.toList (transitivePackageDbs d)]
    ++ [ "-package-id=" ++ Text.unpack p | p <- S.toList (immediatePackageIds d)]

-- | The command-line flags to expose all transitive dependencies.
transitivePackageSetFlags :: ExecutionRoot -> PackageSet -> [String]
transitivePackageSetFlags execRoot d =
    [ "-package-db=" ++ toAbsolute execRoot (Text.unpack t)
    | t <- S.toList (transitivePackageDbs d)]
    ++ [ "-package-id=" ++ Text.unpack p
       | p <- S.toList (transitivePackageIds d)
       ]

-- | Options generated by the build process for interpreting and running
-- source code.
data BuildOptions = BuildOptions
  { ghcOptions :: [Text]
  -- ^ GHC command-line options
  , sourceFiles :: Map Label (Set Text)
  -- ^ Haskell source files to load (.hs, .lhs)
  , runfiles :: Map Text Text
  -- ^ Map from runfile short path to the full path under bazel-{workspace}
  , transitiveCcSharedLibs :: Set Text
  -- ^ Transitive shared libraries for cc_library dependencies
  } deriving Show

instance Semigroup BuildOptions where
  (<>) = mappend

instance Monoid BuildOptions where
  mempty = BuildOptions mempty mempty mempty mempty
  optsA `mappend` optsB = BuildOptions
    { ghcOptions  = (mappend `on` ghcOptions)  optsA optsB
    , sourceFiles = (mappend `on` sourceFiles) optsA optsB
    , runfiles = (mappend `on` runfiles) optsA optsB
    , transitiveCcSharedLibs = (mappend `on` transitiveCcSharedLibs) optsA optsB
    }

buildOptionsOutput :: BuildOutput BuildOptions
buildOptionsOutput = sourceFilesGroup *> cdepsSharedLibGroup
                *> runfilesGroup *> buildOutputs <&> \c ->
    BuildOptions
      { ghcOptions = c ^. _1 . #options
      , sourceFiles = uncurry M.singleton $ mk c
      , runfiles = M.fromList $ c ^.. _1 . #runfiles . traverse
                                . to (\r -> (r ^. #shortPath, r ^. #fullPath))
      , transitiveCcSharedLibs = S.fromList $ c ^. _1 . #transitiveCcSharedLibs
      }
  where
    mk (comp, label') = (label', S.fromList $ comp ^. #sourceFiles)
    buildOutputs :: BuildOutput (CompileInfo, Label)
    buildOutputs = (,) <$> compileInfo <*> outputLabel

-- | Compiles the Haskell packages that the given targets depend on.
-- Does not actually compile any of the input targets themselves, since we're
-- loading them directly into the interpreter.
buildDependentPackages ::
  BazelOpts ->
  -- | Targets to compile and expose
  [Label] ->
  -- | Targets to interpret
  [Label] ->
  IO PackageSet
buildDependentPackages bazelOpts compiledDeps targets = do
  let allCompiled = Q.union . map Q.labelToQuery $ compiledDeps
      allTargets = Q.union . map Q.labelToQuery $ targets
      -- NOTE: Quotes are important.
      haskellRules =
        "'haskell_library|haskell_proto_library|haskell_toolchain_library|haskell_cabal_library'"
  -- The haskell_library and haskell_proto_library rules that these targets
  -- depend on, excluding any of the targets themselves (since they'll be
  -- loaded into the interpreter).
  -- Ignores binaries/tests from the compiled targets, which don't have
  -- corresponding package DBs to load.
  immediateHaskellLibraryDeps <-
    bazelQuery bazelOpts
      $ Q.letIn ("ts", allTargets)
      $ Q.kind haskellRules (Q.depsUpto (Q.var "ts") 1 Q.<+> allCompiled)
        Q.<-> Q.var "ts"
  immediateProtoLibraryDeps <-
    bazelQuery bazelOpts
      $ Q.letIn ("ts", allTargets)
      $ Q.kind
        "proto_library"
        ( Q.depsUpto
            ( Q.kind
                "haskell_proto_library"
                (Q.var "ts")
            )
            1
        )
        Q.<-> Q.var "ts"
  libs <- compile bazelOpts immediateHaskellLibraryDeps
  let aspect =
        [ "--aspects",
          "@rules_haskell//haskell:protobuf.bzl%_haskell_proto_aspect"
        ]
  protos <-
    compile
      (bazelOpts {bazelPost = bazelPost bazelOpts ++ aspect})
      immediateProtoLibraryDeps
  return $ libs <> protos
  where
    compile opts labels =
      mconcat
        <$> collectBuildOutputs opts packageSetOutput labels
