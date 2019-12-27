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

-- | A framework to capture build outputs for a collection of labels.
--
-- The functions in this module are not specific to any particular
-- rule set.  See src/RuleInfo.hs for how
-- they are used for the Haskell build rules.
--
-- Example usage:
--
-- perLabelOutput :: BuildOutput ByteString
-- perLabelOutput = outputGroup "some_output_group"
--                     *> outputFileContents (\binDir label -> ...)
--
-- buildData :: [Label] -> IO [(Label, ByteString)]
-- buildData = collectBuildOutputs
--                defBazelOpts
--                (liftA2 (,) outputLabel perLabelOutput)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module BuildOutput
    ( BuildOutput
    , outputGroup
    , defaultOutputGroup
    , outputLabel
    , outputFileContents
    , collectBuildOutputs
    ) where

import qualified Data.ByteString as B
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intercalate)
import qualified Data.Text as Text
import Data.Text (Text)
import Bazel
import Bazel.Name (Label)

-- | An abstract data type representing build process outputs that clients
-- can request.
--
-- Specifies both a set of Bazel output groups to run, and an action to run
-- that uses those build outputs.
--
-- Use the Applicative instance to request multiple outputs.
data BuildOutput a = BuildOutput
  { outputGroups :: Set Text
  , runBuildOutput :: BinDir -> Label -> IO a
  } deriving (Functor)

instance Applicative BuildOutput where
  pure x = BuildOutput Set.empty $ \_ _ -> pure x
  f <*> x = BuildOutput
      { outputGroups = outputGroups f <> outputGroups x
      , runBuildOutput = \b l -> runBuildOutput f b l <*> runBuildOutput x b l
      }

-- | Request an output group to build for each entry.
outputGroup :: Text -> BuildOutput ()
outputGroup o = BuildOutput
  { outputGroups = Set.singleton o
  , runBuildOutput = \_ _ -> pure ()
  }

-- | Request to build the default output group, i.e., the regular
-- file outputs for each entry.
defaultOutputGroup :: BuildOutput ()
defaultOutputGroup = outputGroup "default"

-- | Collect the label for each entry.
outputLabel :: BuildOutput Label
outputLabel = BuildOutput
  { outputGroups = Set.empty
  , runBuildOutput = const return
  }

-- | An action to run for each entry after building it.
outputFileContents :: (BinDir -> Label -> FilePath) -> BuildOutput B.ByteString
outputFileContents path = BuildOutput Set.empty $ \b l -> B.readFile $ path b l

-- | Build a collection of targets, and run the given action for each target.
--
-- Doesn't actually `bazel build` if the set of labels is empty, since that
-- would wipe out any existing files in the execution root (bazel-{workspace}).
-- Additionally, doesn't `bazel build` when no output groups are requested
-- (for example, if the `BuildOutput` is `pure ()`).
collectBuildOutputs
    :: BazelOpts  -- ^ Options to pass to Bazel.
    -> BuildOutput a  -- ^ Outputs to collect for each target.
    -> [Label]
        -- ^ Labels of the targets to build. The order of the output is
        -- guaranteed to match the order of this input.
    -> IO [a]
collectBuildOutputs _ _ [] = return []
collectBuildOutputs opts output labels = do
    binDir <- getBinDir opts
    bazelCmd_ opts "build" $
        [ "--show_result=0" -- Don't print the list of output files.
        , "--output_groups="
            <> intercalate "," (map Text.unpack $ Set.toList
                                    $ outputGroups output)
        ]
        ++ map show (toList labels)
    traverse (runBuildOutput output binDir) labels
