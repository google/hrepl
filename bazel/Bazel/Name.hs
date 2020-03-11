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
-- | Utilities related to the names of Bazel packages and targets.
-- See https://docs.bazel.build/versions/master/build-ref.html#labels

module Bazel.Name where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

-- | A package name such as //foo/bar, represented as a list of
--   path segments.
newtype PackageName = PackageName [Text]
  deriving (Eq, Ord)
instance Show PackageName where
  show = ("//" ++) . relativePath

-- | An unqualified target name.
newtype TargetName = TargetName { unTargetName :: Text }
  deriving (Eq, Ord)
instance Show TargetName where
  show = Text.unpack . unTargetName

data Workspace = CurrentWorkspace | Workspace Text
  deriving (Show, Eq, Ord)

workspaceToString :: Workspace -> String
workspaceToString CurrentWorkspace = ""
workspaceToString (Workspace t) = "@" ++ Text.unpack t

-- | The fully qualified label of a target: its name qualifed by its package.
data Label = Label Workspace PackageName TargetName
  deriving (Eq, Ord)
instance Show Label where
  show (Label w pn tn) = workspaceToString w ++ show pn ++ ":" ++ show tn

-- > parsePackageName "//foo/bar" == PackageName ["foo", "bar"]
parsePackageName :: Text -> PackageName
parsePackageName = PackageName . dropWhile Text.null . Text.splitOn "/"

-- | Note that this expects a fully qualified label, so things like
--   ":target_name" or "target_name" won't work.
-- > parseLabel "//foo/bar:baz" == Label (PackageName ["foo", "bar"])
-- >                                     (TargetName "baz")
parseLabel :: Text -> Label
parseLabel l
    | Text.null y = error $ "parseLabel: no name part: " ++ show l
    | otherwise = Label w' (parsePackageName x) (TargetName $ Text.tail y)
  where
    (w, pn) = Text.breakOn "//" l
    (x, y) = Text.break (== ':') pn
    w'
        | Text.null w = CurrentWorkspace
        | Text.head w /= '@' = error $ "parseLabel: malformed workspace part: " ++ show l
        | otherwise = Workspace $ Text.tail w  -- drop the final @

-- | Glorified list concatenation.
-- > subPackageName (PackageName "foo") ["bar", "baz"] ==
-- >     PackageName ["foo", "bar", "baz"]
subpackageName :: PackageName -> [Text] -> PackageName
subpackageName (PackageName segs) moreSegs = PackageName (segs ++ moreSegs)

-- | Package //foo/bar is found at foo/bar relative to the root.
relativePath :: PackageName -> FilePath
relativePath (PackageName p) = foldr (</>) "" . fmap Text.unpack $ p

-- | Absolute path for the given package in the @rootDir@.
absolutePath :: FilePath -> PackageName -> FilePath
absolutePath rootDir (PackageName p) =
    rootDir </> foldr (</>) "" (Text.unpack <$> p)

-- | Package directories' names must only contain alphanumeric
-- characters, dashes, underscores and forward slashes.
isValidPackageDirectoryName :: Text -> Bool
isValidPackageDirectoryName = Text.all isValidChar where
  isValidChar a = isAlphaNum a || a `elem` ("-_/" :: String)
