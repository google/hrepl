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

-- | A module for parsing the module name out of a Haskell source file.
module ModuleName
    ( guessModuleName
    , parseModuleNameFromFile
    , parseModuleName
    , ParseResult(..)
    , ModuleName
    , prettyPrint
    ) where

import qualified Data.ByteString as B
import Data.Char (isUpper)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as Syntax
import Distribution.Simple.PreProcess.Unlit (unlit)
import System.FilePath (dropExtension, splitDirectories, takeExtension)

-- haskell-src-exts tags every node in the ADT with a source location.
-- We wipe it out and replace it with (), since we don't care about it and
-- instead want module names in different files to look identical.
type ModuleName = Syntax.ModuleName ()

parseModuleNameFromFile :: FilePath -> IO (ParseResult ModuleName)
parseModuleNameFromFile f =
    parseModuleName f <$> B.readFile f


parseModuleName :: FilePath -> B.ByteString -> ParseResult ModuleName
parseModuleName f content = do
    unlitContent <- maybeUnlit f $ T.decodeUtf8 content
    getModuleName <$>
        (parseWithMode (defaultParseMode {parseFilename = f})
               . T.unpack
               -- Hack: mimic CPP by dropping all files starting with "#".
               . T.unlines . filter (not . isCPPDirective) . T.lines
               $ unlitContent)
  where
    getModuleName (NonGreedy (PragmasAndModuleName (_ :: SrcSpanInfo) _ m))
        = maybe (Syntax.ModuleName () "Main") (() <$) m
    isCPPDirective = T.isPrefixOf (T.pack "#")

maybeUnlit :: FilePath -> T.Text -> ParseResult T.Text
maybeUnlit f content
    | takeExtension f == ".lhs"
        -- Unlike most Haskell code, 'unlit' returns 'Left' in case of
        -- success, and 'Right' in case of failure.  *shrug*
        = either (return . T.pack) fail
              $ unlit f $ T.unpack content
    -- In the common case, don't round-trip through String:
    | otherwise = return content

-- | Guesses the Haskell module name for a given file, using the same logic
-- as enforced in the build rules for library targets (but not yet binaries or
-- tests).
--
-- Follows the longest-capitalized-suffix rule from go/haskell-rules#module-names,
-- as implemented in our build rules.
guessModuleName :: FilePath -> String
guessModuleName =
    intercalate "."
    . reverse
    . takeWhile capitalized
    . reverse
    . splitDirectories
    . dropExtension
  where
    capitalized (c:_) = isUpper c
    capitalized _ = False

