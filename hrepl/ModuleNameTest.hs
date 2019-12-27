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
module Main (main) where

import Data.ByteString (ByteString)
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Hspec.Expectations (shouldSatisfy)
import Test.HUnit                     ( (@=?) )
import Language.Haskell.Exts.Parser (ParseResult(..), fromParseResult)
import Language.Haskell.Exts.Syntax (ModuleName(..))

import ModuleName

parsesFromFileTo :: FilePath -> ByteString -> String -> IO ()
parsesFromFileTo f contents m =
    ModuleName () m @=? fromParseResult (parseModuleName f contents)

parsesTo :: ByteString -> String -> IO ()
parsesTo = parsesFromFileTo "test.hs"

doesntParse :: ByteString -> IO ()
doesntParse = doesntParseFromFile "test.hs"

doesntParseFromFile :: FilePath -> ByteString -> IO ()
doesntParseFromFile f contents = parseModuleName f contents `shouldSatisfy` isFailure
  where
    isFailure (ParseOk _) = False
    isFailure _ = True

main :: IO ()
main = defaultMain
    [ testGroup "parse" parseTests
    , testGroup "guess" guessTests
    ]

parseTests, guessTests :: [Test]
parseTests =
    [ testCase "no comments" $ "module Foo where" `parsesTo` "Foo"
    , testCase "implicit Main module" $ "foo = 3" `parsesTo` "Main"
    , testCase "Parse failure" $ doesntParse "module () where"
    , testCase "junk" $ "module Foo XXXXX-YYYYYY" `parsesTo` "Foo"
    , testCase "blockCommented" $
          "{- module Foo where -} module Bar" `parsesTo` "Bar"
    , testCase "lineCommented" $
          " -- module Foo where \n module Bar" `parsesTo` "Bar"
    , testCase "nestedComments" $ "{- {- -- x\n -} -} module {- -} Foo "
                                    `parsesTo` "Foo"
    , testCase "hierarchical" $ "module Foo.BaR.Baz" `parsesTo` "Foo.BaR.Baz"
    , testCase "underscores" $ "module Fo_o.B_ar" `parsesTo` "Fo_o.B_ar"
    , testCase "CPP" $ "#ignore me \nmodule\n#ignore me\nFoo"
                        `parsesTo` "Foo"
    , testCase "CPP in comment" $ "{- \n#ignoreme {-\n-} module Foo"
                        `parsesTo` "Foo"
    , testGroup "literate"
        [ testCase "parse" $
              parsesFromFileTo "test.lhs" "ignoreme\n\n>module Bar" "Bar"
        , testCase "failure " $
              doesntParseFromFile "test.lhs" "\\end{code}"
        , testCase "ignore" $
              parsesFromFileTo "test.lhs" "module Foo\n\n>module Bar" "Bar"
        , testCase "hs extension" $
              parsesFromFileTo "test.hs" "module Foo\n\n>module Bar" "Foo"
        ]
    ]

guessTests =
    [ testCase "simple" $ guessModuleName "foo/Bar.hs" @=? "Bar"
    , testCase "hierarchical" $ guessModuleName "foo/Bar/Baz.hs" @=? "Bar.Baz"
    , testCase "intermediate lower-case"
        $ guessModuleName "foo/Bar/bar/Baz.hs" @=? "Baz"
    ]
