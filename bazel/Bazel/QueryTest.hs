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

module Main
  ( main,
  )
where

import Bazel.Query
import Bazel.Name (parseLabel)
import GHC.Stack
import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=), (@?=), assertFailure)

assertRender :: HasCallStack => Query -> String -> IO ()
assertRender q s = case nonEmptyQuery q of
  Nothing -> assertFailure $ "Query unexpectedly rendered as empty: " ++ show q
  Just q' -> renderQueryStr q' @?= s

assertEmpty :: HasCallStack => Query -> IO ()
assertEmpty q = case nonEmptyQuery q of
  Nothing -> return ()
  Just q' -> assertFailure $ "Expected query to be empty; got: "
                                ++ renderQueryStr q'

main :: IO ()
main =
  defaultMain
    [ testCase "word" $ do
        let q = word "foo"
        assertRender q "foo",
      testCase "string literal" $
        assertRender "foo" "foo",
      testCase "fun" $ do
        let q = fun "foo" [intArg 42, "some_arg"]
        assertRender q "foo(42, some_arg)",
      testCase "allpaths" $ do
        let q = allpaths "//foo/bar" "//hee/haw"
        assertRender q "allpaths(//foo/bar, //hee/haw)",
      testCase "attr" $ do
        let q = attr "srcs" "//package/file" "//package:*"
        assertRender q "attr(srcs, //package/file, //package:*)",
      testCase "deps" $ do
        let q = deps "//foo"
        assertRender q "deps(//foo)",
      testCase "depsUpto" $ do
        let q = depsUpto "//foo" 3
        assertRender q "deps(//foo, 3)",
      testCase "filter" $ do
        let q = filterQ "\\.cc$" (deps "//foo")
        assertRender q "filter(\\.cc$, deps(//foo))",
      testCase "kind" $ do
        let q = kind "haskell_" "//haskell/tools/..."
        assertRender q "kind(haskell_, //haskell/tools/...)",
      testCase "somepaths" $ do
        let q = somepaths "//foo/bar" "//hee/haw"
        assertRender q "somepaths(//foo/bar, //hee/haw)",
      testCase "queryArg" $ do
        let q1 = fun "attr" [intArg 42, "some_arg"]
            q2 = strFun "kind" ["haskell_", queryArg q1]
        assertRender q2 "kind(haskell_, attr(42, some_arg))",
      testCase "combine" $ do
        let q1 = "//some/path/..."
            q2 = "//some/path/internal/..."
        assertRender (q1 <^> q2)
          "(//some/path/... ^ //some/path/internal/...)"
        assertRender (q1 <+> q2)
          "(//some/path/... + //some/path/internal/...)"
        assertRender (q1 <-> q2)
          "(//some/path/... - //some/path/internal/...)",
      testCase "let-in" $ do
        let q =
              letIn ("v", "foo/...") $
                allpaths (var "v") "//common" <^> var "v"
        assertRender q
          "let v = foo/... in (allpaths($v, //common) ^ $v)",
      testCase "intersection" $ do
        assertEmpty (intersection [])
        assertRender (intersection [word "x", word "y", word "z"])
            "(y ^ (z ^ x))",
      testCase "union" $ do
        assertEmpty (union [])
        assertRender (union [word "x", word "y", word "z"])
            "(y + (z + x))",
      testCase "labelToQuery" $ do
        let label = parseLabel "//some/path:target"
        (assertRender . labelToQuery) label "//some/path:target",
      emptyQueryTests
    ]

emptyQueryTests :: Test
emptyQueryTests = testGroup "empty"
  [ testCase "union" $ do
      assertEmpty (empty <+> empty)
      assertRender (empty <+> word "x") "x"
      assertRender ("x" <+> empty) "x"
      assertRender (("x" <+> empty) <+> "y") "(x + y)"
      assertRender ("x" <+> (empty <+> "y")) "(x + y)",
    testCase "intersect" $ do
      assertEmpty (empty <^> empty)
      assertEmpty (empty <^> "x")
      assertEmpty ("x" <^> empty)
      assertEmpty (("x" <^> empty) <^> "y")
      assertEmpty ("x" <^> (empty <^> "y")),
    testCase "except" $ do
      assertEmpty (empty <-> empty)
      assertEmpty (empty <-> word "x")
      assertRender ("x" <-> empty) "x"
      assertRender (("x" <-> empty) <-> "y") "(x - y)"
      assertRender ("x" <-> ("y" <-> empty)) "(x - y)",
    testCase "fun" $
      assertEmpty $ fun "foo" [intArg 42, queryArg empty],
    testCase "bind" $ do
      assertEmpty $ letIn ("v", empty) (var "v")
      assertRender (letIn ("v", empty) $ letIn ("w", "x") (var "w"))
        "let w = x in $w"
      assertRender (letIn ("v", empty) $ letIn ("w", "x")
                        (var "w" <+> var "v"))
        "let w = x in $w"
      assertEmpty $ letIn ("v", "x") empty
      assertEmpty $ letIn ("v", "x" <^> empty) (var "v")
  ]
