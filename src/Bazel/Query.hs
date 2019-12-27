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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Utilities for building bazel queries.
module Bazel.Query
  ( (<^>),
    (<+>),
    (<->),
    Arg,
    Query,
    NonEmptyQuery,
    nonEmptyQuery,
    allpaths,
    attr,
    deps,
    depsUpto,
    empty,
    filterQ,
    fun,
    intArg,
    intersection,
    kind,
    labelToQuery,
    letIn,
    queryArg,
    renderQuery,
    renderQueryStr,
    somepaths,
    strArg,
    strFun,
    strWord,
    txtArg,
    union,
    var,
    word,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard, mzero)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.List (intersperse)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TL
import Bazel.Name (Label)

data QueryWay = NonEmpty | MaybeEmpty

-- | Represents bazel query expressions.
-- See https://docs.bazel.build/versions/master/query.html.
--
-- Queries are parameterized by a "way", distinguishing whether they may
-- be empty or not.  Bazel's query language doesn't have a good way to represent
-- "empty" queries; however they can be useful for combinators like 'intersection'
-- and 'union' . This particular type allows (sub)queries to be empty. You can
-- use 'nonEmptyQuery' to simplify and filter out any empty subqueries.
type Query = QueryM 'MaybeEmpty
deriving instance Eq Query
deriving instance Show Query

type NonEmptyQuery = QueryM 'NonEmpty

data QueryM (w :: QueryWay) where
  Empty :: QueryM 'MaybeEmpty
    -- ^ A "null" query which is known to evaluate to the empty set.
    -- Bazel's query language doesn't have a good way of representing this,
    -- so we track it explicitly and filter it out with 'nonEmptyQuery'.
  Word :: Text -> QueryM w
  Fun :: Text -> [ArgM w] -> QueryM w
  Intersect :: QueryM w -> QueryM w -> QueryM w
  Union :: QueryM w -> QueryM w -> QueryM w
  Except :: QueryM w -> QueryM w -> QueryM w
  Var :: Text -> QueryM w
  LetIn :: BindingM w -> QueryM w -> QueryM w

instance IsString Query where
  fromString = strWord

-- | Represents arguments to function expressions.
-- Int, word, or other expressions.
type Arg = ArgM 'MaybeEmpty
deriving instance Eq Arg
deriving instance Show Arg

data ArgM w
  = ArgInt Int
  | ArgWord Text
  | ArgQuery (QueryM w)

instance IsString Arg where
  fromString = strArg

data BindingM w = Binding Text (QueryM w)

type Binding = BindingM 'MaybeEmpty
deriving instance Eq Binding
deriving instance Show Binding

-- | Constructs a word expression.
word :: Text -> Query
word = Word

-- | Constructs a word expression.
strWord :: String -> Query
strWord = word . T.pack

-- | Constructs a function expression.
fun :: Text -> [Arg] -> Query
fun = Fun

-- | Constructs an empty query.
empty :: Query
empty = Empty

------------------------------------------------
-- | Simplifies this query by removing all empty subqueries from this query.
-- Returns 'Nothing' if the entire query itself evaluates to empty.
--
-- For example:
--
-- > nonEmptyQuery empty == Nothing
-- > nonEmptyQuery ("x" <+> empty) == Just "x"
-- > nonEmptyQuery ("x" <^> empty) == Nothing
nonEmptyQuery :: Query -> Maybe NonEmptyQuery
nonEmptyQuery = runIdentity . runMaybeT . flip runReaderT Set.empty . nonEmpty'
  where
    nonEmpty' :: Query -> Render NonEmptyQuery
    nonEmpty' (Word w) = pure (Word w)
    nonEmpty' Empty = mzero
    -- An intersection requires both inputs to be nonempty.
    nonEmpty' (Intersect q1 q2) = Intersect <$> nonEmpty' q1 <*> nonEmpty' q2
    nonEmpty' (Union q1 q2) =
        -- A union requires at least one input to be nonempty.
        (do
            q1' <- nonEmpty' q1
            fmap (Union q1') (nonEmpty' q2)  -- Both q1 and q2 are nonempty
                <|> pure q1') -- Only q1 is nonempty
        <|> nonEmpty' q2  -- Only q2 is nonempty
    nonEmpty' (Except q1 q2) = do
        -- "q1 - q2" is empty whenever "q1" is empty.
        q1' <- nonEmpty' q1
        (Except q1' <$> nonEmpty' q2)
            <|> pure q1' -- q2 is empty
    nonEmpty' (Var t) = do
        -- A variable evaluates to empty if it was previously bound to an empty
        -- query.
        asks (Set.notMember t) >>= guard
        pure $ Var t
    nonEmpty' (LetIn (Binding t q) e) =
        -- If the bound query is nonempty, then just evaluate the expression.
        (do
            q' <- nonEmpty' q
            LetIn (Binding t q') <$> nonEmpty' e)
        -- If the bound query is empty, then record then binding variable.
        -- We'll remove all instances of it from 'e'.
        <|> local (Set.insert t) (nonEmpty' e)
    -- A function evaluates to empty if any arguments are empty.
    nonEmpty' (Fun t args) = Fun t <$> mapM nonEmptyArg args

    nonEmptyArg (ArgInt n) = pure $ ArgInt n
    nonEmptyArg (ArgWord t) = pure $ ArgWord t
    nonEmptyArg (ArgQuery q) = ArgQuery <$> nonEmpty' q

-- | Keeps track of the set of variables that were bound to empty queries.
-- Returns Nothing if the thing being evaluated is empty.
type Render = ReaderT (Set.Set Text) (MaybeT Identity)

----------------------------------------------------------------------
-- Shortcuts for constructing various function expressions. Add more as needed.

-- | Constructs an "allpaths" function expression.
allpaths :: Query -> Query -> Query
allpaths start end = fun "allpaths" [queryArg start, queryArg end]

-- | Constructs an "attr" function expression.
attr :: Text -> Text -> Query -> Query
attr name ptrn input = fun "attr" [txtArg name, txtArg ptrn, queryArg input]

-- | Constructs a "deps(x)" function expression.
deps :: Query -> Query
deps x = fun "deps" [queryArg x]

-- | Constructs a "deps(x, depth)" function expression.
depsUpto :: Query -> Int -> Query
depsUpto x depth = fun "deps" [queryArg x, intArg depth]

-- | Constructs a "filter" function expression.
filterQ :: Text -> Query -> Query
filterQ ptrn input = fun "filter" [txtArg ptrn, queryArg input]

-- | Constructs a "kind" function expression.
kind :: Text -> Query -> Query
kind ptrn input = fun "kind" [txtArg ptrn, queryArg input]

-- | Constructs an "somepaths" function expression.
somepaths :: Query -> Query -> Query
somepaths start end = fun "somepaths" [queryArg start, queryArg end]

-- END shortcuts
----------------------------------------------------------------------

-- | Constructs a function expression.
strFun :: String -> [Arg] -> Query
strFun = fun . T.pack

-- | Constructs a variable expression.
-- This should be used within the query argument to the 'letIn' function.
-- A '$' sign will be prepended when this is rendered.
-- See https://docs.bazel.build/versions/master/query.html#variables.
var :: Text -> Query
var = Var

-- | Constructs a let-in expression.
letIn :: (Text, Query) -> Query -> Query
letIn (varName, q) = LetIn (Binding varName q)

-- | Intersection of two expressions.
(<^>) :: Query -> Query -> Query
(<^>) = Intersect

-- | Union of two expressions.
(<+>) :: Query -> Query -> Query
(<+>) = Union

-- | Difference of two expressions.
(<->) :: Query -> Query -> Query
(<->) = Except

-- | Intersection of multiple expressions.
intersection :: [Query] -> Query
intersection [] = Empty
intersection (q:qs) = foldr (<^>) q qs

-- | Unions of multiple expressions.
union :: [Query] -> Query
union [] = Empty
union (q:qs) = foldr (<+>) q qs

-- | Constructs an int argument.
intArg :: Int -> Arg
intArg = ArgInt

-- | Constructs a word argument.
txtArg :: Text -> Arg
txtArg = ArgWord

-- | Constructs a word argument.
strArg :: String -> Arg
strArg = ArgWord . T.pack

-- | Constructs an expression argument.
queryArg :: Query -> Arg
queryArg = ArgQuery

-- | Converts a Label to a Query.
labelToQuery :: Label -> Query
labelToQuery = strWord . show

-- | Renders a bazel query expression to a text.
renderQuery :: NonEmptyQuery -> Text
renderQuery = toStrict . TL.toLazyText . renderQuery'

renderQuery' :: NonEmptyQuery -> TL.Builder
renderQuery' (Word w) = TL.fromText w
renderQuery' (Intersect q1 q2) =
  mconcat
    [ TL.singleton '(',
      renderQuery' q1,
      TL.fromText " ^ ",
      renderQuery' q2,
      TL.singleton ')'
    ]
renderQuery' (Union q1 q2) =
  mconcat
    [ TL.singleton '(',
      renderQuery' q1,
      TL.fromText " + ",
      renderQuery' q2,
      TL.singleton ')'
    ]
renderQuery' (Except q1 q2) =
  mconcat
    [ TL.singleton '(',
      renderQuery' q1,
      TL.fromText " - ",
      renderQuery' q2,
      TL.singleton ')'
    ]
renderQuery' (Fun w args) =
  let args' = mconcat $ intersperse (TL.fromText ", ") (renderArg <$> args)
   in mconcat [TL.fromText w, TL.singleton '(', args', TL.singleton ')']
renderQuery' (Var varName) = TL.singleton '$' <> TL.fromText varName
renderQuery' (LetIn (Binding varName q1) q2) =
  mconcat
    [ TL.fromText "let ",
      TL.fromText varName,
      TL.fromText " = ",
      renderQuery' q1,
      TL.fromText " in ",
      renderQuery' q2
    ]

-- | Renders a bazel query expression to a string.
renderQueryStr :: NonEmptyQuery -> String
renderQueryStr = T.unpack . renderQuery

renderArg :: ArgM 'NonEmpty -> TL.Builder
renderArg (ArgInt n) = TL.fromString . show $ n
renderArg (ArgWord w) = TL.fromText w
renderArg (ArgQuery q) = renderQuery' q
