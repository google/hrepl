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
-- | This binary serves as a template for our tests of hrepl.
-- For more details, see test.bzl.
module Main (main) where

import Control.Applicative (many)
import qualified Data.Text.Lazy as T
import ReplTestLib
import Test.Framework
import Test.Framework.Providers.HUnit
import Options.Applicative
import System.IO

data TestArgs = TestArgs
  { taScript :: TestScript
  , taExpected :: String  -- ^ Expected output
  }

parseTestArgs :: Parser TestArgs
parseTestArgs = TestArgs
    <$> (TestScript
          <$> many (strOption $ long "arg")
          <*> fmap (flip interpolate)
              (T.pack <$>
                  strOption (long "script" <> help "Commands to run in hrepl.")))
    <*> (strOption $ long "expected" <> help "Expected output.")

main :: IO ()
main = do
    -- Improve output latency when run from within `bazel test`:
    hSetBuffering stdout LineBuffering
    testArgs <- execParser $ info (helper <*> parseTestArgs) mempty
    flip defaultMainWithArgs []
        [ testCase "hrepl_test"
                $ hreplTest (taScript testArgs) (taExpected testArgs)
        ]

-- | Splices in the input file as a quoted Haskell string expression.
-- Replaces the template "{OUT}".
interpolate :: FilePath -> T.Text -> T.Text
interpolate f = T.replace "{OUT}" (T.pack $ show f)
