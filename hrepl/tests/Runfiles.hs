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

module Runfiles where

import qualified Bazel.Runfiles as Runfiles

test :: FilePath -> IO ()
test output = do
    rfiles <- Runfiles.create
    mapM_ (readAndWrite rfiles) allData
  where
    allData = [ "hrepl/hrepl/tests/source-data.txt"
              , "hrepl/hrepl/tests/generated-data.txt"
              ]
    readAndWrite rfiles f =
        readFile (Runfiles.rlocation rfiles f)>>= appendFile output
