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

-- An alternate version of Chain2 that, instead of exporting its own symbol,
-- is designed to export a symbol that conflicts with Chain1.
module Chain2Conflict (chain1) where

import qualified Chain1 as X

-- Conflicts with a symbol from Chain1.hs
chain1 :: Int
chain1 = 5
