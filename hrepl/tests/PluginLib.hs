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

-- | Test that we can load and use GHC plugins.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module PluginLib (two) where

import Data.Proxy
import GHC.TypeLits

-- | Requires ghc-typelits-knownnat to solve its constraints:
plusOne :: forall n . KnownNat n => Proxy n -> Integer
plusOne _ = natVal (Proxy :: Proxy (n + 1))

two :: Integer
two = plusOne (Proxy :: Proxy 1)
