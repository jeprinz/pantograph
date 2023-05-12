module Utility where

import Prelude
import Data.Tuple.Nested
import Bug (bug)

map2 f = map (map f)
infixl 4 map2 as <$$>

map3 f = map (map (map f))
infixl 4 map3 as <$$$>

