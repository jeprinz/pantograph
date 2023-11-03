module Data.Isomorphism where

import Prelude

-- | Isomorphism laws:
-- | ```
-- |   to (from a) == a
-- |   from (to a) == a
-- | ```
class Isomorphism a b where
  to :: a -> b
  from :: b -> a
