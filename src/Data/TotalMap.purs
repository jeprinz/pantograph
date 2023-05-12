module Data.TotalMap
  ( TotalMap
  , makeTotalMap
  , lookup
  , update
  ) where

import Data.Tuple.Nested ((/\))
import Prelude
import Data.Enum (class Enum, enumFromTo)
import Data.Map as Map
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

newtype TotalMap k v = TotalMap (Map.Map k v)

over :: forall k v a. (Partial => Map.Map k v -> a) -> TotalMap k v -> a
over f m = unsafePartial (over f m)

makeTotalMap :: forall k v. Enum k => Bounded k => (k -> v) -> TotalMap k v
makeTotalMap f = TotalMap $ Map.fromFoldable $ ((enumFromTo bottom top <#> \k -> k /\ f k) :: Array _)

lookup :: forall k v. Ord k => k -> TotalMap k v -> v
lookup k = over $ fromJust <<< Map.lookup k

update :: forall k v. Ord k => k -> (v -> v) -> TotalMap k v -> TotalMap k v
update k f = over $ Map.alter (fromJust >>> f >>> pure) k >>> TotalMap

instance (Ord k, Semigroup v) => Semigroup (TotalMap k v) where
  append (TotalMap m1) (TotalMap m2) = TotalMap (Map.unionWith append m1 m2)

instance (Enum k, Bounded k, Monoid v) => Monoid (TotalMap k v) where
  mempty = makeTotalMap \_ -> mempty
