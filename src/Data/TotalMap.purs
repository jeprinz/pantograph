module Data.TotalMap
  ( TotalMap
  , makeTotalMap
  , lookup
  , update
  , mapWithKey
  , hasKey
  , fromMap
  ) where

import Prelude

import Bug.Assertion (Assertion(..), assert)
import Control.Monad.Error.Class (throwError)
import Data.Enum (class Enum, enumFromTo)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype as Newtype
import Data.Traversable (class Traversable)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Text.Pretty (quotes)

newtype TotalMap k v = TotalMap (Map.Map k v)

derive instance Functor (TotalMap k)
derive instance Foldable (TotalMap k)
derive instance Traversable (TotalMap k)

over :: forall k v a. (Partial => Map.Map k v -> a) -> TotalMap k v -> a
over = \f (TotalMap m) -> unsafePartial $ f m

makeTotalMap :: forall k v. Enum k => Bounded k => (k -> v) -> TotalMap k v
makeTotalMap f = TotalMap $ Map.fromFoldable $ ((enumFromTo bottom top <#> \k -> k /\ f k) :: Array _)

hasKey :: forall k v. Ord k => Show k => String -> k -> Map.Map k v -> Assertion v
hasKey source k m = Assertion
  { name: "hasKey"
  , source
  , result: case Map.lookup k m of
      Nothing -> throwError $ "Could not find key " <> quotes (show k)
      Just a -> pure a
  }

fromMap :: forall k v. Show k => Enum k => Bounded k => Map.Map k v -> TotalMap k v
fromMap m = makeTotalMap \k -> assert (hasKey "TotalMap.fromMap" k m) identity

lookup :: forall k v. Ord k => k -> TotalMap k v -> v
lookup k = over $ fromJust <<< Map.lookup k

update :: forall k v. Ord k => k -> (v -> v) -> TotalMap k v -> TotalMap k v
update k f = over $ Map.alter (fromJust >>> f >>> pure) k >>> TotalMap

instance (Ord k, Semigroup v) => Semigroup (TotalMap k v) where
  append (TotalMap m1) (TotalMap m2) = TotalMap (Map.unionWith append m1 m2)

instance (Enum k, Bounded k, Monoid v) => Monoid (TotalMap k v) where
  mempty = makeTotalMap (const mempty)

mapWithKey :: forall k v. Ord k => (k -> v -> v) -> TotalMap k v -> TotalMap k v
mapWithKey f = over $ Map.mapMaybeWithKey (\k v -> Just (f k v)) >>> TotalMap
