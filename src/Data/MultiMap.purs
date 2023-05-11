module Data.MultiMap where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe(..))

type MultiMap k v = Map k (Set v)

insert :: forall k v. Ord k => Ord v => k -> v -> MultiMap k v -> MultiMap k v
insert k v m = case Map.lookup k m of
    Nothing -> Map.insert k (Set.insert v Set.empty) m
    Just s -> Map.insert k (Set.insert v s) m

empty :: forall k v. MultiMap k v
empty = Map.empty

union :: forall k v. Ord k => Ord v => MultiMap k v -> MultiMap k v -> MultiMap k v
union m1 m2 = Set.union <$> m1 <*> m2