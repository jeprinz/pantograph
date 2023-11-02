module Data.MultiMap where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe(..))
import Util as Util
import Data.Foldable
import Data.Traversable (sequence)

type MultiMap k v = Map k (Set v)

insert :: forall k v. Ord k => Ord v => k -> v -> MultiMap k v -> MultiMap k v
insert k v m = case Map.lookup k m of
    Nothing -> Map.insert k (Set.insert v Set.empty) m
    Just s -> Map.insert k (Set.insert v s) m

empty :: forall k v. MultiMap k v
empty = Map.empty

union :: forall k v. Ord k => Ord v => MultiMap k v -> MultiMap k v -> MultiMap k v
union m1 m2 = Util.threeCaseUnion (\s -> s) (\s -> s) Set.union m1 m2

unions :: forall f v k. Ord k => Ord v => Foldable f => f (MultiMap k v) -> MultiMap k v
unions f = foldr union empty f

-- NOTE: there is a quirk here that if you put two of the same value into multimap then it only goes in the set once...

-- returns Just if all things happen to map to exactly one element
toMap :: forall k v. MultiMap k v -> Maybe (Map k v)
toMap mm =
--    all (map ?h mm)
    let mm' = map (\x ->
        let elems :: Array _
            elems = Set.toUnfoldable x in
        assertSingleton elems
        ) mm in
    sequence mm'

assertSingleton :: forall t. Array t -> Maybe t
assertSingleton [x] = Just x
assertSingleton _ = Nothing
