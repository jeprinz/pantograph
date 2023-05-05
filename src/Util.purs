module Util where
import Data.Tuple.Nested
import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map, toUnfoldable, fromFoldable, lookup, member, delete, unionWith, insert)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Maybe (maybe)
import Data.Either (Either(..))
import Data.Foldable

-- hole :: forall a. a
-- hole = unsafeThrow "hole"

hole' :: forall a. String -> a
hole' label = unsafeThrow $ "hole: " <> label

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' x m = case lookup x m of
  Just v -> v
  Nothing -> unsafeThrow "lookup failed"

head' :: forall a . List a -> a
head' l = case List.head l of
    Nothing -> unsafeThrow "head failed"
    Just a -> a

fromJust :: forall a . Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = unsafeThrow "fromJust failed"

fromJust' :: forall a . String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' msg Nothing = unsafeThrow $ "fromJust failed: " <> msg

fromRight :: forall a b. Either a b -> b
fromRight (Right b) = b
fromRight _ = unsafeThrow "error: fromRight failed"

justWhen :: forall a. Boolean -> (Unit -> a) -> Maybe a
justWhen false _ = Nothing
justWhen true k = Just (k unit)

delete' :: forall v k . Ord k => k -> Map k v -> Map k v
delete' k m  = if member k m then delete k m else unsafeThrow "Tried to delete an element not present in the map"
--delete' k m  = delete k m

insert' :: forall v k . Ord k => k -> v -> Map k v -> Map k v
insert' k v m =
    if member k m then unsafeThrow "Tried to insert an element already present in the map" else
    insert k v m


mapKeys :: forall k v . Ord k => (k -> k) -> Map k v -> Map k v
mapKeys f m =
--    let bla = toUnfoldable in
    let asList :: List (k /\ v)
        asList = toUnfoldable m in
    fromFoldable (map (\(k /\ v) -> f k /\ v) asList)

-- disjoint union
union' :: forall v k. Ord k => Map k v -> Map k v -> Map k v
union' m1 m2 = unionWith (\_ _ -> unsafeThrow "duplicate key in union'") m1 m2

readUUID :: String -> UUID
readUUID str = fromJust' ("failed to parse UUID: " <> str) <<< UUID.parseUUID $ str

threeCaseUnion :: forall v1 v2 v3 k . Ord k =>
    (v1 -> v3) -> (v2 -> v3) -> (v1 -> v2 -> v3)
    -> Map k v1 -> Map k v2 -> Map k v3
threeCaseUnion onlyLeft onlyRight both m1 m2 =
    let mLeft = Map.filterWithKey (\k _ -> not (member k m2)) m1 in
    let mRight = Map.filterWithKey (\k _ -> not (member k m1)) m2 in
    union'
        (union' (map onlyLeft mLeft) (map onlyRight mRight))
        (Map.mapMaybeWithKey (\k v -> maybe Nothing (\v2 -> Just $ both v v2) (Map.lookup k m2)) m1)

threeCaseUnionMaybe :: forall v1 v2 v3 k . Ord k =>
    (Maybe v1 -> Maybe v2 -> Maybe v3)
    -> Map k v1 -> Map k v2 -> Map k v3
threeCaseUnionMaybe join m1 m2 = Map.mapMaybe (\x -> x) $ threeCaseUnion (\x -> join (Just x) Nothing) (\y -> join Nothing (Just y))
    (\x y -> join (Just x) (Just y)) m1 m2


findWithIndex :: forall l t out. Foldable l => (t -> Maybe out) -> l t -> Maybe (out /\ Int)
findWithIndex f l =
    foldl
        (\acc x i ->
            case f x of
                Just y -> Just (y /\ i)
                Nothing -> acc (i + 1))
        (\_ -> Nothing) l 0

assertSingleton :: forall t. Array t -> t
assertSingleton [x] = x
assertSingleton _ = unsafeThrow "assertion failed: was not a singleton"