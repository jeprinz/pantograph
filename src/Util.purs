module Util where

import Data.Foldable
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Bug as Bug
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map, toUnfoldable, fromFoldable, lookup, member, delete, unionWith, insert)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.UUID (UUID)
import Data.UUID as UUID
import Hole as Hole

unwrapApply nt f = f (unwrap nt)
infixl 1 unwrapApply as >.

mapMap f a = map (map f) a
infixl 1 mapMap as <$$>

mapMapMap f = map (map (map f))
infixl 4 mapMapMap as <$$$>

mapMapFlipped a f = mapMap f a
infixl 1 mapMapFlipped as <##>

hole' :: forall a. String -> a
-- hole' msg = unsafeThrow $ "hole: " <> msg
hole' msg = Hole.hole msg

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' x m = case lookup x m of
  Just v -> v
  Nothing -> Bug.bug "lookup failed"

head' :: forall a . List a -> a
head' l = case List.head l of
    Nothing -> Bug.bug "head failed"
    Just a -> a

fromJust :: forall a . Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = Bug.bug "fromJust failed"

fromJust' :: forall t165. String -> Maybe t165 -> t165
fromJust' _ (Just x) = x
fromJust' msg Nothing = Bug.bug $ "fromJust failed: " <> msg


fromRight :: forall a b. Either a b -> b
fromRight (Right b) = b
fromRight _ = Bug.bug "error: fromRight failed"

justWhen :: forall a. Boolean -> (Unit -> a) -> Maybe a
justWhen false _ = Nothing
justWhen true k = Just (k unit)

delete' :: forall v k . Ord k => k -> Map k v -> Map k v
delete' k m  = if member k m then delete k m else Bug.bug "Tried to delete an element not present in the map"
--delete' k m  = delete k m

insert' :: forall v k . Ord k => k -> v -> Map k v -> Map k v
insert' k v m =
    if member k m then Bug.bug "Tried to insert an element already present in the map" else
    insert k v m


mapKeys :: forall k v . Ord k => (k -> k) -> Map k v -> Map k v
mapKeys f m =
--    let bla = toUnfoldable in
    let asList :: List (k /\ v)
        asList = toUnfoldable m in
    fromFoldable (map (\(k /\ v) -> f k /\ v) asList)

-- disjoint union
union' :: forall v k. Ord k => Map k v -> Map k v -> Map k v
union' m1 m2 = unionWith (\_ _ -> Bug.bug "duplicate key in union'") m1 m2

------ disjoint union, or returns Nothing if the same key leads to two different values
--unionCheckConflicts :: forall v k. Ord k => Eq v => Map k v -> Map k v -> Maybe (Map k v)
--unionCheckConflicts m1 m2 =
--    foldl (\macc (k /\ v) -> do
--        acc <- macc
--        case lookup k acc of
--            Just v' | not (v' == v) -> Nothing
--            _ -> pure (Map.insert k v acc))
--        (Just m1) (toUnfoldable m2 :: List (k /\ v))

readUUID :: String -> UUID
readUUID str = fromJust <<< UUID.parseUUID $ str

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
assertSingleton _ = Bug.bug "assertion failed: was not a singleton"

--  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
-- assumes that the thing is nonempty
foldNonempty :: forall a f. Foldable f => (a -> a -> a) -> f a -> a
foldNonempty f l = case foldl (\acc el ->
        case acc of
            Just a -> Just (f a el)
            Nothing -> Just el
        ) Nothing l of
    Nothing -> Bug.bug "assumption violated in foldNonempty: was empty"
    Just res -> res

-- represents a hole but for types
data Hole

stripSuffix :: forall a. Eq a => List.Pattern a -> List a -> Maybe (List a)
stripSuffix (List.Pattern List.Nil) xs = Just xs
stripSuffix (List.Pattern suf) xs0 = go List.Nil xs0
  where
  go _ List.Nil = Nothing
  go ys (x List.: xs)
    | suf == xs = Just (List.reverse (x List.: ys))
    | otherwise = go (x List.: ys) xs

insertAt :: forall a. String -> Int -> a -> Array a -> Array a
insertAt msg i x xs = case Array.insertAt i x xs of
  Nothing -> bug $ "failed insertAt: " <> msg
  Just xs' -> xs'

deleteAt :: forall a. String -> Int -> Array a -> Array a
deleteAt msg i xs = case Array.deleteAt i xs of
  Nothing -> bug $ "failed deleteAt: " <> msg
  Just xs' -> xs'

