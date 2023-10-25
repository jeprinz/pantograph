module Util where

import Data.Foldable
import Data.Tuple.Nested
import Prelude
import Todo

import Bug (bug)
import Bug as Bug
import Control.Monad.State (StateT(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map, toUnfoldable, fromFoldable, lookup, member, delete, unionWith, insert)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record.Builder as R
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

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
hole' msg = todo msg

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

fromRight' :: forall a b. String -> Either a b -> b
fromRight' _ (Right b) = b
fromRight' msg _ = Bug.bug $ "fromRight failed: " <> msg

fromLeft' :: forall a b. String -> Either a b -> a
fromLeft' _ (Left a) = a
fromLeft' msg _ = Bug.bug $ "fromLeft failed: " <> msg

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

asStateT :: forall m s1 s2 a. Monad m => (s2 -> s1 -> s2) -> (s2 -> s1) -> StateT s1 m a -> StateT s2 m a
asStateT f1 f2 (StateT k) = StateT \s2 -> map (map (f1 s2)) $ k (f2 s2)

indexDeleteAt :: forall a. Int -> Array a -> Maybe (Tuple (Array a) a)
indexDeleteAt i xs = Tuple <$> Array.deleteAt i xs <*> Array.index xs i

uP = unsafePartial

findMapM :: forall m a b. Monad m => (a -> m (Maybe b)) -> Array a -> m (Maybe b)
findMapM f xs = case Array.uncons xs of
  Nothing -> pure Nothing
  Just {head: x, tail: xs'} -> f x >>= case _ of
    Nothing -> findMapM f xs'
    Just b -> pure (Just b)

type Px = Proxy
px = Proxy

foreign import fromHomogenousRecordToTupleArray_ :: forall r a. (String -> a -> String /\ a) -> Record r -> Array (String /\ a)

fromHomogenousRecordToTupleArray :: forall r a. Homogeneous r a => Record r -> Array (String /\ a)
fromHomogenousRecordToTupleArray = fromHomogenousRecordToTupleArray_ Tuple

class RowListKeys :: forall k. RowList k -> Constraint
class RowListKeys rl where
  rowListKeys :: Proxy rl -> Set.Set String

instance RowListKeys Nil where rowListKeys _ = Set.empty
instance (IsSymbol x, RowListKeys rl) => RowListKeys (Cons x a rl) where rowListKeys _ = Set.insert (reflectSymbol (Proxy :: Proxy x)) (rowListKeys (Proxy :: Proxy rl))

class RowKeys :: forall k. Row k -> Constraint
class RowKeys r where
  rowKeys :: Proxy r -> Set.Set String

instance (RowToList r rl, RowListKeys rl) => RowKeys r where rowKeys _ = rowListKeys (Proxy :: Proxy rl)

foreign import unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2

buildFromKeys :: forall r a. RowKeys r => Homogeneous r a => (String -> a) -> Record r
buildFromKeys f = unsafeCoerce $ Array.foldr (\x -> unsafeInsert x (f x)) {} (Array.fromFoldable (rowKeys (Proxy :: Proxy r)))
