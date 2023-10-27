module Data.HeteList 
  ( NilTypeList, ConsTypeList
  , HeteList, cons, (:), nil
  , class IndHeteList, indHeteList
  , mapCoerce, foldlCoerce
  , class MapHete, mapHete
  , class FoldlHete, foldlHete
  ) where

import Prelude

import Bug (bug)
import Data.Tuple.Nested ((/\), type (/\))
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

-- Type

data ConsTypeList :: forall x xs. x -> xs -> Type
data ConsTypeList x xs
data NilTypeList

-- HeteList

data HeteList (xs :: Type)
  = ConsHeteList (forall x. x) (HeteList xs)
  | NilHeteList

cons :: forall x xs. x -> HeteList xs -> HeteList (ConsTypeList x xs)
cons x xs = unsafeCoerce ConsHeteList x xs

infixr 6 cons as :

nil :: HeteList NilTypeList
nil = unsafeCoerce NilHeteList

-- indHeteList :: forall xs_ a.
--   { cons :: forall x xs. x -> HeteList xs -> a
--   , nil  :: Unit -> HeteList NilTypeList -> a } ->
--   HeteList xs_ -> a
-- indHeteList ind = case _ of
--   ConsHeteList x xs -> unsafeCoerce ind.cons x xs
--   NilHeteList -> unsafeCoerce ind.nil

class IndHeteList xs a b | xs -> a where
  indHeteList :: a -> HeteList xs -> b

instance IndHeteList NilTypeList (Unit -> a) a where
  indHeteList f NilHeteList = f unit
  indHeteList _ _ = bug "impossible"

instance IndHeteList (ConsTypeList x xs) (x -> HeteList xs -> a) a where
  indHeteList f (ConsHeteList x xs) = f x (unsafeCoerce xs)
  indHeteList _ _ = bug "impossible"

mapCoerce :: forall as bs. (forall a b. a -> b) -> HeteList as -> HeteList bs
mapCoerce _ NilHeteList = NilHeteList
mapCoerce f (ConsHeteList a as) = ConsHeteList (f a) (mapCoerce f as)

foldlCoerce :: forall b as b'. (forall c d c'. c -> d -> c') -> b -> HeteList as -> b'
foldlCoerce _ b NilHeteList = unsafeCoerce b
foldlCoerce f b (ConsHeteList a as) = foldlCoerce f (f b a) as

-- -- FromRecord

-- class FromRecord (r :: Row Type) (xs :: Type) | r -> xs where
--   fromRecord :: Record r -> HeteList xs

-- instance 
--   ( RowToList r l
--   , FromRecordRowList l xs
--   , ListToRow l r ) =>
--   FromRecord r xs
--   where
--   fromRecord = fromRecordRowList

-- class FromRecordRowList (l :: RowList Type) (xs :: Type) | l -> xs where
--   fromRecordRowList :: forall r. RowToList r l => ListToRow l r => Record r -> HeteList xs

-- instance FromRecordRowList RL.Nil NilTypeList where
--   fromRecordRowList _ = nil

-- instance 
--   ( FromRecordRowList l xs, ListToRow l r, RowToList r l ) =>
--   FromRecordRowList (RL.Cons x a l) (ConsTypeList (Proxy x /\ a) xs)
--   where
--   fromRecordRowList r = ?a

-- ToTuple

class ToTuple xs t | xs -> t where
  toTuple :: HeteList xs -> t

instance ToTuple NilTypeList Unit where
  toTuple = indHeteList \_ -> unit

instance ToTuple xs t => ToTuple (ConsTypeList x xs) (x /\ t) where
  toTuple = indHeteList \x xs -> x /\ toTuple xs

-- MapHete

class MapHete fs as bs | fs -> as bs where
  mapHete :: HeteList fs -> HeteList as -> HeteList bs

instance MapHete NilTypeList NilTypeList NilTypeList where
  mapHete _ _ = nil

instance MapHete fs as bs => MapHete (ConsTypeList (a -> b) fs) (ConsTypeList a as) (ConsTypeList b bs) where
  mapHete (ConsHeteList f fs) (ConsHeteList a as) = ConsHeteList (f a) (mapHete fs as)
  mapHete _ _ = bug "impossible"

-- foldl

class FoldlHete fs as b | fs -> as b where
  foldlHete :: HeteList fs -> b -> HeteList as -> b

instance FoldlHete NilTypeList NilTypeList b where
  foldlHete _ b _ = b

instance FoldlHete fs as b' => FoldlHete (ConsTypeList (b -> a -> b') fs) (ConsTypeList a as) b where
  foldlHete (ConsHeteList f fs) b (ConsHeteList a as) = foldlHete fs (f b a) as
  foldlHete _ _ _ = bug "impossible"
