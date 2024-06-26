module Data.List.Rev
  ( RevList
  , toReversedList
  , fromReversedList
  , reverse
  , reverseArray
  , unreverse
  , snoc, (@@)
  , unsnoc
  , singleton
  , reversed
  , unreversed
  , length
  , null
  , unzip
  , zipWith
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List as List
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Bifunctor
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple)

newtype RevList a = Rev (List.List a)

-- private
wrap = Rev
unwrap (Rev xs) = xs
over f = unwrap >>> f >>> wrap

derive newtype instance Show a => Show (RevList a)
derive newtype instance Eq a => Eq (RevList a)
derive newtype instance Ord a => Ord (RevList a)
derive newtype instance Functor RevList
derive newtype instance Apply RevList
derive newtype instance Applicative RevList
derive newtype instance Bind RevList
derive newtype instance Monad RevList

instance Foldable RevList where
  foldr f b = foldr f b <<< unreverse
  foldl f b = foldl f b <<< unreverse
  foldMap f = foldMap f <<< unreverse

instance Traversable RevList where
  traverse f = map reverse <<< traverse f <<< unreverse
  sequence = map reverse <<< sequence <<< unreverse

derive newtype instance Semigroup (RevList a)
derive newtype instance Monoid (RevList a)
derive instance Generic (RevList a) _
instance EncodeJson a => EncodeJson (RevList a) where encodeJson a = genericEncodeJson a
instance DecodeJson a => DecodeJson (RevList a) where decodeJson a = genericDecodeJson a
-- derive newtype instance (Applicative m, Plus m, Unify m a) => Unify m (RevList a)

-- !TODO is this used anywhere?
-- instance FunctorWithIndex Int RevList where mapWithIndex f = unreversed $ mapWithIndex f
-- instance FoldableWithIndex Int RevList where
--   foldMapWithIndex f = unreverse >>> foldMapWithIndex f
--   foldrWithIndex f b = unreverse >>> foldrWithIndex f b
--   foldlWithIndex f b = unreverse >>> foldlWithIndex f b
-- instance TraversableWithIndex Int RevList where traverseWithIndex f = unreverse >>> traverseWithIndex f >>> map reverse

toReversedList = unwrap
fromReversedList = wrap

reverse = Rev <<< List.reverse

reverseArray = Rev <<< List.fromFoldable <<< Array.reverse

unreverse = List.reverse <<< unwrap

unreversed f = unreverse >>> f >>> reverse

reversed f = reverse >>> f >>> unreverse

unsnoc = unwrap >>> List.uncons >>> map \{ head, tail } -> { init: Rev tail, last: head }

snoc r_xs x = over (List.Cons x) r_xs

infixl 6 snoc as @@

singleton = reverse <<< List.singleton

length = unwrap >>> List.length

null = unwrap >>> List.null

unzip :: forall t56 t57. RevList (Tuple t56 t57) -> Tuple (RevList t56) (RevList t57)
unzip = (bimap wrap wrap) <<< List.unzip <<< unwrap

zipWith :: forall a4045 a46 b47. (a46 -> b47 -> a4045) -> RevList a46 -> RevList b47 -> RevList a4045
zipWith f l1 l2 = wrap (List.zipWith f (unwrap l1) (unwrap l2))

