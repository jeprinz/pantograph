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
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List as List
import Data.Traversable (class Traversable, sequence, traverse)

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