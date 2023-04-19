module Data.List.Rev
  ( List
  , reverse
  , unreverse
  , snoc, (:*)
  , unsnoc
  , singleton
  , reversed
  , unreversed
  , length
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List as List
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)

newtype List a = Rev (List.List a)

-- private
wrap = Rev
unwrap (Rev xs) = xs
over f = unwrap >>> f >>> wrap

derive newtype instance Show a => Show (List a)
derive newtype instance Eq a => Eq (List a)
derive newtype instance Functor List
derive newtype instance Apply List
derive newtype instance Applicative List
derive newtype instance Bind List
derive newtype instance Monad List
derive newtype instance Foldable List
derive newtype instance Traversable List
derive newtype instance Semigroup (List a)
derive newtype instance Monoid (List a)

instance FunctorWithIndex Int List where mapWithIndex f = unreversed $ mapWithIndex f
instance FoldableWithIndex Int List where
  foldMapWithIndex f = unreverse >>> foldMapWithIndex f
  foldrWithIndex f b = unreverse >>> foldrWithIndex f b
  foldlWithIndex f b = unreverse >>> foldlWithIndex f b
instance TraversableWithIndex Int List where traverseWithIndex f = unreverse >>> traverseWithIndex f >>> map reverse

reverse = Rev <<< List.reverse

unreverse = List.reverse <<< unwrap

unreversed f = unreverse >>> f >>> reverse

reversed f = reverse >>> f >>> unreverse

unsnoc = unwrap >>> List.uncons >>> map \{ head, tail } -> { init: Rev tail, last: head }

snoc r_xs x = over (List.Cons x) r_xs

infixl 6 snoc as :*

singleton = reverse <<< List.singleton

length = unwrap >>> List.length