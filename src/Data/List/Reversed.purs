module Data.List.Reversed
  ( ReversedList
  , reverse
  , unreverse
  , snoc
  , unsnoc
  ) where

import Prelude
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)

newtype ReversedList a
  = ReversedList (List a)

-- private
wrap :: forall a. List a -> ReversedList a
wrap = ReversedList

-- private
unwrap :: forall a. ReversedList a -> List a
unwrap (ReversedList xs) = xs

-- private
over :: forall a b. (List a -> List b) -> ReversedList a -> ReversedList b
over f = unwrap >>> f >>> wrap

derive newtype instance showReversedList :: Show a => Show (ReversedList a)

derive newtype instance eqReversedList :: Eq a => Eq (ReversedList a)

derive newtype instance functorReversedList :: Functor ReversedList

derive newtype instance foldableReversedList :: Foldable ReversedList

derive newtype instance traversableReversedList :: Traversable ReversedList

reverse :: forall a. List a -> ReversedList a
reverse = wrap <<< List.reverse

unreverse :: forall a. ReversedList a -> List a
unreverse = List.reverse <<< unwrap

unsnoc :: forall a. ReversedList a -> Maybe { init :: ReversedList a, last :: a }
unsnoc = unwrap >>> List.uncons >>> map \{ head, tail } -> { init: wrap tail, last: head }

snoc :: forall a. ReversedList a -> a -> ReversedList a
snoc r_xs x = over (List.Cons x) r_xs
