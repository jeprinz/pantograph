module Data.List.Rev
  ( RevList
  , reverse
  , unreverse
  , snoc, (:*)
  , unsnoc
  , singleton
  ) where

import Prelude
import Data.Foldable (class Foldable)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)

newtype RevList a = RevList (List.List a)

-- private
unRevList :: forall a. RevList a -> List.List a
unRevList (RevList xs) = xs

-- private
over :: forall a b. (List.List a -> List.List b) -> RevList a -> RevList b
over f = unRevList >>> f >>> RevList

derive newtype instance Show a => Show (RevList a)
derive newtype instance Eq a => Eq (RevList a)
derive newtype instance Functor RevList
derive newtype instance Apply RevList
derive newtype instance Applicative RevList
derive newtype instance Bind RevList
derive newtype instance Monad RevList
derive newtype instance Foldable RevList
derive newtype instance Traversable RevList
derive newtype instance Semigroup (RevList a)
derive newtype instance Monoid (RevList a)

reverse :: forall a. List.List a -> RevList a
reverse = RevList <<< List.reverse

unreverse :: forall a. RevList a -> List.List a
unreverse = List.reverse <<< unRevList

unsnoc :: forall a. RevList a -> Maybe { init :: RevList a, last :: a }
unsnoc = unRevList >>> List.uncons >>> map \{ head, tail } -> { init: RevList tail, last: head }

snoc :: forall a. RevList a -> a -> RevList a
snoc r_xs x = over (List.Cons x) r_xs

infixl 6 snoc as :*

singleton :: forall a. a -> RevList a
singleton = reverse <<< List.singleton
