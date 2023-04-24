module Data.Unify where

import Prelude

import Control.Plus (class Plus, empty)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

class Unify m a where unify :: a -> a -> m a

-- Eq a => Unify Maybe a
eqUnify :: forall a. Eq a => a -> a -> Maybe a
eqUnify x y = if x == y then Just x else Nothing

instance (Applicative m, Unify m a, Unify m b) => Unify m (Tuple a b) where
  unify (Tuple a1 b1) (Tuple a2 b2) = Tuple <$> unify a1 a2 <*> unify b1 b2

instance (Applicative m, Plus m, Unify m a) => Unify m (List a) where
  unify l@List.Nil List.Nil = pure l
  unify (List.Cons x1 xs1) (List.Cons x2 xs2) = List.Cons <$> unify x1 x2 <*> unify xs1 xs2
  unify _ _ = empty

