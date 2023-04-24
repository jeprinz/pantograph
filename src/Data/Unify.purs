module Data.Unify where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

class Unify m a where unify :: a -> a -> m a

-- Plus m, Applicative m, Eq a => Unify m a
eqUnify :: forall m a. Plus m => Applicative m => Eq a => a -> a -> m a
eqUnify x y = if x == y then pure x else empty

instance (Applicative m, Unify m a, Unify m b) => Unify m (Tuple a b) where
  unify (Tuple a1 b1) (Tuple a2 b2) = Tuple <$> unify a1 a2 <*> unify b1 b2

instance (Applicative m, Plus m, Unify m a, Unify m b) => Unify m (Either a b) where
  -- unify (Tuple a1 b1) (Tuple a2 b2) = Tuple <$> unify a1 a2 <*> unify b1 b2
  unify (Left a1) (Left a2) = Left <$> unify a1 a2
  unify (Right b1) (Right b2) = Right <$> unify b1 b2
  unify _ _ = empty

instance (Applicative m, Plus m, Unify m a) => Unify m (List a) where
  unify l@List.Nil List.Nil = pure l
  unify (List.Cons x1 xs1) (List.Cons x2 xs2) = List.Cons <$> unify x1 x2 <*> unify xs1 xs2
  unify _ _ = empty

instance (Plus m, Applicative m) => Unify m String where unify = eqUnify
instance (Plus m, Applicative m) => Unify m Int where unify = eqUnify
instance (Plus m, Applicative m) => Unify m Char where unify = eqUnify
instance (Plus m, Applicative m) => Unify m Boolean where unify = eqUnify
