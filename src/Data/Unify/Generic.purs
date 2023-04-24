module Data.Unify.Generic where

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from, to)
import Prelude
import Control.Alternative (class Plus, empty)
import Data.Unify (class Unify, unify)

class GenericUnify m a where
  genericUnify' :: a -> a -> m a

instance Applicative m => GenericUnify m NoConstructors where
  genericUnify' x _ = pure x

instance Applicative m => GenericUnify m NoArguments where
  genericUnify' x _ = pure x

instance (Functor m, Plus m, GenericUnify m a, GenericUnify m b) => GenericUnify m (Sum a b) where
  genericUnify' (Inl a1) (Inl a2) = Inl <$> genericUnify' a1 a2
  genericUnify' (Inr b1) (Inr b2) = Inr <$> genericUnify' b1 b2
  genericUnify' _ _ = empty

instance (Applicative m, GenericUnify m a, GenericUnify m b) => GenericUnify m (Product a b) where
  genericUnify' (Product a1 b1) (Product a2 b2) = Product <$> genericUnify' a1 a2 <*> genericUnify' b1 b2

instance (Functor m, GenericUnify m a) => GenericUnify m (Constructor name a) where
  genericUnify' (Constructor a1) (Constructor a2) = Constructor <$> genericUnify' a1 a2

instance (Functor m, Unify m a) => GenericUnify m (Argument a) where
  genericUnify' (Argument a1) (Argument a2) = Argument <$> unify a1 a2

genericUnify :: forall m a rep. Functor m => Generic a rep => GenericUnify m rep => a -> a -> m a
genericUnify x y = to <$> genericUnify' (from x) (from y)
