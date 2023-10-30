module Data.Supertype where

import Prelude

import Data.Maybe (Maybe(..))

class Supertype :: Type -> Type -> Constraint
class Supertype a b | b -> a where
  inject :: a -> b
  project :: b -> Maybe a

instance Supertype a (Maybe a) where
  inject = Just
  project = identity

