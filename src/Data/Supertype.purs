module Data.Supertype where

import Prelude

import Data.Maybe (Maybe(..))

class Supertype :: Type -> Type -> Constraint
class Supertype b a | b -> a where
  inject :: a -> b
  project :: b -> Maybe a

instance Supertype (Maybe a) a where
  inject = Just
  project = identity

