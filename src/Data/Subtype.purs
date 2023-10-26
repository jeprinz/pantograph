module Data.Subtype where

import Prelude

import Data.Maybe (Maybe(..))

class Subtype :: Type -> Type -> Constraint
class Subtype a b | b -> a where
  inject :: a -> b
  project :: b -> Maybe a

instance Subtype a (Maybe a) where
  inject = Just
  project = identity