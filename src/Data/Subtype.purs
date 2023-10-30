module Data.Subtype where

import Prelude

import Data.Maybe (Maybe(..))
import Util (fromJust')

class Subtype :: Type -> Type -> Constraint
class Subtype a b | a -> b where
  inject :: a -> b
  project :: b -> Maybe a

