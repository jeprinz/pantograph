module Data.Pointed where

import Prelude

import Data.Either (Either)
import Data.Functor.Compose (Compose(..))
import Data.List as List
import Data.List.Rev as Rev
import Data.Maybe (Maybe)
import Data.Newtype (wrap)

class Pointed f where
  point :: forall a. a -> f a

instance Pointed Array where point = pure
instance Pointed List.List where point = pure
instance Pointed Rev.List where point = pure
instance Pointed Maybe where point = pure
instance Pointed (Either a) where point = pure
instance (Pointed f, Pointed g) => Pointed (Compose f g) where point = Compose <<< point <<< point
