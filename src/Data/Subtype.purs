module Data.Inject where

import Prelude

import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe(..))

class Subtype a b | b -> a where
  inject :: a -> b
  project :: b -> Maybe a

instance Subtype a (Maybe a) where
  inject = Just
  project = identity