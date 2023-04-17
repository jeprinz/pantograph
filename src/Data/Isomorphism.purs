module Data.Isomorphism where

import Prelude

import Data.Identity (Identity(..))

class Isomorphism f where
  to :: forall a. a -> f a 
  from :: forall a. f a -> a

instance Isomorphism Identity where
  to = Identity
  from (Identity a) = a
