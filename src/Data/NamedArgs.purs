module Data.NamedArgs where

import Prelude

import Data.Tuple (Tuple(..))

class NamedArgs t r | t -> r where
  make :: Record r -> t

instance NamedArgs (Tuple a b) (fst :: a, snd :: b) where
  make {fst, snd} = Tuple fst snd
  
instance NamedArgs (Record r) r where
  make = identity
