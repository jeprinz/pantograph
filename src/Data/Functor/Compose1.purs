module Data.Functor.Compose1 where

import Data.Functor.Product as P
import Data.Newtype (class Newtype)

newtype Compose2 f g1 g2 a b = Compose2 (f (g1 a) (g2 b))
derive instance Newtype (Compose2 f g1 g2 a b) _

newtype Compose3 f g1 g2 g3 a b c = Compose3 (f (g1 a) (g2 b) (g3 c))
derive instance Newtype (Compose3 f g1 g2 g3 a b c) _

newtype Const2 a b c = Const2 a
derive instance Newtype (Const2 a b c) _

newtype Const2' a b c = Const2' b
derive instance Newtype (Const2' a b c) _

newtype Const2'' a b c = Const2'' c
derive instance Newtype (Const2'' a b c) _