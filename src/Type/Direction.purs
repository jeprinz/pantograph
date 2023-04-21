module Type.Direction where

import Prelude

import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy)

data Dir = Down | Up
derive instance Generic Dir _
instance Eq Dir where eq x y = genericEq x y
instance Ord Dir where compare x y = genericCompare x y
instance Enum Dir where succ x = genericSucc x
                        pred x = genericPred x
instance Show Dir where show x = genericShow x

type Down = "down"
type Up = "up"

class IsDir (d :: Symbol) where reflectDir :: Proxy d -> Dir

instance IsDir Down where reflectDir _ = Down
instance IsDir Up where reflectDir _ = Up

-- utilities

class (IsDir d, IsDir d') <= Rev d d' | d -> d'
instance Rev Down Up
instance Rev Up Down
