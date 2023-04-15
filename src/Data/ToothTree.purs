module Data.ToothTree where

import Prelude
import Data.Newtype (class Newtype, over)

newtype ToothTree th (f :: Type -> Type)
  = ToothTree (th (f (ToothTree th f)))

derive instance newtypeToothTree :: Newtype (ToothTree th f) _

mapToothTree ::
  forall th f.
  Functor th =>
  Functor f =>
  (forall x. th (f x) -> th (f x)) -> ToothTree th f -> ToothTree th f
mapToothTree f tt = over ToothTree (f <<< map (map (mapToothTree f))) tt

-- example
data Tooth
  = Var String
  | Lam String
  | App

-- data PathTooth r1 r2 = PathTooth Tooth (Array (ToothTree r1)) r2