module Data.Display where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested
import Halogen.HTML as HH

type Html = HH.PlainHTML

class Display a where
  display :: a -> Html

instance Display Int where display = HH.text <<< show
instance Display Boolean where display = HH.text <<< show
instance Display String where display = HH.text

instance (Show k, Display v) => Display (Map.Map k v) where
  display m = 
    HH.table_ $ (Map.toUnfoldable m :: Array _) <#> \(k /\ v) ->
      HH.tr_ [HH.td_ [HH.text (show k)], HH.td_ [display v]]
