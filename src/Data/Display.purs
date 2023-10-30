module Data.Display where

import Data.Tuple.Nested
import Prelude

import Data.Bifunctor (bimap)
import Data.Map as Map
import Halogen.HTML as HH

type Html = HH.HTML Void Unit

embedHtml :: forall w i. i -> Html -> HH.HTML w i
embedHtml i = bimap absurd (const i)

class Display a where
  display :: a -> Html

instance Display Int where display = HH.text <<< show
instance Display Boolean where display = HH.text <<< show
instance Display String where display = HH.text

instance (Show k, Display v) => Display (Map.Map k v) where
  display m = 
    HH.table_ $ (Map.toUnfoldable m :: Array _) <#> \(k /\ v) ->
      HH.tr_ [HH.td_ [HH.text (show k)], HH.td_ [display v]]

class DisplayS a where
  displayS :: a -> Html -> Html
