module Data.Display where

import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Map as Map
import Effect.Aff (Aff)
import Halogen.HTML as HH

type Html = HH.HTML Void (Aff Unit)

embedHtml :: forall w i. (Aff Unit -> i) -> Html -> HH.HTML w i
embedHtml f = bimap absurd f

class Display a where
  display :: a -> Html

instance Display Int where display = HH.text <<< show
instance Display Boolean where display = HH.text <<< show
instance Display String where display = HH.text

instance (Show k, Display v) => Display (Map.Map k v) where
  display m = 
    HH.table_ $ (Map.toUnfoldable m :: Array _) <#> \(k /\ v) ->
      HH.tr_ [HH.td_ [HH.text (show k)], HH.td_ [display v]]

instance (Display a) => Display (Array a) where
  display as = HH.span_ $ [HH.text "["] <> (Array.intercalate [HH.text ", "] $ (Array.singleton <<< display) <$> as) <> [HH.text "]"]

class DisplayS a where
  displayS :: a -> Html -> Html
