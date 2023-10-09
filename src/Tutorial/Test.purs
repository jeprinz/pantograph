module Test.Test where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar (defaultDerivTerm, (%|-*))
import Language.Pantograph.Generic.Rendering.Editor (editorComponent) as Rendering
import Partial.Unsafe as Partial
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen as H
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested
import Data.List (List(..), (:))
import Bug as Bug

type Input = { label :: String }

type State = { label :: String }

type Slots = ( button :: forall query. H.Slot query Void Int )

_button = Proxy :: Proxy "button"

button :: forall query output m. H.Component query Input output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: Input -> State
  initialState input = input

  render :: forall state action. state -> H.ComponentHTML action Slots m
  render _ =
      HH.div_ [ HH.slot_ _button 0 button { label: "Click Me" } ]

