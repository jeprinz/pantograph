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
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen as H
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested
import Data.List (List(..), (:))
import Bug as Bug
import Tutorial.Parser (parser, Parser)
import Debug (traceM)

foreign import setupTextEditor :: Parser -> Unit

runTest :: Effect Unit
runTest = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI testComponent unit body

data TestAction = Initialize

testComponent :: forall query input output. H.Component query input output Aff
testComponent =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            {initialize = Just Initialize
            , handleAction = handleAction
            }
        }
    where
    initialState _ = {
    }

    render :: _ -> H.ComponentHTML TestAction () Aff
    render state = HH.div [HP.id "codemirror"] []

    handleAction = case _ of
        Initialize -> do
            _ <- pure $ setupTextEditor parser
            pure unit
