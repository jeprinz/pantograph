module SquareStandalone where

import Prelude

import Control.Monad.State (modify_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Rendering.RunnableEditor as RunnableEditor
import Language.Pantograph.Specific.LangD as LangD
import Language.Pantograph.Specific.Square as Square
import Language.Pantograph.Specific.SquareInterpreter as SquareInterpreter
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI containerComponent {} body

data Action 
  = SetLang Square.LangD

containerComponent :: forall query input output. H.Component query input output Aff
containerComponent = H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { lang: Square.LangD "Empty" [ Square.RuleD "MakeUnit" [] (Square.PropD "Unit") ] }

  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
        SetLang lang -> do 
          -- TODO: check validity of lang
          -- has at least one rule
          modify_ _ { lang = lang }
    }

  render state =
    HH.div []
      [ HH.slot (Proxy @"grammar") 0 RunnableEditor.component { spec: Square.editorSpec, interpreter: SquareInterpreter.interpreter } SetLang
      , HH.slot_ (Proxy @"lang") 0 RunnableEditor.component { spec: LangD.editorSpec state.lang , interpreter: const ("No interpreter" /\ unit) } ]
