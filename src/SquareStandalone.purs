module SquareStandalone where

import Prelude

import CSS as CSS
import CSS.Common as CSS
import Control.Monad.State (modify_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
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
  | ToggleFocus

-- TODO: add button that toggles focus (the enabled value) between grammar and object
containerComponent :: forall query input output. H.Component query input output Aff
containerComponent = H.mkComponent { initialState, render, eval }
  where
  initialState _ =
    { lang: Square.LangD "Empty" [ Square.RuleD "MakeUnit" [] (Square.PropD "Unit") ]
    , focusGrammar: true
    , grammarSlotId: 0
    , objectSlotId: 0
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
        SetLang lang -> do
          -- TODO: check validity of lang
          -- has at least one rule
          modify_ \state -> state { lang = lang, objectSlotId = state.objectSlotId + 1 }
        ToggleFocus -> do
          modify_ \state -> state { focusGrammar = not state.focusGrammar, grammarSlotId = state.grammarSlotId + 1, objectSlotId = state.objectSlotId + 1 }
    }

  render state =
    HH.div
      [ HCSS.style do
          CSS.display CSS.flex
          CSS.flexDirection CSS.column
          CSS.justifyContent CSS.spaceBetween
      ]
      [ HH.div
          [ HCSS.style do
              CSS.flexGrow 0.0
              CSS.flexShrink 0.0
              CSS.flexBasis CSS.auto
          ]
          []
      , HH.div
          [ HCSS.style do
              CSS.flexGrow 1.0
              CSS.flexShrink 1.0
              -- 
              CSS.display CSS.flex
              CSS.flexDirection CSS.row
              CSS.justifyContent CSS.spaceBetween
          ]
          [ HH.slot (Proxy @"grammar") state.grammarSlotId RunnableEditor.component { spec: Square.editorSpec, interpreter: SquareInterpreter.interpreter, enabled: state.focusGrammar } SetLang
          , HH.slot_ (Proxy @"object") state.objectSlotId RunnableEditor.component { spec: LangD.editorSpec state.lang, interpreter: const ("No interpreter" /\ unit), enabled: not state.focusGrammar }
          ]
      ]
