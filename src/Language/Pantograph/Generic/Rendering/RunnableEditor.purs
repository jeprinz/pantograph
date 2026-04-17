module Language.Pantograph.Generic.Rendering.RunnableEditor where

import Prelude

import CSS as CSS
import CSS.Font as CSSFont
import Control.Monad.State (get, put)
import Data.Expr (Expr)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Type.Proxy (Proxy(..))

type Slots l r
  = ( editor :: H.Slot (Editor.EditorQuery l r) (Base.EditorSpec l r) Unit )

_editorSlot = Proxy :: Proxy "editor"

data Action l r output
  = RunProgram
  | Receive (Input l r output)

type Interpreter l r output = Grammar.DerivTerm l r -> String /\ output

type Input l r output =
  { spec :: Base.EditorSpec l r
  , interpreter :: Interpreter l r output
  , enabled :: Boolean 
  }

type State l r output = 
  { spec :: Base.EditorSpec l r
  , interpreter :: Expr (Grammar.DerivLabel l r) -> String /\ output
  , output :: String
  , enabled :: Boolean
  }

component ::
  forall l r query output.
  Grammar.IsRuleLabel l r =>
  H.Component query (Input l r output) output Aff
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input l r output -> State l r output
  initialState input =
    { spec: input.spec
    , interpreter: input.interpreter
    , output: ""
    , enabled: input.enabled
    }

  eval =
    H.mkEval
      H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive }

  handleAction = case _ of
    RunProgram -> do
      state <- get
      mprog <- H.request _editorSlot unit Editor.GetProgram
      state <- H.get
      case mprog of
        
        Just prog -> do 
          let outputString /\ output = state.interpreter prog
          H.raise output
          H.modify_ _ { output = outputString }
        Nothing -> pure unit
    
    Receive input -> put $ initialState input

  render state =
    HH.div
      [ do
          HCSS.style do
            CSS.flexGrow 1.0
            CSS.flexShrink 1.0
            CSS.display CSS.flex
            CSS.flexDirection CSS.column
      ]
      [ HH.div
          [ HCSS.style do
              CSS.height (1.5 # CSS.em)
              (let s = 0.5 # CSS.em in CSS.padding s s s s)
              CSS.display CSS.flex
              CSS.flexDirection CSS.row
              CSS.rule $ CSS.Property (CSS.fromString "gap") (CSS.fromString "0.5em")
              CSS.backgroundColor (CSS.rgb 0 0 0)
              CSS.color (CSS.rgb 255 255 255)
          ]
          [ HH.button
              [ HE.onMouseDown (const RunProgram) ]
              [ HH.text "run" ]
          , HH.div
              [ HP.id "evaluation"
              , HCSS.style do
                  CSS.fontFamily [] $ NonEmpty.singleton $ CSSFont.monospace
              ]
              [ HH.text state.output ]
          ]
      , HH.div
          [ HCSS.style do
              (let s = 1.0 # CSS.em in CSS.padding s s s s)
          ]
          [ HH.slot_ _editorSlot unit (Editor.editorComponent unit) { spec: state.spec, enabled: state.enabled } ]
      ]
