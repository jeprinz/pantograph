module Language.Pantograph.Generic.Rendering.RunnableEditor where

import Prelude
import CSS as CSS
import CSS.Font as CSSFont
import Data.Expr (Expr)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Type.Proxy (Proxy(..))

type Slots l r
  = ( editor :: H.Slot (Editor.EditorQuery l r) (Base.EditorSpec l r) Unit )

_editorSlot = Proxy :: Proxy "editor"

data Action
  = RunProgram

component ::
  forall l r query output.
  Grammar.IsRuleLabel l r =>
  H.Component query { spec :: Base.EditorSpec l r, interpreter :: Grammar.DerivTerm l r -> String } output Aff
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: { spec :: Base.EditorSpec l r, interpreter :: Grammar.DerivTerm l r -> String } -> { spec :: Base.EditorSpec l r, interpreter :: Expr (Grammar.DerivLabel l r) -> String, output :: String }
  initialState input =
    { spec: input.spec
    , interpreter: input.interpreter
    , output: ""
    }

  eval =
    H.mkEval
      H.defaultEval
        { handleAction = handleAction }

  handleAction = case _ of
    RunProgram -> do
      mprog <- H.request _editorSlot unit Editor.GetProgram
      state <- H.get
      case mprog of
        Just prog -> H.modify_ _ { output = state.interpreter prog }
        Nothing -> pure unit

  render state =
    HH.div
      [ do
          HCSS.style do
            CSS.display CSS.flex
            CSS.flexDirection CSS.column
      ]
      [ HH.div
          [ HCSS.style do
              (let s = 1.0 # CSS.em in CSS.padding s s s s)
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
              [ HCSS.style do
                  CSS.fontFamily [] $ NonEmpty.singleton $ CSSFont.monospace
              ]
              [ HH.text state.output ]
          ]
      , HH.div
          [ HCSS.style do
              (let s = 1.0 # CSS.em in CSS.padding s s s s)
          ]
          [ HH.slot_ _editorSlot unit (Editor.editorComponent unit) state.spec ]
      ]
