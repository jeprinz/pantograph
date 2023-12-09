module Tutorial.EditorTutorial2 where

import Prelude
import Halogen.HTML as HH
import Halogen as H
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Halogen.VDom.Driver as VDomDriver
import Effect.Class.Console as Console
import Language.Pantograph.Generic.Rendering.Base as Base
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Effect (Effect)
import Type.Proxy (Proxy(..))
import Bug as Bug
import Data.Lazy (Lazy, force, defer)
import Data.Maybe (Maybe(..))
import Debug (traceM, trace)
import Type.Direction as Dir
import Data.Tuple.Nested
import Effect.Aff (Aff)
import Util as Util
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Data.Array as Array
import Web.HTML.Common (AttrName(..))
import Halogen.Component (ComponentSlot)
import Unsafe.Coerce (unsafeCoerce)

{-
I was having issues with all the components reading keyboard input at the same time before, so now I'm going to do it
with just a single editor and statefully setting the program
-}

type Slots l r = ( editor :: H.Slot (Editor.EditorQuery l r) (Base.EditorSpec l r) Unit)
_editorSlot = Proxy :: Proxy "editor"

--newtype MyHTML = MyHTML (forall w i. HH.HTML w i)

type Lesson l r = {
    program:: Grammar.DerivTerm l r
    , paths:: Array (Grammar.DerivPath Dir.Up l r)
    , instructions:: HH.HTML Unit Unit -- forall w i. HH.HTML w i
}

data PantographLessonAction = EditorOutput Unit | Initialize | ResetLesson | PreviousLesson | NextLesson | RunProgram

makePantographTutorial :: forall l r query input output. Grammar.IsRuleLabel l r =>
    Base.EditorSpec l r
    -> Array (Lazy (Lesson l r))
    -> (Grammar.DerivTerm l r -> String)
    -> H.Component query input output Aff
makePantographTutorial spec lessons interpereter =
--    let paths = defer \_ -> force markedPaths <#> \path -> (Base.HoleyDerivPath path false) in

      H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval {
            handleAction = handleAction,
--                handleQuery = handleQuery,
            initialize = Just Initialize
            }
        }
      where
      editorComponent = Editor.editorComponent unit
      convertPaths markedPaths = markedPaths <#> \path -> (Base.HoleyDerivPath path false)
      initialState _ = {
        activeLesson : 0
        , lessonsSolved : Array.replicate (Array.length lessons) false
        , output : ""
        }
--      render :: _ -> H.ComponentHTML PantographLessonAction ( editor :: H.Slot (Editor.EditorQuery l r) (Base.EditorSpec l r) Unit) Aff
--      render :: _ -> H.ComponentHTML PantographLessonAction (Slots l r) Aff
      render state =
        let lesson = force (Util.index' lessons state.activeLesson) in
        HH.div [classNames["vertical-container"]]
            [
            HH.div [classNames["PantographHeader", "horizontal-container", "padded"],
                HP.style "height: 1.4em; justify-content: space-between"] [
                HH.div [ classNames ["PantographTitle"] ] [
                    HH.div_ [HH.text "Pantograph"],
                    HH.div_ [HH.text "|"],
                    HH.div_ [HH.text $ "Lesson " <> show (state.activeLesson + 1)]
                ]
                , HH.div [ classNames ["PantographControls"] ] [
                    HH.button [ classNames ["TutorialControlButton"], HE.onClick \_ -> ResetLesson ] [ HH.text "Reset" ]
                    , HH.button [ classNames ["TutorialControlButton"], HP.disabled (state.activeLesson == 0), HE.onClick \_ -> PreviousLesson ] [ HH.text "Previous lesson" ]
                    , HH.button [ classNames ["TutorialControlButton"], HP.disabled (state.activeLesson == Array.length lessons - 1) ,  HE.onClick \_ -> NextLesson ] [ HH.text "Next lesson" ]
                ]
--                , HH.text (if Util.index' state.lessonsSolved state.activeLesson then "SOLVED" else "NOT YET SOLVED")
            ]
--            , HH.hr [HP.style "width: 5px"]
            , HH.div [ classNames ["horizontal-bar"], HP.style "height: 2px;" ] []
--            , HH.div [ classNames ["horizontal-container sidebar-container"] ] [
            , HH.div [ classNames ["horizontal-container", "fill-space"], HP.style "height: calc(100vh - 3em - 2px);" ] [
                HH.main [ classNames ["fill-space", "padded"], HP.style "overflow: auto"] [
                    HH.div_
                      [HH.slot _editorSlot unit editorComponent spec EditorOutput]
                ]
--                , HH.div [ classNames ["resize-handle--x"] ] []
                , HH.div [ classNames ["vertical-bar", "resize-handle--x"], HP.attr (AttrName "data-target") "aside"] []
                , HH.aside [ classNames [], HP.style "width: 650px; overflow: auto;"] [
                    HH.div [classNames ["vertical-container"]] [
                        HH.div [HP.style "height: 3em"] [
                            HH.button [ classNames ["TutorialControlButton"], HE.onClick \_ -> RunProgram, HP.style "margin: 10px" ] [ HH.text "Run" ]
                            , HH.span [HP.style "font-family: monospace; font-size: 12pt"] [HH.text state.output]
                        ]
                        -- , HH.div [ classNames ["horizontal-bar"], HP.style "height: 2px;" ] []
                        , HH.div [HP.style "float:right", classNames ["padded", "lessonInstructions"]] [unsafeCoerce lesson.instructions]
                    ]
                ]
            ]
        ]

      setLesson = do
                state <- H.get
                let lesson = force (Util.index' lessons state.activeLesson)
                H.tell _editorSlot unit (Editor.SetProgram (lesson.program) (convertPaths lesson.paths))

      handleAction = case _ of
        EditorOutput _unit2 -> Bug.bug "not yet implemented"
        Initialize -> setLesson
        NextLesson -> do
            H.modify_ \state ->
                state {activeLesson= state.activeLesson + 1}
            setLesson
        PreviousLesson -> do
            H.modify_ \state ->
                state {activeLesson= state.activeLesson - 1}
            setLesson
        ResetLesson -> do
            setLesson
        RunProgram -> do
            mprog <- H.request _editorSlot unit (Editor.GetProgram)
            state <- H.get
            case mprog of
                Just prog ->
                    H.modify_ \state ->
                        state {output = interpereter prog}
                Nothing -> pure unit

runTutorial :: forall l r. Grammar.IsRuleLabel l r => Base.EditorSpec l r
    -> Array (Lazy (Lesson l r)) -> (Grammar.DerivTerm l r -> String) -> Effect Unit
runTutorial spec lessons interpereter = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI (makePantographTutorial spec lessons interpereter) unit body

