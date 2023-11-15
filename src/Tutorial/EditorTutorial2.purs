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

{-
I was having issues with all the components reading keyboard input at the same time before, so now I'm going to do it
with just a single editor and statefully setting the program
-}

_editorSlot = Proxy :: Proxy "lesson"

type Lesson l r = {
    program:: Grammar.DerivTerm l r
    , paths:: Array (Grammar.DerivPath Dir.Up l r)
    , instructions:: String -- (forall w i. HH.HTML w i)
}

data PantographLessonAction = EditorOutput Unit | Initialize | ResetLesson | PreviousLesson | NextLesson

makePantographTutorial :: forall l r q output. Grammar.IsRuleLabel l r =>
    Base.EditorSpec l r
    -> Array (Lazy (Lesson l r))
    -> H.Component q Unit Unit Aff
makePantographTutorial spec lessons =
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
        }

      render state =
        let lesson = force (Util.index' lessons state.activeLesson) in
        HH.div [classNames["vertical-container"]]
            [
            HH.div [classNames["horizontal-container", "padded"], HP.style "height: 2em"] [
                HH.text ("2: Lesson number " <> show state.activeLesson)
                , HH.button [ HE.onClick \_ -> ResetLesson ] [ HH.text "Reset" ]
                , HH.button [ HP.disabled (state.activeLesson == 0), HE.onClick \_ -> PreviousLesson ] [ HH.text "Previous lesson" ]
                , HH.button [ HP.disabled (state.activeLesson == Array.length lessons - 1) ,  HE.onClick \_ -> NextLesson ] [ HH.text "Next lesson" ]
                , HH.text (if Util.index' state.lessonsSolved state.activeLesson then "SOLVED" else "NOT YET SOLVED")
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
                , HH.aside [ classNames ["padded"], HP.style "width: 19em; overflow: auto;"] [
                    HH.div [HP.style "float:right"] [HH.text lesson.instructions] -- [lesson.instructions]
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

runTutorial :: forall l r. Grammar.IsRuleLabel l r => Base.EditorSpec l r -> Array (Lazy (Lesson l r)) -> Effect Unit
runTutorial spec lessons = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI (makePantographTutorial spec lessons) unit body
