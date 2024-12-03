module Tutorial.EditorTutorial2 where

import Data.Tuple.Nested
import Prelude

import Bug as Bug
import CSS as CSS
import CSS.Cursor as CSSCursor
import CSS.Font as CSSFont
import Data.Array as Array
import Data.Lazy (Lazy, force, defer)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Data.Tuple (Tuple(..))
import Debug (traceM, trace)
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (ComponentSlot)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames, get_url_search_param)
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Type.Direction as Dir
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util as Util
import Web.HTML.Common (AttrName(..))

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

data PantographLessonAction = EditorOutput Unit | Initialize | ResetLesson | PreviousLesson | NextLesson | RunProgram | SetLesson Int

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
      initialState _ =
        { activeLesson : 0
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
                    HH.div_ [HH.a [HP.style "color:lightblue" , HP.href "https://github.com/jeprinz/pantograph/blob/main/README.md"] [HH.text "[About]"]],
                    HH.div_ [HH.text "|"],
                    -- HH.div_ [HH.text $ "Lesson [" <> show (state.activeLesson + 1) <> " / " <> show (Array.length state.lessonsSolved) <> "]"]
                    HH.div
                      [ HCSS.style do
                          CSS.display CSS.flex
                          CSS.flexDirection CSS.row
                          CSS.rule $ CSS.Property (CSS.fromString "gap") (CSS.value (CSS.em 0.5))
                      ] $
                      Array.mapWithIndex Tuple state.lessonsSolved <#> \(Tuple i _) ->
                        HH.div
                          [ HCSS.style do
                              (let s = CSS.em 1.0 in CSS.borderRadius s s s s)
                              CSS.border CSS.solid (CSS.px 1.0) CSS.white
                              CSS.width (CSS.em 1.7)
                              CSS.fontSize (CSS.pt 8.0)
                              CSS.cursor CSSCursor.pointer
                              (let s = CSS.em 0.2 in CSS.padding s (CSS.em 0.0) s (CSS.em 0.0))
                              CSS.rule $ CSS.Property (CSS.fromString "text-align") (CSS.fromString "center")
                              CSS.fontFamily [] $ NonEmpty.singleton $ CSSFont.monospace
                              if (i == state.activeLesson) then do
                                CSS.background CSS.white
                                CSS.color CSS.black
                              else do
                                CSS.background CSS.black
                                CSS.color CSS.white
                          , HE.onClick \_mouseEvent -> SetLesson i
                          ]
                          [ HH.text $ show (i + 1) ]
                ]
                , HH.div [ classNames ["PantographControls"] ] [
                    HH.button [ classNames ["TutorialControlButton"], HE.onClick \_ -> ResetLesson ] [ HH.text "Reset" ]
                    , HH.button [ classNames ["TutorialControlButton"], HP.disabled (state.activeLesson == 0), HE.onClick \_ -> PreviousLesson ] [ HH.text "Previous Lesson" ]
                    , HH.button [ classNames ["TutorialControlButton"], HP.disabled (state.activeLesson == Array.length lessons - 1) ,  HE.onClick \_ -> NextLesson ] [ HH.text "Next Lesson" ]
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
        SetLesson i -> do
            H.modify_ \state ->
                state {activeLesson = i}
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

