module Tutorial.Tutorial where

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
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested
import Data.List (List(..), (:))
import Bug as Bug
import Data.Array as Array
import Util as Util
import Debug (trace)

runTutorial :: Effect Unit
runTutorial = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI (tutorialComponent lessons) unit body

lessons :: Array Lesson
lessons = [
    {component: exampleLesson}
    , {component: exampleLesson}
    , {component: exampleLesson}
    , {component: exampleLesson}
]

--------------------------------------------------------------------------------

data TutorialSubjectOutput = TaskCompleted

type Lesson = {
    component :: forall q m. H.Component q Unit TutorialSubjectOutput m
}

-- I think that I will need to build cons and nil components to form a list. Each component can only have finite children seemingly.
-- The list components will take a number as input, and decide which child to render based on that.

type Slots = ( subject :: forall query. H.Slot query TutorialSubjectOutput Int)
_subject = Proxy :: Proxy "subject" -- what is this nonsense. Look me in the eye and tell me this is good design, whoever designed this crap

data TutorialAction =
    SubjectSolved TutorialSubjectOutput
    | NextLesson
    | PreviousLesson

tutorialComponent ::
    Array Lesson
    ->
    forall query input output m. H.Component query input output m
tutorialComponent lessons =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
    initialState _ = {
        activeLesson : 0
        , lessonsSolved : Array.replicate (Array.length lessons) false
    }

    render :: forall m. _ -> H.ComponentHTML TutorialAction Slots m
    render state =
        trace ("state is " <> show state) \_ ->
        HH.div_ (
            [ HH.text ("Lesson number " <> show state.activeLesson)]
            <> (if not (state.activeLesson == 0) then
                [HH.button [ HE.onClick \_ -> PreviousLesson ] [ HH.text "Previous lesson" ]]
                else [])
            <> (if  state.activeLesson < Array.length lessons - 1 then
                [HH.button [ HE.onClick \_ -> NextLesson ] [ HH.text "Next lesson" ]]
                else [])
            <>
            [
            HH.text (if Util.index' state.lessonsSolved state.activeLesson then "SOLVED" else "NOT YET SOLVED")
            , HH.slot _subject state.activeLesson (Util.index' lessons state.activeLesson).component unit SubjectSolved
            ]
            )

    handleAction :: forall output m. TutorialAction -> H.HalogenM _ TutorialAction Slots output m Unit
    handleAction = case _ of
        SubjectSolved TaskCompleted ->
            H.modify_ \state ->
                state{
                    lessonsSolved= Util.fromJust' "11" $ Array.updateAt state.activeLesson true state.lessonsSolved
                }
        NextLesson ->
            H.modify_ \state ->
                state {activeLesson= state.activeLesson + 1}
        PreviousLesson ->
            H.modify_ \state ->
                state {activeLesson= state.activeLesson - 1}


data ExampleLessonAction = Click

exampleLesson :: forall q m. H.Component q Unit TutorialSubjectOutput m
exampleLesson =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Click ] [ HH.text "Click here to solve this lesson" ]
      ]

  handleAction = case _ of
    Click ->
        H.raise TaskCompleted

