module Tutorial.Tutorial where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen as H
import Halogen.Query.HalogenQ as HQ
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested
import Data.List (List(..), (:))
import Bug as Bug
import Data.Array as Array
import Util as Util
import Debug (trace, traceM)
import Halogen.Utilities (classNames)
import Web.HTML.Common (AttrName(..))

{-
This file defines a framework for Tutorials in general.
A tutorial takes a list of Lessons.
-}

runTutorial :: Array Lesson -> Effect Unit
runTutorial lessons = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI (tutorialComponent lessons) unit body

longString :: String
longString = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

exampleLessons :: Array Lesson
exampleLessons = [
    {component: \_ -> exampleLesson, instructions: HH.text (longString <> longString <> longString)}
    , {component: \_ -> exampleLesson2, instructions: HH.text "lesson2"}
    , {component: \_ -> exampleLesson, instructions: HH.text "lesson3"}
    , {component: \_ -> exampleLesson, instructions: HH.text "lesson4"}
]

--------------------------------------------------------------------------------


data LessonQuery :: forall k. k -> Type
data LessonQuery a
data LessonOutput = TaskCompleted
data LessonInput = LessonInput

type Lesson = {
    -- This is a function so that the tutorial can reset lessons if the user hits the reset button
    component :: Unit -> H.Component LessonQuery LessonInput LessonOutput Aff
    , instructions:: forall w i. HH.HTML w i
}

-- I think that I will need to build cons and nil components to form a list. Each component can only have finite children seemingly.
-- The list components will take a number as input, and decide which child to render based on that.

type Slots = ( lesson :: H.Slot ResettableComponentQuery LessonOutput Int)
_lesson = Proxy :: Proxy "lesson"

data TutorialAction =
    SubjectSolved LessonOutput
    | NextLesson
    | PreviousLesson
    | ResetLesson

tutorialComponent ::
    Array Lesson
    ->
    forall query input output. H.Component query input output Aff
tutorialComponent lessons =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
    initialState _ = {
        activeLesson : 0 :: Int
        , lessonsSolved : Array.replicate (Array.length lessons) false
        , lessonComponents : map (\lesson -> resettableComponent (lesson.component unit)) lessons :: Array (H.Component ResettableComponentQuery LessonInput LessonOutput Aff)
    }

    render :: _ -> H.ComponentHTML TutorialAction Slots Aff
    render state =
        HH.div [classNames["vertical-container"]]
            [
            HH.div [classNames["horizontal-container", "padded"], HP.style "height: 2em"] [
                HH.text ("Lesson number " <> show state.activeLesson)
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
                    (Array.mapWithIndex (\i lessonComponent ->
                        HH.div (if state.activeLesson == i then [] else [classNames ["hidden"]]) [
                            HH.slot _lesson i lessonComponent LessonInput SubjectSolved
                        ]
                        ) state.lessonComponents)
                ]
--                , HH.div [ classNames ["resize-handle--x"] ] []
                , HH.div [ classNames ["vertical-bar", "resize-handle--x"], HP.attr (AttrName "data-target") "aside"] []
                , HH.aside [ classNames ["padded"], HP.style "width: 19em; overflow: auto;"] [
                    HH.div [HP.style "float:right"] [(Util.index' lessons state.activeLesson).instructions]
                ]
            ]
        ]

    handleAction :: forall output. TutorialAction -> H.HalogenM _ TutorialAction Slots output Aff Unit
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
        ResetLesson -> do
            state <- H.get
            H.put (state{lessonsSolved= Util.fromJust $ Array.updateAt state.activeLesson false state.lessonsSolved})
            H.tell _lesson (state.activeLesson) Reset


data ExampleLessonAction = Click

exampleLesson :: forall q m. H.Component q LessonInput LessonOutput m
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
      [
      HH.div_ [HH.text (show state)]
      , HH.button [ HE.onClick \_ -> Click ] [ HH.text "Click here to solve this lesson" ]
      ]

  handleAction = case _ of
    Click -> do
        H.modify_ (\state -> state + 1)
        H.raise TaskCompleted

data ExampleLesson2Action = SetTextArea String | Click2

exampleLesson2 :: forall q m. H.Component q LessonInput LessonOutput m
exampleLesson2 =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = "default text 1"

  render state =
    HH.div_
      [
--        HH.textarea []
        HH.element (HH.ElemName "textarea") [HE.onValueChange (\s -> SetTextArea s)] [HH.text state]
      , HH.button [ HE.onClick \_ -> Click2 ] [ HH.text "Click here to solve this lesson" ]
      ]

  handleAction = case _ of
    SetTextArea str ->
        H.modify_ \_ -> str
    Click2 ->
        H.raise TaskCompleted

--data ResettableComponentQuery :: forall k. k -> Type -> Type
--data ResettableComponentQuery q a
--    = Reset a
data ResettableComponentQuery a = Reset a

resettableComponent ::
    H.Component LessonQuery LessonInput LessonOutput Aff -> H.Component ResettableComponentQuery LessonInput LessonOutput Aff
resettableComponent component = H.unComponent
    (\{initialState, render, eval} -> H.mkComponent {
        render
        , initialState
        , eval: case _ of
            HQ.Initialize a -> eval (HQ.Initialize a)
            HQ.Finalize a -> eval (HQ.Finalize a)
            HQ.Receive input a -> eval (HQ.Receive input a)
            HQ.Action action a -> eval (HQ.Action action a)
            HQ.Query req f ->
--                Coyoneda.unCoyoneda (\g -> map (Maybe.maybe (f unit) g) <<<
--                    case _ of
--                    Reset a -> do
--                        _ <- eval (HQ.Initialize LessonInput)
--                        pure (Just a)
--                    ) req
                    H.mkEval (H.defaultEval
                        {handleQuery = case _ of
                            Reset a -> do
                                H.put (initialState LessonInput)
                                pure (Just a)
                        }
                        ) (HQ.Query req f)
    })
    component

--data HalogenQ query action input a
--  = Initialize a
--  | Finalize a
--  | Receive input a
--  | Action action a
--  | Query (Coyoneda query a) (Unit -> a)
