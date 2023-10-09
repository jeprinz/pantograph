module Language.Pantograph.Tutorial.TutorialExample where

import Prelude

import Effect (Effect)
import Data.Array as Array
import Halogen.Aff as HA
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hole (hole)
import Type.Proxy (Proxy(..))
import Util (fromJust)
import Halogen.VDom.Driver as VDomDriver

-- tutorial

runTutorial :: Effect Unit
runTutorial = HA.runHalogenAff do
  body <- HA.awaitBody
  VDomDriver.runUI tutorialComponent (TutorialInput {lessonStates : [LessonState {n : 10}]}) body

newtype TutorialInput = TutorialInput
  { lessonStates :: Array LessonState }

newtype TutorialState = TutorialState 
  { lessonStates :: Array LessonState 
  , lessonIndex :: Int }

data TutorialAction
  = SetLessonState Int LessonState
  | SetLessonIndex Int

tutorialComponent :: forall query output m. H.Component query TutorialInput output m
tutorialComponent = H.mkComponent
  { initialState: \(TutorialInput {lessonStates}) -> TutorialState {lessonStates, lessonIndex: 0}
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = case _ of
          SetLessonState i lessonState -> H.modify_ \(TutorialState st) -> TutorialState st {lessonStates = fromJust $ Array.updateAt i lessonState st.lessonStates}
          SetLessonIndex i -> H.modify_ \(TutorialState st) -> TutorialState st {lessonIndex = i}
      }
  , render: \(TutorialState {lessonStates, lessonIndex}) ->
      HH.div []
        [ HH.div [] [HH.text $ "lesson index: " <> show lessonIndex]
        , case Array.index lessonStates lessonIndex of
            Nothing -> HH.div [] [HH.text "all done!"]
            Just lessonState ->
              HH.slot (Proxy :: Proxy "lesson") unit lessonComponent (LessonInput {state: lessonState}) case _ of
                CompleteLesson lessonState' -> SetLessonState lessonIndex lessonState'
        , HH.div [] [HH.button [HE.onClick \_ -> SetLessonIndex (lessonIndex - 1)] [HH.text "go back"]]
        ]
  }

-- lesson

newtype LessonInput = LessonInput
  { state :: LessonState }

newtype LessonState = LessonState {n :: Int}

data LessonOutput
  = CompleteLesson LessonState

data LessonAction
  = OutputLessonAction LessonOutput
  | AddWidget

lessonComponent :: forall query m. H.Component query LessonInput LessonOutput m
lessonComponent = H.mkComponent
  { initialState: \(LessonInput {state}) -> state
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = case _ of
          OutputLessonAction output -> H.raise output
          AddWidget -> H.modify_ \(LessonState st) -> LessonState st {n = st.n + 1}
      }
  , render: \state@(LessonState {n}) ->
    HH.div [] $ Array.concat
      [ Array.range 1 n <#> \i ->
          HH.slot_ (Proxy :: Proxy "widget") i widgetComponent i
      , [HH.button [HE.onClick \_ -> AddWidget] [HH.text "remove widget"]]
      , [HH.button [HE.onClick \_ -> OutputLessonAction (CompleteLesson state)] [HH.text "complete"]]
      ]
  }

-- widget

widgetComponent :: forall query output m. H.Component query Int output m
widgetComponent = H.mkComponent
  { initialState: identity
  , eval: H.mkEval $ H.defaultEval {handleAction = \_ -> H.modify_ (1 + _)}
  , render: \i ->
      HH.div [HE.onClick \_ -> unit] [HH.text $ show i]
  }
