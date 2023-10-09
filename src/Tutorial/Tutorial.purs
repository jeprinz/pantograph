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

runTutorial :: Effect Unit
runTutorial = HA.runHalogenAff do
  Console.log "[runTutorial]"
  body <- HA.awaitBody
  VDomDriver.runUI (tutorialComponent {component: exampleTutorialSubject}) unit body

data TutorialSubjectOutput = TaskCompleted

type TutorialSubject a = {
    component :: forall q m. H.Component q Unit TutorialSubjectOutput m
}

-- I think that I will need to build cons and nil components to form a list. Each component can only have finite children seemingly.
-- The list components will take a number as input, and decide which child to render based on that.

type Slots = ( subject :: forall query. H.Slot query TutorialSubjectOutput Int)
_subject = Proxy :: Proxy "subject" -- what is this nonsense. Look me in the eye and tell me this is good design, whoever designed this crap

data TutorialAction = SubjectSolved TutorialSubjectOutput

tutorialComponent :: forall a.
    TutorialSubject a
    ->
    forall query input output m. H.Component query input output m
tutorialComponent subject =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
    initialState _ = false

    render :: forall m. Boolean -> H.ComponentHTML TutorialAction Slots m
    render state =
        HH.div_
            [ HH.text "Hello this is a tutorial"
            , HH.slot _subject 0 subject.component unit SubjectSolved
            , HH.text (if state then "SOLVED" else "NOT YET SOLVED")
            ]

    handleAction :: forall output m. TutorialAction -> H.HalogenM Boolean TutorialAction Slots output m Unit
    handleAction = case _ of
        SubjectSolved TaskCompleted ->
            H.modify_ \_state -> true

type ConsSlots query input output = ( cons1 :: forall query. H.Slot query output input
                 , cons2 :: forall query. H.Slot query output (Int /\ input))
_cons1 = Proxy :: Proxy "cons1"
_cons2 = Proxy :: Proxy "cons2"

-- This is a component which will display the n'th component that it is given
--makeOneOfListComponent :: forall query input output m. List (H.Component query input output m) -> H.Component query (Int /\ input) output m
--makeOneOfListComponent = case _ of
--    Nil ->
--        let error :: forall t. t
--            error = Bug.bug "list component index out of bounds"
--        in
--        H.mkComponent {
--            initialState: \_ -> unit
--            , render: \_ -> error
--            , eval: H.mkEval $ H.defaultEval { handleAction = \_ -> error }
--        }
--    component : components ->
--      H.mkComponent
--        { initialState
--        , render
--        , eval: H.mkEval H.defaultEval {
--            handleAction = handleAction
--            , receive = Just
--          }
--        }
--      where
--      initialState input = input
--
--      render state = if state == 0 then
--        -- Render this child component
--        ?h
--        else
--        -- Go to next cons in list
--        ?h
--
--      handleAction input =
--            H.modify_ \_ -> input


data ExampleTutorialSubjectAction = Click

exampleTutorialSubject :: forall q m. H.Component q Unit TutorialSubjectOutput m
exampleTutorialSubject =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Click ] [ HH.text "-" ]
      ]

  handleAction = case _ of
    Click ->
        H.raise TaskCompleted

