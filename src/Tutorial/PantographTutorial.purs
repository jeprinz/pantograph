module Tutorial.PantographTutorial where

import Prelude
import Halogen.HTML as HH
import Halogen as H
import Tutorial.Tutorial
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Language.Pantograph.Generic.Rendering.Base as Base
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Bug as Bug

data PantographLessonAction = Click | EditorOutput Unit

pantographLesson :: forall l r. Grammar.IsRuleLabel l r =>
    Base.EditorSpec l r -> (forall w i. HH.HTML w i) -> Lesson
pantographLesson spec instructions =
    let editorComponent = Editor.editorComponent in
    let component _unit =
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
              , HH.slot (Proxy :: Proxy "editor") unit editorComponent spec EditorOutput
              ]

          handleAction = case _ of
            Click -> do
                H.modify_ (\state -> state + 1)
                H.raise TaskCompleted
            EditorOutput _unit2 -> Bug.bug "not yet implemented"
    in
    { instructions , component }
