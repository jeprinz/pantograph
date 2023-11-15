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
import Data.Lazy (Lazy, force, defer)
import Data.Maybe (Maybe(..))
import Debug (traceM, trace)
import Type.Direction as Dir

data PantographLessonAction = EditorOutput Unit | Initialize

_editorSlot = Proxy :: Proxy "lesson"

{-
Represents a Tutorial lesson with the editor.
-}
pantographLesson :: forall l r. Grammar.IsRuleLabel l r =>
    Base.EditorSpec l r -> Lazy (Grammar.DerivTerm l r) -> Lazy (Array (Grammar.DerivPath Dir.Up l r)) -> (forall w i. HH.HTML w i) -> Lesson
pantographLesson spec startTerm markedPaths instructions =
    let paths = defer \_ -> force markedPaths <#> \path -> (Base.HoleyDerivPath path false) in
    let editorComponent = Editor.editorComponent in
    let component _unit =
          H.mkComponent
            { initialState
            , render
            , eval: H.mkEval H.defaultEval {
                handleAction = handleAction,
                handleQuery = handleQuery,
                initialize = Just Initialize
                }
            }
          where
          initialState _ = unit

          render state =
            HH.div_
              [
              HH.slot _editorSlot unit (editorComponent unit) spec{dterm=force startTerm} EditorOutput
              ]

          handleAction = case _ of
            EditorOutput _unit2 -> Bug.bug "not yet implemented"
            Initialize ->
                    H.tell _editorSlot unit (Editor.SetProgram (force startTerm) (force paths))

          handleQuery :: forall a m. LessonQuery a -> H.HalogenM _ _ _ LessonOutput m (Maybe a)
          handleQuery query =
            case query of
                ResetLessonQuery a -> do
                    traceM "reset recieved in pantographLesson"
                    H.tell _editorSlot unit (Editor.SetProgram (force startTerm) (force paths))
                    pure (Just a)
    in
    { instructions , component }
