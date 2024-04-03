module MainTutorial (main) where

import Prelude
import Effect (Effect)
import Language.Pantograph.Specific.Currying as Currying
import Language.Pantograph.Specific.CurryingInterpereter as CurryingInterpereter
import Tutorial.CurriedTutorial as CurriedTutorial
import Tutorial.EditorTutorial2 as EditorTutorial2

main :: Effect Unit
main =
  EditorTutorial2.runTutorial
    Currying.editorSpec
    CurriedTutorial.lessons
    CurryingInterpereter.interpereter
