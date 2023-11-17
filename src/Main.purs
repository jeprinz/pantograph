module Main where

import Language.Pantograph.Specific.FullyApplied as FullyApplied
import Language.Pantograph.Specific.Currying as Currying
--import Language.Pantograph.Specific.Multary as Multary
import Tutorial.Tutorial as Tutorial
import Tutorial.EditorTutorial2 as EditorTutorial2
import Prelude

import Bug.Assertion (assert, just)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar (defaultDerivTerm, (%|-*))
import Language.Pantograph.Generic.Rendering.Editor (editorComponent) as Rendering
import Partial.Unsafe as Partial
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Grammar as Grammar
import Tutorial.CurriedTutorial as CurriedTutorial
import Language.Pantograph.Specific.CurryingInterpereter as CurryingInterpereter

-- Maybe in the future we can make a better way, but for now you can switch which thing gets run by uncommenting the correct main function

-- Some different languages
--main :: Effect Unit
--main = runEditorForLang FullyApplied.editorSpec

--main :: Effect Unit
--main = runEditorForLang Currying.editorSpec

--main :: Effect Unit
--main = runEditorForLang Multary.editorSpec

-- Tutorial
main :: Effect Unit
--main = Tutorial.runTutorial CurriedTutorial.lessons
main = EditorTutorial2.runTutorial Currying.editorSpec CurriedTutorial.lessons CurryingInterpereter.interpereter



--main = Tutorial.runTutorial Tutorial.exampleLessons

runEditorForLang :: forall l r. Grammar.IsRuleLabel l r => Base.EditorSpec l r -> Effect Unit
runEditorForLang l = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI (Rendering.editorComponent unit) l body
