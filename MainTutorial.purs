module MainTutorial where

import Prelude
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Editor (editorComponent) as Rendering
import Language.Pantograph.Generic.Rendering.RunnableEditor as RunnableEditor
import Language.Pantograph.Specific.Currying as Currying
import Language.Pantograph.Specific.CurryingInterpereter as CurryingInterpereter
import Language.Pantograph.Specific.FullyApplied as FullyApplied
import Tutorial.CurriedTutorial as CurriedTutorial
import Tutorial.EditorTutorial2 as EditorTutorial2

-- Maybe in the future we can make a better way, but for now you can switch which thing gets run by uncommenting the correct main function
-- Some different languages
--main :: Effect Unit
--main = runEditorForLang FullyApplied.editorSpec
main_standalone :: Effect Unit
main_standalone = runEditorForLang { spec: Currying.editorSpec, interpreter: CurryingInterpereter.interpereter }

--main :: Effect Unit
--main = runEditorForLang Multary.editorSpec
runEditorForLang :: forall l r. Grammar.IsRuleLabel l r => { spec :: Base.EditorSpec l r, interpreter :: Grammar.DerivTerm l r -> String } -> Effect Unit
runEditorForLang { spec, interpreter } =
  HA.runHalogenAff do
    Console.log "[main]"
    body <- HA.awaitBody
    VDomDriver.runUI RunnableEditor.component { spec, interpreter } body

-- Tutorial
main_tutorial :: Effect Unit
main_tutorial = EditorTutorial2.runTutorial Currying.editorSpec CurriedTutorial.lessons CurryingInterpereter.interpereter
