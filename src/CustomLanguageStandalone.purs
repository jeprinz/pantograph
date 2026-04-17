module CustomLanguageStandalone where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.RunnableEditor as RunnableEditor
import Language.Pantograph.Specific.CustomLanguage as CustomLanguage
import Language.Pantograph.Specific.CustomLanguageInterpreter as CustomLanguageInterpreter

main :: Effect Unit
main = runEditorForLang { spec: CustomLanguage.editorSpec, interpreter: CustomLanguageInterpreter.interpereter }

runEditorForLang :: forall l r. Grammar.IsRuleLabel l r => { spec :: Base.EditorSpec l r, interpreter :: Grammar.DerivTerm l r -> String /\ Unit } -> Effect Unit
runEditorForLang { spec, interpreter } =
  HA.runHalogenAff do
    Console.log "[main]"
    body <- HA.awaitBody
    VDomDriver.runUI RunnableEditor.component { spec, interpreter } body
