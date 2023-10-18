module Main where

--import Language.Pantograph.Specific.STLC (editorSpec)
import Language.Pantograph.Specific.October14 as October14
import Tutorial.Tutorial as Tutorial
import Language.Pantograph.Tutorial.TutorialExample as HenryTutorial
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

-- Maybe in the future we can make a better way, but for now you can switch which thing gets run by uncommenting the correct main function

-- October14
main :: Effect Unit
main = runEditorForLang October14.editorSpec

-- Tutorial test
--main :: Effect Unit
--main = Tutorial.runTutorial

runEditorForLang :: forall l r. Grammar.IsRuleLabel l r => Base.EditorSpec l r -> Effect Unit
runEditorForLang l = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI Rendering.editorComponent l body
