module Main where

import Prelude
import Effect (Effect)
import MainStandalone as MainStandalone
import MainTutorial as MainTutorial

{-
-- Maybe in the future we can make a better way, but for now you can switch which thing gets run by uncommenting the correct main function

-- Some different languages
--main :: Effect Unit
--main = runEditorForLang FullyApplied.editorSpec

main_standalone :: Effect Unit
main_standalone = runEditorForLang {spec: Currying.editorSpec, interpreter: CurryingInterpereter.interpereter }

--main :: Effect Unit
--main = runEditorForLang Multary.editorSpec

runEditorForLang :: forall l r. Grammar.IsRuleLabel l r => { spec :: Base.EditorSpec l r, interpreter :: Grammar.DerivTerm l r -> String } -> Effect Unit
runEditorForLang {spec, interpreter }  = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI RunnableEditor.component {spec, interpreter } body

-- Tutorial
main_tutorial :: Effect Unit
main_tutorial = EditorTutorial2.runTutorial Currying.editorSpec CurriedTutorial.lessons CurryingInterpereter.interpereter

data Mode = Standalone | Tutorial

main :: Effect Unit
main = case Tutorial of 
  Standalone -> main_standalone
  Tutorial -> main_tutorial
-}
data Mode
  = Standalone
  | Tutorial

main :: Effect Unit
main =
  let
    opts =
      { active_mode: Tutorial
      }
  in
    case opts.active_mode of
      Standalone -> MainStandalone.main
      Tutorial -> MainTutorial.main
