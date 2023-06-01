module Main where

import Prelude

import Language.Pantograph.SULC
import Bug.Assertion (assert, just)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar (defaultDerivTerm, (%|-*))
import Language.Pantograph.Generic.Rendering.Editor (editorComponent) as Rendering

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI Rendering.editorComponent spec body
  where
  sort = TermSort %|-* [CtxNilSort %|-* []]
  dterm = assert (just "main.topTerm" (defaultDerivTerm sort)) identity
  spec =
    { dterm
    , editsAtCursor
    , editsAtHoleInterior
    , arrangeDerivTermSubs
    , stepRules
    }

