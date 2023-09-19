module Main where

import Prelude
import Bug.Assertion (assert, just)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
-- import Language.Pantograph.Specific.STLC (editorSpec)
-- import Language.Pantograph.Generic.Grammar (defaultDerivTerm, (%|-*))
-- import Language.Pantograph.Generic.Rendering.Editor (editorComponent) as Rendering
import Partial.Unsafe as Partial

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  -- VDomDriver.runUI Rendering.editorComponent editorSpec body
  pure unit
