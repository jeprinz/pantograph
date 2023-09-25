module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Generic.Rendering as Rendering
import Partial.Unsafe as Partial
import Pantograph.Specific.ULC

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  -- void $ VDomDriver.runUI Rendering.editorComponent
  --   { term: ?a 
  --   , ctx: ?a 
  --   , env: ?a }
  --   body
  pure unit
