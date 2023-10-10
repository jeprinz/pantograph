module Main where

import Pantograph.Generic.Rendering
import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Specific.LC (basicRenderer, scratchRenderer)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  let editorInput = EditorInput
        { renderer: scratchRenderer }
  VDomDriver.runUI editorComponent editorInput body
