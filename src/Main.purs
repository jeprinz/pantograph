module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Specific.LC (editorComponent, editorInput)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  HA.awaitBody >>=
    VDomDriver.runUI editorComponent editorInput
