module Main where

import Pantograph.Generic.Rendering (EditorInput(..), editorComponent)
import Prelude
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Specific.LC.Rendering.Basic as LcRenderingBasic
import Pantograph.Specific.LC.Rendering.Scratch as LcRenderingScratch

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  HA.awaitBody >>=
    VDomDriver.runUI
      editorComponent
      (EditorInput
        { renderer:
            LcRenderingBasic.renderer 
            -- LcRenderingScratch.renderer 
        })
