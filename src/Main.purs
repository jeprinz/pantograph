module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Generic.Rendering (EditorInput(..), editorComponent)
import Pantograph.Specific.LC.Rendering.Basic as LcRenderingBasic
-- import Pantograph.Specific.LC.Rendering.Scratch as LcRenderingScratch
-- import Pantograph.Specific.Sexp.Rendering as SexpRendering

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
            -- SexpRendering.renderer
        })
