module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Pantograph.Generic.Rendering as PR
import Pantograph.Specific.FSTLC as FSTLC
import Pantograph.Specific.LC as LC

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  FSTLC.runEditor (PR.EditorOptions {verbosity: 0})
