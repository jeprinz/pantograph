module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Pantograph.Specific.ULC as ULC

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  void $ ULC.run body
  pure unit
