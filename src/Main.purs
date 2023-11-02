module Main where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Tree.Common (validKidsCount)
import Data.Tree.Traverse (traverseGyro)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Pantograph.Generic.App as App
import Pantograph.Generic.Rendering as P
import Pantograph.Specific.FSTLC as FSTLC
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Util (debug, debugM)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  App.runEditor
    (Proxy :: Proxy FSTLC.SN)
    (P.EditorOptions {verbosity: 0})
