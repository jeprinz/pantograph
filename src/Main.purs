module Main where

import Prelude

import Bug.Assertion (assert, just)
import Data.Expr ((%), (%*))
import Data.Expr as Expr
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar (SortLabel(..), defaultDerivTerm, (%|-*))
import Language.Pantograph.Generic.Rendering.Base as Rendering
import Language.Pantograph.Generic.Rendering.Editor as Rendering


--------------------------------------------------------------------------------
-- SULC
--------------------------------------------------------------------------------

import Language.Pantograph.SULC.Grammar
import Language.Pantograph.SULC.Rendering

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
    }

