module Main where

import Language.Pantograph.SULC.Grammar
import Language.Pantograph.SULC.Rendering
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
import Language.Pantograph.Generic.Rendering as Rendering

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI Rendering.editorComponent spec body
  where
  topSort = TermSort %|-* [CtxNilSort %|-* []]
  topTerm = assert (just "main.topTerm" (defaultDerivTerm topSort)) identity
  spec =
    { hdzipper: Rendering.InjectHoleyDerivZipper (Expr.Zipper mempty topTerm)
    , topSort
    , editsAtHoleyDerivZipper
    , renderDerivTermKids'
    }
