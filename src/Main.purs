module Main where

import Language.Pantograph.ULC.Grammar
import Language.Pantograph.ULC.Rendering
import Prelude

import Data.Expr ((%), (%*))
import Data.Expr as Expr
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Grammar (holeDerivExpr)
import Language.Pantograph.Generic.Rendering as Rendering

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  body <- HA.awaitBody
  VDomDriver.runUI Rendering.editorComponent spec body
  where
  spec =
    { dzipper: Expr.Zipper
        { path: mempty
        , expr: holeDerivExpr (TermSort %* [])
        }
    , getEdits
    , renderDerivExprKids
    }
