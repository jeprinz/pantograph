module Main where

import Prelude

import Data.Expr as Expr
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Language.Pantograph.Generic.Rendering as Rendering
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit 
main = unsafeCrashWith "!TODO fix rendering so can actually run"


-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   Console.log "[main]"
--   body <- HA.awaitBody
--   VDomDriver.runUI Rendering.editorComponent input body
--   where
--   input =
--     { zipper: Expr.Zipper
--         { path: mempty
--         , expr: ex_expr2
--         }
--     , getEdits
--     , renderExprKids: renderExprKids
--     }
