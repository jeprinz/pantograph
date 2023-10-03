module Pantograph.Generic.Rendering.Language where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

renderExpr :: forall ctx env r n d s.
  Lacks "elemId" d =>
  Renderer r n d s ->
  Expr r n d s ->
  RenderM ctx env r n d s (Expr r n (RenderData d) s /\ Html r n d s)
renderExpr (Renderer ren) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        renExpr /\ html <- renderExpr (Renderer ren) expr
        pure {i, expr, renExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: expr'@(Expr {node: node'}), renExpr, html} -> 
          node' /\ {i, expr: expr', renExpr, html}
  nodes <- ren.arrangeExpr (ExprNode node) arrangeKids

  let htmls = nodes # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let html = HH.div 
        [ HU.id elemId
          -- TODO: other attrs
        ]
        htmls

  let renKids = nodes # Array.foldMap case _ of
        Left _ -> []
        Right {renExpr} -> [renExpr]
  let renExpr = Expr 
        { node: ExprNode node {data = R.insert (Proxy :: Proxy "elemId") elemId node.data}
        , kids: renKids }

  pure $ renExpr /\ html
