module Pantograph.Generic.Rendering.Language where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
        , HP.classes (HH.ClassName <$> ["Expr"])
          -- TODO: onClick ==> raise SetCursor here
          -- TODO: onClick ==> raise SetSelect here
        ]
        htmls

  let renKids = nodes # Array.foldMap case _ of
        Left _ -> []
        Right {renExpr} -> [renExpr]
  let renExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ renExpr /\ html

renderCursorExpr :: forall ctx env r n d s.
  Lacks "elemId" d => Lacks "cursor" d =>
  Renderer r n d s ->
  Expr r n (CursorData d) s ->
  RenderM ctx env r n d s (Expr r n (RenderData (CursorData d)) s /\ Html r n d s)
renderCursorExpr (Renderer ren) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        renExpr /\ html <- renderCursorExpr (Renderer ren) expr
        pure {i, expr, renExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renExpr, html} -> 
          ExprNode node' {dat = unCursorData node.dat} /\ {i, renExpr, html}
  let arrangeExprNode = ExprNode node {dat = unCursorData node.dat}
  nodes <- ren.arrangeExpr arrangeExprNode arrangeKids

  let htmls = nodes # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let cursorClasses = case node.dat.cursor of
        Nothing -> []
        Just CursorStatus -> [HH.ClassName "Cursor"]
  let html = HH.div 
        [ HU.id elemId
        , HP.classes ([HH.ClassName "Expr"] <> cursorClasses)
          -- TODO: onClick ==> raise SetCursor here
          -- TODO: onClick ==> raise SetSelect here
        ]
        htmls

  let renKids = nodes # Array.foldMap case _ of
        Left _ -> []
        Right {renExpr} -> [renExpr]
  let renExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ renExpr /\ html

renderSelectExpr :: forall ctx env r n d s.
  Lacks "elemId" d => Lacks "select" d =>
  Renderer r n d s ->
  Expr r n (SelectData d) s ->
  RenderM ctx env r n d s (Expr r n (RenderData (SelectData d)) s /\ Html r n d s)
renderSelectExpr (Renderer ren) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        renExpr /\ html <- renderSelectExpr (Renderer ren) expr
        pure {i, expr, renExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renExpr, html} -> 
          ExprNode node' {dat = unSelectData node.dat} /\ {i, renExpr, html}
  let arrangeExprNode = ExprNode node {dat = unSelectData node.dat}
  nodes <- ren.arrangeExpr arrangeExprNode arrangeKids

  let htmls = nodes # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let selectClasses = case node.dat.select of
        Nothing -> []
        Just OuterSelectStatus -> [HH.ClassName "OuterSelect"]
        Just InnerSelectStatus -> [HH.ClassName "InnerSelect"]
  let html = HH.div 
        [ HU.id elemId
        , HP.classes ([HH.ClassName "Expr"] <> selectClasses)
          -- TODO: onClick ==> raise SetCursor here
          -- TODO: onClick ==> raise SetSelect here
        ]
        htmls

  let renKids = nodes # Array.foldMap case _ of
        Left _ -> []
        Right {renExpr} -> [renExpr]
  let renExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ renExpr /\ html
