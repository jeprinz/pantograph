module Pantograph.Generic.Rendering.Language where

import Data.Tuple.Nested ((/\))
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Utilities as HU
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

renderExpr :: forall ctx env r n d s.
  Lacks "elemId" d =>
  Renderer r n d s ->
  Expr r n d s ->
  RenderM ctx env r n d s {renderedExpr :: Expr r n (RenderData d) s, html :: Html r n d s}
renderExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        {renderedExpr, html} <- renderExpr (Renderer renderer) expr
        pure {i, expr, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: expr'@(Expr {node: node'}), renderedExpr, html} -> 
          node' /\ {i, expr: expr', renderedExpr, html}
  nodes <- renderer.arrangeExpr (ExprNode node) arrangeKids

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
        Right {renderedExpr} -> [renderedExpr]
  let renderedExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ {renderedExpr, html}

renderCursorExpr :: forall ctx env r n d s.
  Lacks "elemId" d => Lacks "cursor" d =>
  Renderer r n d s ->
  Expr r n (CursorData d) s ->
  RenderM ctx env r n d s {renderedExpr :: Expr r n (RenderData (CursorData d)) s, html :: Html r n d s}
renderCursorExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        {renderedExpr, html} <- renderCursorExpr (Renderer renderer) expr
        pure {i, expr, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
          ExprNode node' {dat = unCursorData node.dat} /\ {i, renderedExpr, html}
  let arrangeExprNode = ExprNode node {dat = unCursorData node.dat}
  nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

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
        Right {renderedExpr} -> [renderedExpr]
  let renderedExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ {renderedExpr, html}

renderSelectExpr :: forall ctx env r n d s.
  Lacks "elemId" d => Lacks "select" d =>
  Renderer r n d s ->
  Expr r n (SelectData d) s ->
  RenderM ctx env r n d s {renderedExpr :: Expr r n (RenderData (SelectData d)) s, html :: Html r n d s}
renderSelectExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        {renderedExpr, html} <- renderSelectExpr (Renderer renderer) expr
        pure {i, expr, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
          ExprNode node' {dat = unSelectData node.dat} /\ {i, renderedExpr, html}
  let arrangeExprNode = ExprNode node {dat = unSelectData node.dat}
  nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

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
        Right {renderedExpr} -> [renderedExpr]
  let renderedExpr = Expr 
        { node: ExprNode node {dat = R.insert (Proxy :: Proxy "elemId") elemId node.dat}
        , kids: renKids }

  pure $ {renderedExpr, html}
