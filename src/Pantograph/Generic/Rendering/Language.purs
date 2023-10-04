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

renderExpr :: forall ctx env r n d.
  Lacks "elemId" d =>
  Renderer ctx env r n d ->
  Expr r n d (Sort n d) ->
  RenderM ctx env r n d {renderedExpr :: Expr r n (RenderData d) (Sort n d), html :: Html r n d}
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
        { node: ExprNode node {d = R.insert (Proxy :: Proxy "elemId") elemId node.d}
        , kids: renKids }

  pure $ {renderedExpr, html}

renderCursorExpr :: forall ctx env r n d.
  Lacks "elemId" d => Lacks "cursor" d =>
  Renderer ctx env r n d ->
  Expr r n (CursorData d) (Sort n d) ->
  RenderM ctx env r n d {renderedExpr :: Expr r n (RenderData (CursorData d)) (Sort n d), html :: Html r n d}
renderCursorExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        {renderedExpr, html} <- renderCursorExpr (Renderer renderer) expr
        pure {i, expr, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
          ExprNode node' {d = unCursorData node.d} /\ {i, renderedExpr, html}
  let arrangeExprNode = ExprNode node {d = unCursorData node.d}
  nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

  let htmls = nodes # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let cursorClasses = case node.d.cursor of
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
        { node: ExprNode node {d = R.insert (Proxy :: Proxy "elemId") elemId node.d}
        , kids: renKids }

  pure $ {renderedExpr, html}

renderSelectExpr :: forall ctx env r n d.
  Lacks "elemId" d => Lacks "select" d =>
  Renderer ctx env r n d ->
  Expr r n (SelectData d) (Sort n d) ->
  RenderM ctx env r n d {renderedExpr :: Expr r n (RenderData (SelectData d)) (Sort n d), html :: Html r n d}
renderSelectExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  let elemId = HU.freshElementId unit

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
        {renderedExpr, html} <- renderSelectExpr (Renderer renderer) expr
        pure {i, expr, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
          ExprNode node' {d = unSelectData node.d} /\ {i, renderedExpr, html}
  let arrangeExprNode = ExprNode node {d = unSelectData node.d}
  nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

  let htmls = nodes # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let selectClasses = case node.d.select of
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
        { node: ExprNode node {d = R.insert (Proxy :: Proxy "elemId") elemId node.d}
        , kids: renKids }

  pure $ {renderedExpr, html}
