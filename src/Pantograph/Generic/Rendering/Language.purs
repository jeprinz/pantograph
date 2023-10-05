module Pantograph.Generic.Rendering.Language where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (get)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- prerender

type PrerenderM r n d = ReaderT (PrerenderCtx r n d) Identity
type PrerenderCtx r n d =
  { cursor :: Lazy (CursorExpr r n (PrerenderData r n d) (Sort n d))
  }

prerenderExpr :: forall r n d.
  Lacks "elemId" d => Lacks "cursor" d =>
  Expr r n d (Sort n d) ->
  PrerenderM r n d (Expr r n (PrerenderData r n d) (Sort n d))
prerenderExpr (Expr {node: ExprNode node, kids}) = do
  {cursor} <- ask
  let node' = ExprNode node 
    { d = enPrerenderData {elemId: HU.freshElementId unit, cursor: Just <<< PrerenderCursor <$> cursor} node.d}
  -- TODO: prerender kids, with appropriately modified contexts
  hole "TODO"
  -- Expr 
  --   { node: ExprNode node {d = enPrerenderData {elemId: HU.freshElementId unit, cursor: hole "TODO"} node.d}
  --   , kids: prerenderExpr <$> kids }

-- render

renderExpr :: forall ctx env r n d.
  Lacks "elemId" d => Lacks "cursor" d =>
  Renderer ctx env r n d ->
  Expr r n (PrerenderData r n d) (Sort n d) ->
  RenderM ctx env r n d (BufferHtml r n)
renderExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
  ctx <- ask
  -- env <- get

  -- let elemId = HU.freshElementId unit
  let elemId = hole "TODO: get from prerender data"

  let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ renderedExpr) -> do
        html <- renderExpr (Renderer renderer) renderedExpr
        pure {i, renderedExpr, html}
  let arrangeKids = renderKids <##> 
        \{i, renderedExpr: renderedExpr'@(Expr {node: node'}), html} -> 
          node' /\ {i, renderedExpr: renderedExpr', html}
  arrangedHtmls <- 
    renderer.arrangeExpr
      (ExprNode node {d = unPrerenderData node.d})
      (arrangeKids <##> \(ExprNode renderedExprNode' /\ {renderedExpr, html}) -> 
        ExprNode renderedExprNode' {d = unPrerenderData node.d} /\ {html, renderedExpr})

  let htmls = arrangedHtmls # Array.foldMap case _ of
        Left htmls' -> htmls'
        Right {html} -> [html]
  let html = HH.div 
        [ HU.id elemId
        , HP.classes (HH.ClassName <$> ["Expr"])
        , HE.onMouseDown \mouseEvent -> do
            liftEffect $ Event.stopPropagation (MouseEvent.toEvent mouseEvent)
            HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "onMouseDown: " <> show elemId}
        ]
        htmls

  pure html

-- renderCursorExpr :: forall ctx env r n d.
--   Lacks "elemId" d => Lacks "cursor" d =>
--   Renderer ctx env r n d ->
--   Expr r n (CursorData d) (Sort n d) ->
--   RenderM ctx env r n d {renderedExpr :: Expr r n (PrerenderData (CursorData d)) (Sort n d), html :: BufferHtml r n}
-- renderCursorExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
--   let elemId = HU.freshElementId unit

--   let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
--         {renderedExpr, html} <- renderCursorExpr (Renderer renderer) expr
--         pure {i, expr, renderedExpr, html}
--   let arrangeKids = renderKids <##> 
--         \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
--           ExprNode node' {d = unCursorData node.d} /\ {i, renderedExpr, html}
--   let arrangeExprNode = ExprNode node {d = unCursorData node.d}
--   nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

--   let htmls = nodes # Array.foldMap case _ of
--         Left htmls' -> htmls'
--         Right {html} -> [html]
--   let cursorClasses = case node.d.cursor of
--         Nothing -> []
--         Just CursorStatus -> [HH.ClassName "Cursor"]
--   let html = HH.div 
--         [ HU.id elemId
--         , HP.classes ([HH.ClassName "Expr"] <> cursorClasses)
--           -- TODO: onClick ==> raise SetCursor here
--           -- TODO: onClick ==> raise SetSelect here
--         ]
--         htmls

--   let renKids = nodes # Array.foldMap case _ of
--         Left _ -> []
--         Right {renderedExpr} -> [renderedExpr]
--   let renderedExpr = Expr 
--         { node: ExprNode node {d = R.insert (Proxy :: Proxy "elemId") elemId node.d}
--         , kids: renKids }

--   pure $ {renderedExpr, html}

-- renderSelectExpr :: forall ctx env r n d.
--   Lacks "elemId" d => Lacks "select" d =>
--   Renderer ctx env r n d ->
--   Expr r n (SelectData d) (Sort n d) ->
--   RenderM ctx env r n d {renderedExpr :: Expr r n (PrerenderData (SelectData d)) (Sort n d), html :: BufferHtml r n}
-- renderSelectExpr (Renderer renderer) (Expr {node: ExprNode node, kids}) = do
--   let elemId = HU.freshElementId unit

--   let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ expr) -> do
--         {renderedExpr, html} <- renderSelectExpr (Renderer renderer) expr
--         pure {i, expr, renderedExpr, html}
--   let arrangeKids = renderKids <##> 
--         \{i, expr: Expr {node: ExprNode node'}, renderedExpr, html} -> 
--           ExprNode node' {d = unSelectData node.d} /\ {i, renderedExpr, html}
--   let arrangeExprNode = ExprNode node {d = unSelectData node.d}
--   nodes <- renderer.arrangeExpr arrangeExprNode arrangeKids

--   let htmls = nodes # Array.foldMap case _ of
--         Left htmls' -> htmls'
--         Right {html} -> [html]
--   let selectClasses = case node.d.select of
--         Nothing -> []
--         Just OuterSelectStatus -> [HH.ClassName "OuterSelect"]
--         Just InnerSelectStatus -> [HH.ClassName "InnerSelect"]
--   let html = HH.div 
--         [ HU.id elemId
--         , HP.classes ([HH.ClassName "Expr"] <> selectClasses)
--           -- TODO: onClick ==> raise SetCursor here
--           -- TODO: onClick ==> raise SetSelect here
--         ]
--         htmls

--   let renKids = nodes # Array.foldMap case _ of
--         Left _ -> []
--         Right {renderedExpr} -> [renderedExpr]
--   let renderedExpr = Expr 
--         { node: ExprNode node {d = R.insert (Proxy :: Proxy "elemId") elemId node.d}
--         , kids: renKids }

--   pure $ {renderedExpr, html}
