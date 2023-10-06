module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude
import Util

import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT)
import Data.Foldable (foldM, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Pantograph.Generic.Language (Sort)
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- types

-- type M ctx env el ed sn a = ReaderT ctx (StateT env (HK.HookM Aff)) (BufferHtml el ed sn /\ a)
type M ctx env el ed sn = ReaderT (Record ctx) (StateT (Record env) (HK.HookM Aff))

type HydrateExprData ed = (gyroPosition :: GyroPosition | ed)

data ArrangeKid el ed sn a
  = ExprKidArrangeKid a
  | PunctuationArrangeKid (Array (BufferHtml el ed sn))
  | IndentationArrangeKid (Array (BufferHtml el ed sn))

-- functions

syncExprNode :: forall ctx env el ed ed' sn.
  ExprNode el ed' sn ->
  M ctx env el ed sn (ExprNode el (SyncExprData ed') sn)
syncExprNode = hole "TODO: hydrateExprNode"

hydrateExprNode :: forall ctx env el ed ed' sn.
  ExprNode el ed' sn ->
  M (HydrateCtx el ed sn ctx) (HydrateEnv el ed sn env) el ed sn (ExprNode el (HydrateExprData ed') sn)
hydrateExprNode = hole "TODO: hydrateExprNode"

setupExprKid :: forall ctx env el ed ed' sn a.
  ExprNode el ed' sn ->
  M (HydrateCtx el ed sn ctx) (HydrateEnv el ed sn env) el ed sn a ->
  M (HydrateCtx el ed sn ctx) (HydrateEnv el ed sn env) el ed sn a
setupExprKid = hole "TODO: setupExprKid"

hydrateClassName :: forall el ed sn. ExprNode el (HydrateExprData ed) sn -> HH.ClassName
hydrateClassName = hole "TODO: hydrateClassName"

-- sync, hydrate, render
renderExpr :: forall ctx env el ed sn.
  Expr el ed sn ->
  M (RenderCtx el ed sn (HydrateCtx el ed sn ctx)) (RenderEnv el ed sn (HydrateEnv el ed sn env)) el ed sn {html :: BufferHtml el ed sn, expr :: Expr el (HydrateExprData (SyncExprData ed)) sn}
renderExpr (Tree expr) = do
  hydratedExprNode :: ExprNode el (HydrateExprData (SyncExprData ed)) sn <- 
    expr.node # (syncExprNode >=> hydrateExprNode)
  
  arrangedHtmls <- do
    let 
      -- arrangeKids :: Array (M (RenderCtx el ed sn ctx) (RenderEnv el ed sn env) el ed sn ({html :: BufferHtml el ed sn, expr :: Expr el (HydrateExprData (SyncExprData ed)) sn} /\ ExprNode el (HydrateExprData (SyncExprData ed)) sn))
      arrangeKids = expr.kids <#>
        renderExpr >>>
        setupExprKid expr.node >>>
        map \renderedKid@{expr: Tree expr'} -> renderedKid /\ expr'.node
    arrangeExpr hydratedExprNode arrangeKids


  let
    htmlKids = arrangedHtmls # foldMap case _ of
      ExprKidArrangeKid {html} -> [html]
      PunctuationArrangeKid htmls -> htmls
      IndentationArrangeKid htmls -> htmls

  let
    hydratedExprKids :: Array (Expr el (HydrateExprData (SyncExprData ed)) sn)
    hydratedExprKids = arrangedHtmls # foldMap case _ of
      ExprKidArrangeKid {expr: expr'} -> [expr']
      PunctuationArrangeKid _ -> []
      IndentationArrangeKid _ -> []

  let
    html = HH.div
      [ HU.id $ (unwrap hydratedExprNode).dat.elemId
      , HP.class_ $ hydrateClassName hydratedExprNode
      , HE.onMouseDown \mouseEvent -> do
          liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
          hole "TODO: renderExpr onMouseDown"
      , HE.onMouseUp \mouseEvent -> do
          liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
          hole "TODO: renderExpr onMouseUp"
      ]
      htmlKids

  pure
    { html
    , expr: Tree {node: hydratedExprNode, kids: hydratedExprKids} }

arrangeExpr :: forall ctx env el ed ed' sn a.
  ExprNode el ed' sn ->
  Array (M (RenderCtx el ed sn ctx) (RenderEnv el ed sn env) el ed sn (a /\ ExprNode el ed' sn)) ->
  M (RenderCtx el ed sn ctx) (RenderEnv el ed sn env) el ed sn (Array (ArrangeKid el ed sn a))
arrangeExpr = hole "TODO: placeholder arrangeExpr"

{-
bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  gyro /\ gyroStateId <- HK.useState $ RootGyro input.expr
  syncGyro /\ syncGyroRef <- HK.useRef $ syncGyro gyro

  -- let 
  --   updateSyncGyro :: (_ -> HK.HookM Aff _) -> HK.HookM Aff Unit
  --   updateSyncGyro f = do
  --       syncGyro_old <- liftEffect $ Ref.read syncGyroRef
  --       syncGyro_new <- f syncGyro_old
  --       liftEffect $ Ref.write syncGyro_new syncGyroRef
  --       let ctx = 
  --             { gyroPosition: case syncGyro_old of
  --                 RootGyro _ -> InsideRoot
  --                 CursorGyro _ -> OutsideCursor
  --                 SelectGyro _ -> OutsideSelect }
  --       let env = {}
  --       runHydrateM ctx env $ hydrateGyro syncGyro_new

  ctx /\ ctxStateId <- HK.useState $ R.union {depth: 0} renderer.topCtx
  env /\ envStateId <- HK.useState $ R.union {holeCount: 0} renderer.topEnv

  -- render
  HK.pure $ do
    let gyroHtml = runRenderM ctx env $ renderGyro (Renderer renderer) syncGyro
    HH.div 
      [HP.classes [HH.ClassName "Panel Buffer"]]
      [ HH.div
          [HP.classes [HH.ClassName "PanelHeader"]]
          [ HH.div
              [ HP.classes [HH.ClassName "button"]
              , HE.onClick \_ -> Console.log "TODO: close buffer" ]
              [HH.text "X"]
          , HH.text "Buffer" ]
      , HH.div
          [HP.classes [HH.ClassName "PanelContent"]]
          [gyroHtml] ]

-- | # Sync

syncGyro :: forall el ed sn.
  ExprGyro el ed sn ->
  ExprGyro el (SyncExprData ed) sn
syncGyro = map \(ExprNode node) -> ExprNode node {dat = R.union {elemId: HU.freshElementId unit} node.dat}

-- | # Hydrate

hydrateGyro :: forall el ed sn.
  ExprGyro el (SyncExprData ed) sn ->
  HydrateM el ed sn Unit
hydrateGyro (RootGyro expr) = hydrateExpr expr
hydrateGyro (CursorGyro cursor) = hydrateCursor cursor
hydrateGyro (SelectGyro select) = hydrateSelect select

hydrateExprNode :: forall el ed sn. ExprNode el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateExprNode (ExprNode node) = do
  ctx <- ask
  liftEffect $ void $ HU.setClassName node.dat.elemId $ toClassName ctx.gyroPosition

-- | Given the parent's GyroPosition, naively computes each kid's GyroPosition.
-- | For the following transitions, need to be handled before calling the
-- | appropriate hydrate functions.
kidGyroPosition :: GyroPosition -> GyroPosition
kidGyroPosition = case _ of
  InsideRoot -> InsideRoot
  OutsideCursor -> OutsideCursor
  AtCursor -> InsideCursor
  InsideCursor -> InsideCursor
  OutsideSelect -> OutsideCursor
  AtOutsideSelect -> BetweenSelect
  BetweenSelect -> BetweenSelect
  AtInsideSelect -> InsideSelect
  InsideSelect -> InsideSelect

hydrateExpr :: forall el ed sn. Expr el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateExpr (Tree tree) = do
  hydrateExprNode tree.node
  local (\ctx -> ctx {gyroPosition = kidGyroPosition ctx.gyroPosition}) $ 
    hydrateExpr `traverse_` tree.kids

hydrateExprPath :: forall el ed sn. ExprPath el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateExprPath (Path Nil) = pure unit
hydrateExprPath (Path (Cons (Tooth tooth) ts)) = do
  hydrateExprNode tooth.node
  local (\ctx -> ctx {gyroPosition = kidGyroPosition ctx.gyroPosition}) $
    hydrateExprPath (Path ts)

hydrateCursor :: forall el ed sn. ExprCursor el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateCursor (Cursor cursor) = do
  local (\ctx -> ctx {gyroPosition = OutsideCursor}) $
    hydrateExprPath cursor.outside
  local (\ctx -> ctx {gyroPosition = AtCursor}) $
    hydrateExpr cursor.inside

hydrateSelect :: forall el ed sn. ExprSelect el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateSelect (Select select) = do
  local (\ctx -> ctx {gyroPosition = OutsideSelect}) $
    hydrateExprPath select.outside
  local (\ctx -> ctx {gyroPosition = AtOutsideSelect}) $
    hydrateExprPath select.middle
  local (\ctx -> ctx {gyroPosition = AtInsideSelect}) $
    hydrateExpr select.inside

-- # Render

renderGyro :: forall ctx env el ed sn.
  Renderer ctx env el ed sn ->
  ExprGyro el (SyncExprData ed) sn ->
  RenderM ctx env el ed sn (BufferHtml el ed sn)
renderGyro = hole "TODO: renderGyro"
-}