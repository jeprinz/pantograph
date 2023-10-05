module Pantograph.Generic.Rendering.Buffer where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Control.Monad.Reader (ask, local)
import Data.Maybe (Maybe(..))
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

bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  gyro /\ gyroStateId <- HK.useState $ RootGyro input.expr
  syncGyro /\ syncGyroRef <- HK.useRef $ syncGyro gyro

  let 
    updateSyncGyro :: (_ -> HK.HookM Aff _) -> HK.HookM Aff Unit
    updateSyncGyro f = do
        syncGyro_old <- liftEffect $ Ref.read syncGyroRef
        syncGyro_new <- f syncGyro_old
        liftEffect $ Ref.write syncGyro_new syncGyroRef
        let ctx = 
              { gyroPosition: case syncGyro_old of
                  RootGyro _ -> InsideRoot
                  CursorGyro _ -> OutsideCursor
                  SelectGyro _ -> OutsideSelect }
        let env = {}
        liftAff $ runHydrateM ctx env $ hydrateGyro syncGyro_new

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

syncGyro :: forall el ed sn.
  ExprGyro el ed sn ->
  ExprGyro el (SyncExprData ed) sn
syncGyro = map \(ExprNode node) -> ExprNode node {dat = R.union {elemId: HU.freshElementId unit} node.dat}

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
  AtTopSelect -> BetweenSelect
  BetweenSelect -> BetweenSelect
  AtBotSelect -> InsideSelect
  InsideSelect -> InsideSelect

hydrateExpr :: forall el ed sn. Expr el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateExpr (Tree tree) = do
  hydrateExprNode tree.node
  local (\ctx -> ctx {gyroPosition = kidGyroPosition ctx.gyroPosition}) $ 
    hydrateExpr `traverse_` tree.kids

hydrateCursor :: forall el ed sn. ExprCursor el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateCursor (Cursor cursor) = hole "TODO: hydrateCursor"

hydrateSelect :: forall el ed sn. ExprSelect el (SyncExprData ed) sn -> HydrateM el ed sn Unit
hydrateSelect select = hole "TODO: hydrateSelect"

renderGyro :: forall ctx env el ed sn.
  Renderer ctx env el ed sn ->
  ExprGyro el (SyncExprData ed) sn ->
  RenderM ctx env el ed sn (BufferHtml el ed sn)
renderGyro = hole "TODO: renderGyro"
