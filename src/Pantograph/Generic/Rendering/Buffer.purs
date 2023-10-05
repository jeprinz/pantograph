module Pantograph.Generic.Rendering.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tree
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Hole (hole)
import Pantograph.Generic.Language (Sort)
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  gyro /\ gyroStateId <- HK.useState $ RootGyro input.expr

  syncGyro /\ syncGyroRef <- HK.useState $ syncGyro gyro

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
syncGyro = hole "TODO: syncGyro"

renderGyro :: forall ctx env el ed sn.
  Renderer ctx env el ed sn ->
  ExprGyro el (SyncExprData ed) sn ->
  RenderM ctx env el ed sn (BufferHtml el ed sn)
renderGyro = hole "TODO: renderGyro"
