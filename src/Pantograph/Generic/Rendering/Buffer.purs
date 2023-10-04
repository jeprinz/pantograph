module Pantograph.Generic.Rendering.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Engine engine = input.engine
  let Renderer renderer = engine.renderer
  -- state
  buffer /\ bufferStateId <- HK.useState $ TopBuffer input.expr
  ctx /\ ctxStateId <- HK.useState 
    $ R.insert (Proxy :: Proxy "depth") 0
    $ R.insert (Proxy :: Proxy "outputToken") outputToken
    $ renderer.topCtx
  env /\ envStateId <- HK.useState
    $ R.insert (Proxy :: Proxy "holeCount") 0
    $ renderer.topEnv

  -- query
  HK.useQuery queryToken case _ of
    SetBuffer buffer' a -> do
      HK.modify_ bufferStateId (const buffer')
      pure $ Just a

  -- render
  HK.pure $ do
    let bufferHtml = runRenderM ctx env $ renderBuffer (Renderer renderer) buffer
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
          [bufferHtml]
      ]

renderBuffer :: forall ctx env r n d.
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  Renderer ctx env r n d ->
  Buffer r n d ->
  RenderM ctx env r n d (Html r n d)
renderBuffer renderer = case _ of
  TopBuffer expr -> _.html <$> renderExpr renderer expr
  CursorBuffer cursorExpr -> _.html <$> renderCursorExpr renderer cursorExpr
  SelectBuffer selectExpr -> _.html <$> renderSelectExpr renderer selectExpr