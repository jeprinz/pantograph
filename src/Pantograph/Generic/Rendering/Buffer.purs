module Pantograph.Generic.Rendering.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Data.Maybe (Maybe(..))
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
  let Engine engine = input.engine
  let Renderer renderer = engine.renderer

  buffer /\ bufferStateId <- HK.useState $ TopBuffer input.expr
  prerenderedBuffer /\ prerenderedBufferRef <- HK.useRef $ prerenderBuffer buffer

  let setBuffer buffer' = do 
        processBuffer buffer'
        liftEffect $ Ref.write buffer' prerenderedBufferRef

  ctx /\ ctxStateId <- HK.useState $ enRenderCtx {depth: 0, outputToken, setBuffer} renderer.topCtx
  env /\ envStateId <- HK.useState $ enRenderEnv {holeCount: 0} renderer.topEnv

  -- query
  HK.useQuery queryToken case _ of
    SetBuffer buffer' a -> do
      HK.modify_ bufferStateId (const buffer')
      pure $ Just a

  -- render
  HK.pure $ do
    let bufferHtml = runRenderM ctx env $ renderBuffer (Renderer renderer) prerenderedBuffer
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

-- prerender

prerenderBuffer :: forall r n d.
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  Buffer r n d (Sort n d) -> Buffer r n (PrerenderData d) (Sort n d)
prerenderBuffer = case _ of
  TopBuffer expr -> TopBuffer $ prerenderExpr expr
  _ -> hole "TODO"

-- render

renderBuffer :: forall ctx env r n d.
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  Renderer ctx env r n d ->
  Buffer r n (PrerenderData d) (Sort n d) ->
  RenderM ctx env r n d (BufferHtml r n)
renderBuffer renderer = case _ of
  TopBuffer expr -> renderExpr renderer expr
  CursorBuffer cursorExpr -> hole "TODO: render CursorBuffer" -- _.html <$> renderCursorExpr renderer cursorExpr
  SelectBuffer selectExpr -> hole "TODO: render SelectBuffer" -- _.html <$> renderSelectExpr renderer selectExpr

-- process

processBuffer :: forall r n d.
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  Buffer r n (PrerenderData d) (Sort n d) -> 
  HK.HookM Aff Unit
processBuffer = hole "TODO"