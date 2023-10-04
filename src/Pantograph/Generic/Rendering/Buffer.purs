module Pantograph.Generic.Rendering.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Prim.Row (class Lacks)

bufferComponent = HK.component \{queryToken} (BufferInput input) -> HK.do
  -- state
  buffer /\ bufferStateId <- HK.useState $ TopBuffer input.expr
  ctx /\ ctxStateId <- HK.useState $ input.ctx
  env /\ envStateId <- HK.useState $ input.env

  -- query
  HK.useQuery queryToken case _ of
    SetBuffer buffer' a -> do
      HK.modify_ bufferStateId (const buffer')
      pure $ Just a

  -- render
  HK.pure $ do
    let bufferHtml = runRenderM ctx env $ renderBuffer input.renderer buffer
    HH.div 
      [HP.classes [HH.ClassName "Buffer"]]
      [bufferHtml]

renderBuffer :: forall ctx env r n d s.
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  Renderer r n d s ->
  Buffer r n d s ->
  RenderM ctx env r n d s (Html r n d s)
renderBuffer renderer = case _ of
  TopBuffer expr -> _.html <$> renderExpr renderer expr
  CursorBuffer cursorExpr -> _.html <$> renderCursorExpr renderer cursorExpr
  SelectBuffer selectExpr -> _.html <$> renderSelectExpr renderer selectExpr