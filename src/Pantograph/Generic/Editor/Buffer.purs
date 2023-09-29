module Pantograph.Generic.Editor.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Editor.Common
import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

bufferComponent :: forall ctx env rule joint tooth. 
  IsEditor rule joint tooth =>
  Lacks "depth" ctx => Lacks "bufferId" ctx => Lacks "outputToken" ctx => 
  Lacks "holeCount" env =>
  H.Component (BufferQuery rule joint tooth) (BufferInput ctx env rule joint tooth) (BufferOutput rule joint tooth) Aff
bufferComponent = HK.component \token (BufferInput input) -> HK.do
  buffer /\ bufferId <- HK.useState input.buffer

  ctx /\ ctxId <- HK.useState
    ( R.insert (Proxy :: Proxy "depth") 0 
    $ R.insert (Proxy :: Proxy "bufferId") bufferId
    $ R.insert (Proxy :: Proxy "outputToken") token.outputToken
    $ input.ctx
    :: RenderingCtx ctx env rule joint tooth )

  env /\ envId <- HK.useState
    ( R.insert (Proxy :: Proxy "holeCount") 0
    $ input.env
    :: RenderingEnv ctx env rule joint tooth )

  HK.pure $ runRenderingM ctx env $ do
    html <- renderBuffer buffer
    pure $
      HH.div [HU.classNames ["Buffer"]]
        [html]

renderBuffer :: forall ctx env rule joint tooth. IsEditor rule joint tooth =>
  Buffer rule joint tooth ->
  RenderingM ctx env rule joint tooth (EditorHtml rule joint tooth)
renderBuffer (TopBuffer expr) = hole "TODO"
renderBuffer (CursorBuffer cursorExpr) = hole "TODO"
renderBuffer (SelectBuffer selectExpr) = hole "TODO"