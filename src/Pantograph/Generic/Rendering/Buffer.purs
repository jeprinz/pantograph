module Pantograph.Generic.Rendering.Buffer where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Pantograph.Generic.Rendering.Language (renderExpr)

bufferComponent = HK.component \{queryToken} (BufferInput input) -> HK.do
  -- state
  buffer /\ bufferStateId <- HK.useState $ TopBuffer input.expr

  -- query
  HK.useQuery queryToken case _ of
    SetBuffer buffer' a -> do
      HK.modify_ bufferStateId (const buffer')
      pure $ Just a

  -- render
  HK.pure $
    HH.div [] [HH.text "<buffer>"]

renderBuffer renderer = case _ of
  TopBuffer expr -> renderExpr renderer expr
  CursorBuffer cursorExpr -> renderCursorExpr renderer cursorExpr
  SelectBuffer selectExpr -> renderSelectExpr renderer selectExpr