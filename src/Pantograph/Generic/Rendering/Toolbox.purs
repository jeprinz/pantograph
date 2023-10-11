module Pantograph.Generic.Rendering.Toolbox where

import Pantograph.Generic.Rendering.Common
import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Hole (hole)

toolboxComponent :: forall sn el ctx env. H.Component (ToolboxQuery sn el) (ToolboxInput sn el) (ToolboxOutput sn el) Aff
toolboxComponent = HK.component \{queryToken} (ToolboxInput input) -> HK.do
  HK.pure $
    HH.div [] [HH.text "<toolbox>"]
