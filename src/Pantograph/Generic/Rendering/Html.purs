module Pantograph.Generic.Rendering.Html where

import Prelude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

panel {name, header, content} =
  HH.div [HP.classes [HH.ClassName "Panel", HH.ClassName name]]
    [ HH.div [HP.classes [HH.ClassName "PanelHeader"]] header
    , HH.div [HP.classes [HH.ClassName "PanelContent"]] $ Array.singleton $
        HH.div [HP.classes [HH.ClassName "PanelContentInterior"]]
          content
    ]
