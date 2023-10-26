module Pantograph.Generic.Rendering.Html where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

panel {name, info, control, content} =
  HH.div [HP.classes [HH.ClassName "Panel", HH.ClassName name]]
    [ HH.div [HP.classes [HH.ClassName "PanelHeader"]]
        [ HH.div [HP.classes [HH.ClassName "PanelHeaderInfo"]] info
        , HH.div [HP.classes [HH.ClassName "PanelHeaderControl"]] control
        ]
    , HH.div [HP.classes [HH.ClassName "PanelContent"]] $ Array.singleton $
        HH.div [HP.classes [HH.ClassName "PanelContentInterior"]]
          content
    ]

whitespace str = HH.span [HP.classes [HH.ClassName "Whitespace"]] [HH.text str]

hole {index, ann} = 
  HH.span [HP.classes [HH.ClassName "Hole"]] $
    [HH.text "□"] <>
    (index # maybe [] (\i -> [HH.sub_ [i]]))


