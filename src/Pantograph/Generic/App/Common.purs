module Pantograph.Generic.App.Common where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Halogen.Elements as El
import Halogen.HTML as HH

makePanel :: forall w i.
  { content :: Array (HH.HTML w i)
  , control :: Array (HH.HTML w i)
  , info :: Array (HH.HTML w i)
  , className :: El.ClassName
  } -> 
  HH.HTML w i
makePanel {className, info, control, content} =
  El.ℓ [El.Classes [className]]
    [ El.ℓ [El.Classes [El.PanelHeader]]
        [ El.ℓ [El.Classes [El.PanelHeaderInfo]] info
        , El.ℓ [El.Classes [El.PanelHeaderControl]] control
        ]
    , El.ℓ [El.Classes [El.PanelContent]] $ Array.singleton $
        El.ℓ [El.Classes [El.PanelContentInterior]]
          content
    ]

-- hole :: forall w i.
--   { ann :: Maybe (HH.HTML w i)
--   , index :: Maybe (HH.HTML w i) } ->
--   HH.HTML w i
-- hole {index, ann} = 
--   El.ℓ [El.Classes [El.Hole]] $
--     [El.text "□"] <>
--     (index # maybe [] (\i -> [HH.sub_ [i]]))

