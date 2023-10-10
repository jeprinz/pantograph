module Pantograph.Generic.Rendering.Terminal where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

maximumTerminalItems = 20

terminalComponent = HK.component \{queryToken} (TerminalInput input) -> HK.do

  -- state
  items /\ itemsStateId <- HK.useState (mempty :: List TerminalItem)
  isOpen /\ isOpenStateId <- HK.useState true

  let toggleOpenTerminal mb_isOpen = do
        case mb_isOpen of
          Nothing -> HK.modify_ isOpenStateId not
          Just b -> HK.modify_ isOpenStateId (const b)

  -- query
  HK.useQuery queryToken case _ of
    WriteTerminal item a -> do
      HK.modify_ itemsStateId (List.take maximumTerminalItems <<< List.Cons item)
      pure (Just a)
    ToggleOpenTerminal mb_isOpen a -> do
      toggleOpenTerminal mb_isOpen
      pure (Just a)

  -- render
  HK.pure $ 
    HH.div
      [HP.classes [HH.ClassName "Panel Terminal"]] $
      [ HH.div
          [HP.classes [HH.ClassName "PanelHeader"]]
          [ if isOpen then
              HH.div 
                [ HP.classes [HH.ClassName "button"]
                , HE.onClick \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    toggleOpenTerminal (Just false) ]
                [HH.text "↓"]
            else
              HH.div 
                [ HP.classes [HH.ClassName "button"]
                , HE.onClick \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    toggleOpenTerminal (Just true)
                ]
                [HH.text "↑"]
          , HH.text "Terminal" 
          ]
      ] <>
      if not isOpen then [] else
        [ HH.div
            [HP.classes [HH.ClassName "PanelContent"]]
            [HH.fromPlainHTML $ HH.div
                [HP.classes [HH.ClassName "TerminalItems"]]
                (List.toUnfoldable items <#> \(TerminalItem item) -> do
                  HH.div
                    [HP.classes [HH.ClassName "TerminalItem"]]
                    [ renderTag item.tag
                    , HH.div [HP.classes [HH.ClassName "TerminalItemContent"]] [item.html] ])]
        ]

renderTag :: TerminalItemTag -> HH.PlainHTML
renderTag = case _ of
  DebugTerminalItemTag -> HH.div [HP.classes [HH.ClassName "TerminalItemTag DebugTerminalItemTag"]] [HH.text "debug"]
