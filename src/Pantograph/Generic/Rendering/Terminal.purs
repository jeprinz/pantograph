module Pantograph.Generic.Rendering.Terminal where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude

import Data.Foldable (length, null)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (freshElementId)
import Halogen.Utilities as HU
import Pantograph.Generic.Rendering.Html as HH
import Util (fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent as MouseEvent

maximumTerminalItems = 20

terminalComponent :: H.Component TerminalQuery TerminalInput TerminalOutput Aff
terminalComponent = HK.component \{queryToken} (TerminalInput input) -> HK.do
  let terminalInputRefLabel = RefLabel "TerminalInput"

  -- state
  items /\ itemsStateId <- HK.useState (mempty :: List TerminalItem)
  isOpen /\ isOpenStateId <- HK.useState true

  let toggleOpenTerminal mb_isOpen = do
        case mb_isOpen of
          Nothing -> HK.modify_ isOpenStateId not
          Just b -> HK.modify_ isOpenStateId (const b)

  _ /\ terminalInputIsFocusedRef <- HK.useRef false

  -- query
  HK.useQuery queryToken case _ of
    WriteTerminal item a -> do
      HK.modify_ itemsStateId (List.take maximumTerminalItems <<< List.Cons item)
      pure (Just a)
    ToggleOpenTerminal mb_isOpen a -> do
      toggleOpenTerminal mb_isOpen
      pure (Just a)
    GetFocusedTerminal k -> do
      b <- liftEffect $ Ref.read terminalInputIsFocusedRef
      pure (Just (k b))

  -- render
  HK.pure $ HH.panel
    { name: "TerminalPanel"
    , info:
        [ HH.div [HP.classes [HH.ClassName "subtitle"]] [HH.text $ "Terminal"] ]
    , control:
        [ if isOpen then
            HH.div 
              [ HP.classes [HH.ClassName "button"]
              , HP.ref terminalInputRefLabel
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
        ]
    , content:
        [ HH.div [HP.classes $ [HH.ClassName "TerminalContent"] <> if not isOpen then [HH.ClassName "closed"] else []]
            [ HH.div [HP.classes [HH.ClassName "TerminalInput"]]
                [ HH.input 
                    [ HP.ref terminalInputRefLabel
                    , HE.onFocusIn \_ -> liftEffect $ Ref.write true terminalInputIsFocusedRef
                    , HE.onFocusOut \_ -> liftEffect $ Ref.write false terminalInputIsFocusedRef ]
                ]
            , HH.fromPlainHTML $
              HH.div [HP.classes [HH.ClassName "TerminalItems"]]
                (List.toUnfoldable items <#> \(TerminalItem item) -> do
                  HH.div
                    [HP.classes [HH.ClassName "TerminalItem"]]
                    [ renderTag item.tag
                    , HH.div [HP.classes [HH.ClassName "TerminalItemContent"]] [item.html] ])
            ]
        ]
    }

renderTag :: TerminalItemTag -> HH.PlainHTML
renderTag = case _ of
  DebugTerminalItemTag -> HH.div [HP.classes [HH.ClassName "TerminalItemTag DebugTerminalItemTag"]] [HH.span_ [HH.text "debug"]]
