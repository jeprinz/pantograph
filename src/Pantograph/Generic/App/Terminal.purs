module Pantograph.Generic.App.Terminal (terminalComponent) where

import Data.Tuple.Nested
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Common
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Data.Display (Html, embedHtml)
import Data.Foldable (length, null)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Variant (case_, on)
import Debug as Debug
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.Elements as El
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (freshElementId)
import Halogen.Utilities as HU
import Pantograph.Generic.App.Common as HH
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Rendering.Hook.Timer as Timer
import Type.Proxy (Proxy(..))
import Util (fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent as MouseEvent

terminalComponent :: H.Component TerminalQuery TerminalInput TerminalOutput Aff
terminalComponent = HK.component \{queryToken} (TerminalInput input) -> Debug.trace "[render:terminal]" \_ -> HK.do
  let terminalInputRefLabel = RefLabel "TerminalInput"

  let items = GMB.getGlobalMessages unit

  _ /\ dummyBitStateId <- HK.useState false
  let forceUpdate = HK.modify_ dummyBitStateId not

  isOpen /\ isOpenStateId <- HK.useState true

  let toggleOpenTerminal mb_isOpen = do
        case mb_isOpen of
          Nothing -> HK.modify_ isOpenStateId not
          Just b -> HK.modify_ isOpenStateId (const b)

  _ /\ terminalInputIsFocusedRef <- HK.useRef false

  HK.useQuery queryToken \(TerminalQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "write") (\(item /\ a) -> do
        GMB.modifyGlobalMessages (List.Cons item) \_ -> do
          forceUpdate
          pure (Just a)
      )
    # on (Proxy :: Proxy "toggle isOpen") (\(mb_isOpen /\ a) -> do
        toggleOpenTerminal mb_isOpen
        pure (Just a)
      )
    # on (Proxy :: Proxy "get inputIsFocused") (\k -> do
        b <- liftEffect $ Ref.read terminalInputIsFocusedRef
        pure (Just (k b))        
      )

  Timer.useTimer (Milliseconds 1000.0) forceUpdate

  -- render
  HK.pure $ makePanel
    { className: El.TerminalPanel
    , info:
        [ El.ℓ [El.Classes [El.Subtitle]] [El.τ $ "Terminal"] ]
    , control:
        [ if isOpen then
            El.ℓ
              [ El.Classes [El.Button]
              , El.OnMouseDown \mouseEvent -> do
                  liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                  toggleOpenTerminal (Just false) ]
              [El.τ "↓"]
          else
            El.ℓ 
              [ El.Classes [El.Button]
              , El.OnMouseDown \mouseEvent -> do
                  liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                  toggleOpenTerminal (Just true)
              ]
              [El.τ "↑"] 
        ] <>
        [
          El.ℓ
              [ El.Classes [El.Button]
              , El.OnMouseDown \mouseEvent -> do
                  liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                  GMB.modifyGlobalMessages mempty mempty ]
              [El.τ "×"]
        ]
    , content:
        [ El.ℓ [El.Classes $ [El.TerminalContent] <> if not isOpen then [El.Closed] else []]
            [ 
              embedHtml mempty $
              El.ℓ [El.Classes [El.GlobalMessages]]
                (List.toUnfoldable items <#> \(GMB.GlobalMessage item) -> do
                  El.ℓ [El.Classes [El.GlobalMessage]]
                    [ renderTag item.tag
                    , El.ℓ [El.Classes [El.GlobalMessageContent]] [item.html] ])
            ]
        ]
    }

renderTag :: GMB.GlobalMessageTag -> Html
renderTag = case _ of
  GMB.DebugGlobalMessageTag -> El.ℓ [El.Classes [El.DebugGlobalMessageTag]] [El.τ "debug"]
  GMB.ErrorGlobalMessageTag -> El.ℓ [El.Classes [El.ErrorGlobalMessageTag]] [El.τ "error"]
  GMB.InfoGlobalMessageTag -> El.ℓ [El.Classes [El.InfoGlobalMessageTag]] [El.τ "info"]
