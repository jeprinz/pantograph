module Pantograph.Generic.App.Terminal (terminalComponent) where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Pantograph.Generic.App.Common
import Pantograph.Generic.Dynamics
import Prelude

import Data.Display (Html, embedHtml)
import Data.Foldable (length, null)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.Elements as El
import Pantograph.Generic.App.Common
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (freshElementId)
import Halogen.Utilities as HU
import Pantograph.Generic.Rendering.TerminalItems (TerminalItemTag)
import Pantograph.Generic.Rendering.TerminalItems as TI
import Pantograph.Generic.App.Common as HH
import Type.Proxy (Proxy(..))
import Util (fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent as MouseEvent

maximumTerminalItems = 20

terminalComponent :: H.Component TerminalQuery TerminalInput TerminalOutput Aff
terminalComponent = HK.component \{queryToken} (TerminalInput input) -> HK.do
  let terminalInputRefLabel = RefLabel "TerminalInput"

  let items = TI.getTerminalItems unit

  -- state
  _ /\ counterStateId <- HK.useState 0
  isOpen /\ isOpenStateId <- HK.useState true

  let toggleOpenTerminal mb_isOpen = do
        case mb_isOpen of
          Nothing -> HK.modify_ isOpenStateId not
          Just b -> HK.modify_ isOpenStateId (const b)

  _ /\ terminalInputIsFocusedRef <- HK.useRef false

  HK.useQuery queryToken \(TerminalQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "write") (\(item /\ a) -> do
        -- HK.modify_ itemsStateId (List.take maximumTerminalItems <<< List.Cons item)
        TI.modifyTerminalItems (List.take maximumTerminalItems <<< List.Cons item) \_ -> do
          HK.modify_ counterStateId (1 + _)
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

  -- render
  HK.pure $ makePanel
    { className: El.TerminalPanel
    , info:
        [ El.ℓ [El.Classes [El.Subtitle]] [El.text $ "Terminal"] ]
    , control:
        [ if isOpen then
            El.ℓ
              [ El.Classes [El.Button]
              , El.Ref terminalInputRefLabel
              , El.OnMouseDown \mouseEvent -> do
                  liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                  toggleOpenTerminal (Just false) ]
              [El.text "↓"]
          else
            El.ℓ 
              [ El.Classes [El.Button]
              , El.OnMouseDown \mouseEvent -> do
                  liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                  toggleOpenTerminal (Just true)
              ]
              [El.text "↑"] 
        ]
    , content:
        -- [ El.ℓ [El.Classes $ [HH.ClassName "TerminalContent"] <> if not isOpen then [HH.ClassName "closed"] else []]
        [ El.ℓ [El.Classes $ [El.TerminalContent] <> if not isOpen then [El.Closed] else []]
            [ 
              -- TODO: should there be a text terminal?
              --   El.ℓ [El.Classes [HH.ClassName "TerminalInput"]]
              --     [ HH.input 
              --         [ HP.ref terminalInputRefLabel
              --         , HE.onFocusIn \_ -> liftEffect $ Ref.write true terminalInputIsFocusedRef
              --         , HE.onFocusOut \_ -> liftEffect $ Ref.write false terminalInputIsFocusedRef ]
              --     ]
              -- , 
              El.ℓ 
                [ El.Classes [El.Button]
                , El.OnMouseDown \_ -> HK.modify_ counterStateId (1 + _) ]
                [ El.text "force update" ]
            ,
              embedHtml mempty $
              El.ℓ [El.Classes [El.TerminalItems]]
                (List.toUnfoldable items <#> \(TI.TerminalItem item) -> do
                  El.ℓ [El.Classes [El.TerminalItem]]
                    [ renderTag item.tag
                    , El.ℓ [El.Classes [El.TerminalItemContent]] [item.html] ])
            ]
        ]
    }

renderTag :: TerminalItemTag -> Html
renderTag = case _ of
  TI.DebugTerminalItemTag -> El.ℓ [El.Classes [El.TerminalItemTag, El.DebugTerminalItemTag]] [El.text "debug"]
