module Pantograph.Generic.Rendering.Hook.Keyboard where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes

useKeyboardEffect :: forall m. MonadEffect m => (KeyboardEvent -> HK.HookM m Unit) -> HK.Hook m HK.UseEffect Unit
useKeyboardEffect handleKeyboardEvent = HK.useLifecycleEffect do
  -- initialize
  document <- liftEffect $ HTML.window >>= Window.document
  keyboardSubId <- HK.subscribe do 
    HQ.eventListener
      EventTypes.keydown
      (HTMLDocument.toEventTarget document)
      (KeyboardEvent.fromEvent >>> map handleKeyboardEvent)

  pure $ Just do
    -- finalize
    HK.unsubscribe keyboardSubId
