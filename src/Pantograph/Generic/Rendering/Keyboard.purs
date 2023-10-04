module Pantograph.Generic.Rendering.Keyboard where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes

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

-- KeyInfo

type KeyInfo = {alt :: Boolean, cmd :: Boolean, ctrl :: Boolean, key :: String, meta :: Boolean, shift :: Boolean}

showKeyInfo :: KeyInfo -> String
showKeyInfo ki =
  (if ki.ctrl then "^ " else "") <>
  (if ki.alt then "⎇ " else "") <>
  (if ki.meta then "⌘ " else "") <>
  (if ki.shift then "⇧ " else "") <>
  ki.key

getKeyInfo :: KeyboardEvent.KeyboardEvent -> KeyInfo
getKeyInfo keyboardEvent = do
  let ctrl = KeyboardEvent.ctrlKey keyboardEvent
  let alt = KeyboardEvent.altKey keyboardEvent
  let meta = KeyboardEvent.metaKey keyboardEvent
  let shift = KeyboardEvent.shiftKey keyboardEvent
  let cmd = ctrl || meta
  let key = KeyboardEvent.key keyboardEvent
  {ctrl, alt, meta, shift, cmd, key}
