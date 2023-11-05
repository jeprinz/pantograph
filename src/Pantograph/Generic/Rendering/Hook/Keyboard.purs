module Pantograph.Generic.Rendering.Hook.Keyboard where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
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

-- KeyInfo

type KeyInfo = 
  { mods :: KeyMods
  , key :: String 
  -- | `(point == Just p) <==> (key == [p])`.
  , point :: Maybe CodePoint
  }

type KeyMods = 
  { alt :: Boolean
  , ctrl :: Boolean
  , meta :: Boolean
  , shift :: Boolean
  -- | `cmd <==> ctrl || meta`
  , cmd :: Boolean
  -- | `special <==> alt || cmd || ctrl || meta`
  , special :: Boolean }

showKeyInfo :: KeyInfo -> String
showKeyInfo ki =
  (if ki.mods.ctrl then "^ " else "") <>
  (if ki.mods.alt then "⎇ " else "") <>
  (if ki.mods.meta then "⌘ " else "") <>
  (if ki.mods.shift then "⇧ " else "") <>
  ki.key

getKeyInfo :: KeyboardEvent.KeyboardEvent -> KeyInfo
getKeyInfo keyboardEvent = do
  let ctrl = KeyboardEvent.ctrlKey keyboardEvent
  let alt = KeyboardEvent.altKey keyboardEvent
  let meta = KeyboardEvent.metaKey keyboardEvent
  let shift = KeyboardEvent.shiftKey keyboardEvent
  let cmd = ctrl || meta
  let special = alt || cmd || ctrl || meta
  let key = KeyboardEvent.key keyboardEvent
  let point = join $ String.uncons key <#> \{head, tail} -> if String.null tail then Just head else Nothing
  { mods: {ctrl, alt, meta, shift, cmd, special}
  , key
  , point }
