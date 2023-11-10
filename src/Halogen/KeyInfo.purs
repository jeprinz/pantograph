module Halogen.KeyInfo where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Web.UIEvent.KeyboardEvent as KeyboardEvent

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
