module Pantograph.Generic.App.Editor (editorComponent) where

import Data.Tuple.Nested
import Pantograph.Generic.App.Buffer
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Terminal
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Util

import Data.Maybe (Maybe(..))
import Data.Variant (case_, on)
import Debug as Debug
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen.Elements as El
import Halogen.HTML (slot) as HH
import Halogen.Hooks as HK
import Halogen.KeyInfo as KeyInfo
import Pantograph.Generic.Rendering.Hook.Keyboard as Keyboard
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent

editorComponent :: forall sn el ctx env. Dynamics sn el ctx env => EditorComponent sn el ctx env
editorComponent = HK.component \{slotToken} (EditorInput input) -> Debug.trace "[render:editor]" \_ -> HK.do

  _ /\ clipboardRef <- HK.useRef (Nothing :: Maybe (Clipboard sn el))

  -- keyboard

  Keyboard.useKeyboardEffect \keyboardEvent -> do
      let ki = KeyInfo.getKeyInfo keyboardEvent

      if false then pure unit
      -- Ctrl+`: toggle Terminal openc/closed
      else if ki.mods.ctrl && ki.key == "`" then 
        tell slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "toggle isOpen") Nothing
      -- Cmd+c: copy
      else if ki.mods.cmd && ki.key == "c" then
        request slotToken (Proxy :: Proxy "buffer") unit BufferQuery (Proxy :: Proxy "copy") >>= case _ of
          Nothing -> pure unit
          Just clipboard -> liftEffect $ Ref.write (Just clipboard) clipboardRef
      -- Cmd+x: cut
      else if ki.mods.cmd && ki.key == "x" then
        request slotToken (Proxy :: Proxy "buffer") unit BufferQuery (Proxy :: Proxy "cut") >>= case _ of
          Nothing -> pure unit
          Just clipboard -> liftEffect $ Ref.write (Just clipboard) clipboardRef
      -- Cmd+v: paste
      else if ki.mods.cmd && ki.key == "v" then do
        mb_clipboard <- liftEffect $ Ref.read clipboardRef
        case mb_clipboard of
          Nothing -> pure unit
          Just clipboard -> tell slotToken (Proxy :: Proxy "buffer") unit BufferQuery (Proxy :: Proxy "paste") clipboard
      -- forward to active buffer
      else
        tell slotToken (Proxy :: Proxy "buffer") unit BufferQuery (Proxy :: Proxy "keyboard") keyboardEvent

  let bufferHtml = do
        let bufferInput = BufferInput 
              { name: "Main"
              , expr: fromJust' "defaultTopExpr" (defaultTopExpr :: Maybe (Expr sn el)) }
        let bufferHandler (BufferOutput output) = (output # _) $ case_
              # on (Proxy :: Proxy "write terminal") \item -> tell slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "write") item
        HH.slot (Proxy :: Proxy "buffer") unit bufferComponent bufferInput bufferHandler

  -- terminal

  let terminalHtml = do
        let terminalInput = TerminalInput {}
        let terminalHandler = absurd
        HH.slot (Proxy :: Proxy "terminal") unit terminalComponent terminalInput terminalHandler

  -- render

  HK.pure $ makePanel
    { className: El.EditorPanel
    , info:
        [ El.ℓ [El.Classes [El.Title]] [El.text "Pantograph"] ]
    , control:
        []
    , content:
        [ bufferHtml
        , terminalHtml ]
    }

-- KeyInfo

shouldPreventDefault :: _ -> Boolean
shouldPreventDefault ki =
  -- blacklist
  -- Array.all not
  --   [ ki.cmd && ki.key == "Tab"
  --   , ki.cmd && ki.key == "r"
  --   , (ki.cmd || ki.alt) && ki.key == "i"
  --   ]
  false

