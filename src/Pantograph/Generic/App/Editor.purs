module Pantograph.Generic.App.Editor (editorComponent) where

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
import Halogen (liftEffect)
import Halogen.Elements as El
import Halogen.HTML (slot) as HH
import Halogen.Hooks as HK
import Pantograph.Generic.Rendering.Hook.Keyboard as Keyboard
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent

editorComponent :: forall sn el ctx env. Dynamics sn el ctx env => EditorComponent sn el ctx env
editorComponent = HK.component \{slotToken} (EditorInput input) -> Debug.trace "[render:editor]" \_ -> HK.do

  -- keyboard

  Keyboard.useKeyboardEffect \keyboardEvent -> do
      let ki = Keyboard.getKeyInfo keyboardEvent

      terminalIsFocused <- map (fromJust' "editorComponent.useKeyboardEffect") $
        request slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "get inputIsFocused")
      
      if false then pure unit
      else if ki.mods.ctrl && ki.key == "`" then tell slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "toggle isOpen") Nothing
      else if terminalIsFocused then pure unit else do
        when (shouldPreventDefault ki) $
          liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent keyboardEvent

        if false then pure unit
        else tell slotToken (Proxy :: Proxy "buffer") unit BufferQuery (Proxy :: Proxy "keyboard") keyboardEvent

  -- buffer

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

