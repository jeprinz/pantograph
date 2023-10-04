module Pantograph.Generic.Rendering.Editor where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Pantograph.Generic.Rendering.Keyboard (getKeyInfo, showKeyInfo, useKeyboardEffect)
import Pantograph.Generic.Rendering.Terminal (terminalComponent)
import Prim.Row (class Lacks)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes

editorComponent = HK.component \{slotToken} (EditorInput input) -> HK.do
  let Editor editor = input.editor
  let Language language = editor.language

  -- keyboard

  useKeyboardEffect \keyboardEvent -> do
      let ki = getKeyInfo keyboardEvent
      
      Console.log $ "[editorComponent.handleKeyboardEvent] " <>  showKeyInfo ki

      when (shouldPreventDefault ki) $
        liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent keyboardEvent

      if false then pure unit
      else if ki.ctrl && ki.key == "`" then HK.tell slotToken (Proxy :: Proxy "terminal") unit $ ToggleOpenTerminal Nothing
      else pure unit

      HK.tell slotToken (Proxy :: Proxy "terminal") unit $ WriteTerminal $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "key = " <> show ki.key}

  -- buffer

  let bufferHtml = do
        let bufferInput = BufferInput 
              { expr: topExpr (Language language)
              , editor: Editor editor }
        let bufferHandler = case _ of
              WriteTerminalFromBuffer item -> do
                HK.tell slotToken (Proxy :: Proxy "terminal") unit (WriteTerminal item)
        HH.slot (Proxy :: Proxy "buffer") unit bufferComponent bufferInput bufferHandler

  -- terminal

  let terminalHtml = do
        let terminalInput = TerminalInput {}
        let terminalHandler = absurd
        HH.slot (Proxy :: Proxy "terminal") unit terminalComponent terminalInput terminalHandler

  -- render

  HK.pure $
    HH.div
      [ HP.classes [HH.ClassName "Editor"] ]
      [ bufferHtml
      , terminalHtml
      ]

-- KeyInfo

shouldPreventDefault :: _ -> Boolean
shouldPreventDefault ki =
  -- blacklist
  Array.all not
    [ ki.cmd && ki.key == "Tab"
    , ki.cmd && ki.key == "r" ]

