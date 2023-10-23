module Pantograph.Generic.Rendering.Editor where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tree (class PrettyTreeNode)
import Data.Variant (case_, inj, on)
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
import Pantograph.Generic.Rendering.Html as HH
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

editorComponent :: forall sn el ctx env. Rendering sn el ctx env => EditorComponent sn el ctx env
editorComponent = HK.component \{slotToken} (EditorInput input) -> HK.do

  -- keyboard

  useKeyboardEffect \keyboardEvent -> do
      let ki = getKeyInfo keyboardEvent
      -- Console.log $ "[editorComponent.handleKeyboardEvent] " <>  showKeyInfo ki

      terminalIsFocused <- map (fromJust' "editorComponent.useKeyboardEffect") $
        request slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "get inputIsFocused")
      
      if false then pure unit
      else if ki.ctrl && ki.key == "`" then tell slotToken (Proxy :: Proxy "terminal") unit TerminalQuery (Proxy :: Proxy "toggle isOpen") Nothing
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

  HK.pure $ HH.panel
    { name: "EditorPanel"
    , info: 
        [ HH.div [HP.classes [HH.ClassName "title"]] [HH.text $ "Pantograph"]
        ]
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

