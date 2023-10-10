module Pantograph.Generic.Rendering.Editor where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tree (class PrettyTreeNode)
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

editorComponent :: forall sn el ctx env. Show sn => Show el => PrettyTreeNode el => H.Component EditorQuery (EditorInput sn el ctx env) EditorOutput Aff
editorComponent = HK.component \{slotToken} (EditorInput input) -> HK.do
  let Renderer renderer = input.renderer
  let Language language = renderer.language

  -- keyboard

  useKeyboardEffect \keyboardEvent -> do
      let ki = getKeyInfo keyboardEvent
      
      Console.log $ "[editorComponent.handleKeyboardEvent] " <>  showKeyInfo ki

      when (shouldPreventDefault ki) $
        liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent keyboardEvent

      if false then pure unit
      else if ki.ctrl && ki.key == "`" then HK.tell slotToken (Proxy :: Proxy "terminal") unit $ ToggleOpenTerminal Nothing
      else HK.tell slotToken (Proxy :: Proxy "buffer") unit $ KeyboardEventBufferQuery keyboardEvent

  -- buffer

  let bufferHtml = do
        let bufferInput = BufferInput 
              { name: "Main"
              , renderer: Renderer renderer
              , expr: fromJust' "defaultTopExpr" $ defaultTopExpr (Language language) }
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
      [ HP.classes [HH.ClassName "Panel Editor"] ]
      [ HH.div 
          [HP.classes [HH.ClassName "PanelHeader"]]
          [ HH.div [HP.classes [HH.ClassName "title"]] [HH.text $ "Pantograph"]
          , HH.div [HP.classes [HH.ClassName "info"]] [HH.text $ rendererFullName (Renderer renderer)]
          ]
      , HH.div
          [HP.classes [HH.ClassName "PanelContent"]]
          [ bufferHtml
          , terminalHtml ]
      ]

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

