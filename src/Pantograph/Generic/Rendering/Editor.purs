module Pantograph.Generic.Rendering.Editor where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Pantograph.Generic.Rendering.Terminal (terminalComponent)
import Prim.Row (class Lacks)
import Type.Proxy (Proxy(..))

editorComponent = HK.component \{slotToken} (EditorInput input) -> HK.do
  let Editor editor = input.editor
  let Language language = editor.language

  let bufferHtml = do
        let bufferInput = BufferInput 
              { expr: topExpr (Language language)
              , editor: Editor editor }
        let bufferHandler = case _ of
              WriteTerminalFromBuffer item -> do
                HK.tell slotToken (Proxy :: Proxy "Terminal") unit (WriteTerminal item)
        HH.slot (Proxy :: Proxy "buffer") unit bufferComponent bufferInput bufferHandler

  let terminalHtml = do
        let terminalInput = TerminalInput {}
        let terminalHandler = absurd
        HH.slot (Proxy :: Proxy "Terminal") unit terminalComponent terminalInput terminalHandler

  HK.pure $
    HH.div
      [ HP.classes [HH.ClassName "Editor"] ]
      [ bufferHtml
      , terminalHtml
      ]
