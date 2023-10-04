module Pantograph.Generic.Rendering.Editor where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Pantograph.Generic.Rendering.Console (consoleComponent)
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
              WriteConsoleFromBuffer item -> do
                HK.tell slotToken (Proxy :: Proxy "console") unit (WriteConsole item)
        HH.slot (Proxy :: Proxy "buffer") unit bufferComponent bufferInput bufferHandler

  let consoleHtml = do
        let consoleInput = ConsoleInput {}
        let consoleHandler = absurd
        HH.slot (Proxy :: Proxy "console") unit consoleComponent consoleInput consoleHandler

  HK.pure $
    HH.div
      []
      [ bufferHtml
      , consoleHtml
      ]
