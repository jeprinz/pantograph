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

editorComponent :: forall ctx env r n d. 
  Lacks "elemId" d => Lacks "cursor" d => Lacks "select" d =>
  H.Component EditorQuery (EditorInput ctx env r n d (Sort n d)) EditorOutput Aff
editorComponent = HK.component \{} (EditorInput input) -> HK.do
  let Language language = input.language

  let bufferHtml = do
        let bufferInput = BufferInput 
              { expr: topExpr (Language language)
              , ctx: input.ctx
              , env: input.env
              , renderer: input.renderer }
        let bufferHandler = case _ of
              LogFromBuffer typ html -> pure unit
        HH.slot (Proxy :: Proxy "buffer") unit bufferComponent bufferInput bufferHandler

  let consoleHtml = do
        let consoleInput = ConsoleInput {}
        let consoleHandler = ?a
        HH.slot (Proxy :: Proxy "console") unit consoleComponent consoleInput consoleHandler

  HK.pure $
    HH.div
      []
      [ bufferHtml
      , consoleHtml
      ]
