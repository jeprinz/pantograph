module Pantograph.Generic.Editor where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen as HH
import Halogen.Hooks as HK

type EditorHTML rule joint = 
  HH.ComponentHTML
    (HK.HookM Aff Unit)
    ( buffer :: H.Slot (BufferQuery rule joint) (BufferOutput rule joint) Unit
    , preview :: H.Slot (PreviewQuery rule joint) Unit Unit -- HorizontalDir
    , console :: H.Slot ConsoleQuery ConsoleOutput Unit
    ) 
    Aff

data BufferQuery rule joint a
data BufferOutput rule joint

data PreviewQuery rule joint a
data PreviewOutput rule joint

data ConsoleQuery a = ConsoleQuery
type ConsoleOutput = Void