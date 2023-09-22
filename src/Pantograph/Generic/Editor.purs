module Pantograph.Generic.Editor where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK

-- EditorHTML

type EditorHTML (rule :: Type) (joint :: Type -> Type) = 
  HH.ComponentHTML
    (HK.HookM Aff Unit)
    ( toolbox :: H.Slot (ToolboxQuery rule joint) (ToolboxOutput rule joint) ToolboxSlot
    , preview :: H.Slot (PreviewQuery rule joint) Unit PreviewSlot
    , clipboard :: H.Slot (ClipboardQuery rule joint) (ClipboardOutput rule joint) ClipboardSlot
    , console :: H.Slot (ConsoleQuery rule joint) (ConsoleOutput rule joint) ConsoleSlot
    )
    Aff

-- Toolbox

-- | A Toolbox is a drop-down menu of possible edits. It appears when you start
-- | typing at a cursor.

type ToolboxSlot = Unit
type ToolboxInput = Unit
data ToolboxQuery rule joint a
data ToolboxOutput rule joint

-- Preview

-- | A Preview is on either side of the cursor when an insertion edit is being
-- | considered. It renders the result of the insertion edit in-place either
-- | outside the cursor for an outwards insertion or inside the cursor for an
-- | inwards.

type PreviewSlot = PreviewPosition
data PreviewQuery rule joint a
type PreviewInput = Unit
data PreviewOutput rule joint

data PreviewPosition 
  = OutsideBeforePreviewPosition | OutsideAfterPreviewPosition
  | InsideBeforePreviewPosition | InsideAfterPreviewPosition

previewComponent :: forall rule joint. H.Component (PreviewQuery rule joint) PreviewInput (PreviewOutput rule joint) Aff
previewComponent = HK.component \tkn pos -> HK.do
  HK.pure do
    HH.div [] [HH.text "TODO: previewComponent"]

-- Clipboard

type ClipboardSlot = Unit
data ClipboardQuery rule joint a
type ClipboardInput = Unit
data ClipboardOutput rule joint

data ClipboardValue rule joint
  = TermClipboardValue (Term rule joint)
  | PathClipboardValue (SomePath joint (Term rule joint))

clipboardComponent :: forall rule joint. H.Component (ClipboardQuery rule joint) ClipboardInput (ClipboardOutput rule joint) Aff
clipboardComponent = HK.component \tkn dir -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (ClipboardValue rule joint))
  HK.pure do
    HH.div [] [HH.text "TODO: clipboardComponent"]

-- Console

type ConsoleSlot = Unit
data ConsoleQuery rule joint a = ConsoleQuery
type ConsoleInput = Unit
data ConsoleOutput rule joint

consoleComponent :: forall rule joint. H.Component (ConsoleQuery rule joint) ConsoleInput (ConsoleOutput rule joint) Aff
consoleComponent = HK.component \tkn _ -> HK.do
  logs /\ logs_id <- HK.useState (mempty :: List HH.PlainHTML)
  HK.pure do
    HH.div [] [HH.text "TODO: consoleComponent"]

-- RenderingCtx, RenderingEnv

newtype RenderingCtx ctx = RenderingCtx { | ctx } -- TODO:

newtype RenderingEnv env = RenderingEnv { | env } -- TODO:

type RenderingT ctx env m = ReaderT (RenderingCtx ctx) (StateT (RenderingEnv env) m)
type RenderingM ctx env = RenderingT ctx env Identity

-- EditorState

newtype EditorState rule joint = EditorState
  { buffer :: Buffer rule joint }

-- | A `Buffer` is an independent section of code.
data Buffer rule joint
  = CursorBuffer (Cursor joint (Term rule joint))
  | SelectBuffer (Select joint (Term rule joint))
  | TopBuffer (Term rule joint)
