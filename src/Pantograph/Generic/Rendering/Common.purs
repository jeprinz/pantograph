module Pantograph.Generic.Rendering.Common where

import Prelude
import Data.Either.Nested
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Lazy (Lazy)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (ElementId(..))
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Type.Proxy (Proxy(..))

-- | # RenderM
-- |
-- | TODO: description

type RenderM ctx env el ed sn = 
  ReaderT (RenderCtx ctx el ed sn) (
  StateT (RenderEnv env el ed sn) (
  Identity))

runRenderM :: forall ctx env el ed sn a. RenderCtx ctx el ed sn -> RenderEnv env el ed sn -> RenderM ctx env el ed sn a -> a
runRenderM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

mapRenderM :: forall ctx env el ed sn a b. (a -> b) -> (RenderM ctx env el ed sn a -> RenderM ctx env el ed sn b)
mapRenderM f = map f

type RenderCtx ctx el ed sn =
  { depth :: Int
  | ctx }

type RenderEnv env el ed sn =
  { holeCount :: Int
  | env }

-- | # Renderer
-- |
-- | TODO: description

newtype Renderer ctx env el ed sn = Renderer
  { name :: String
  , language :: Language el ed sn
  , topCtx :: Record ctx
  , topEnv :: Record env
  , arrangeExpr :: forall a.
      ExprNode el ed sn ->
      Array (RenderM ctx env el ed sn (ExprNode el ed sn /\ a)) ->
      RenderM ctx env el ed sn (Array (Array (BufferHtml el ed sn) \/ a)) }

-- | # Editor
-- |
-- | TODO: description

type EditorSlot = H.Slot EditorQuery EditorOutput EditorSlotId
newtype EditorInput ctx env el ed sn = EditorInput
  { renderer :: Renderer ctx env el ed sn }
type EditorQuery :: Type -> Type
type EditorQuery = Const Void
type EditorOutput = Void
type EditorSlotId = Unit

-- | # Buffer
-- |
-- | A "Buffer" is an editable window of code.

type BufferSlot el ed sn = H.Slot (BufferQuery el ed sn) (BufferOutput el ed sn) BufferSlotId
newtype BufferInput ctx env el ed sn = BufferInput 
  { name :: String
  , renderer :: Renderer ctx env el ed sn
  , expr :: Expr el ed sn }
data BufferQuery el ed sn a
  = SetBuffer (Buffer el ed sn) a
data BufferOutput el ed sn
  = WriteTerminalFromBuffer TerminalItem
data BufferSlotId

data Buffer el ed sn
  = TopBuffer (Expr el ed sn)
  | CursorBuffer (ExprCursor el ed sn)
  | SelectBuffer (ExprSelect el ed sn)

type BufferHtml el ed sn = 
  HH.ComponentHTML
    (HK.HookM Aff Unit) 
    ( toolbox :: ToolboxSlot el ed sn
    , preview :: PreviewSlot el ed sn )
    Aff

-- | # Toolbox
-- |
-- | A "Toolbox" is a little box of completions that appears when you start
-- | typing at a cursor.

type ToolboxSlot el ed sn = H.Slot (ToolboxQuery el ed sn) (ToolboxOutput el ed sn) (ToolboxSlotId el ed sn)
newtype ToolboxInput el ed sn = ToolboxInput 
  { items :: ToolboxItem el ed sn }
data ToolboxQuery el ed sn a
data ToolboxOutput el ed sn
  = SubmitToolboxItem (ToolboxItem el ed sn)
data ToolboxSlotId el ed sn = ToolboxSlotId (ExprPath el ed sn)

data ToolboxItem el ed sn
  = ReplaceToolboxItem (Expr el ed sn)
  | InsertToolboxItem (ExprPath el ed sn)

-- | # Preview
-- |
-- | TODO: description

type PreviewSlot el ed sn = H.Slot (PreviewQuery el ed sn) (PreviewOutput el ed sn) (PreviewSlotId el ed sn)
newtype PreviewInput el ed sn = PreviewInput {}
data PreviewQuery el ed sn a
data PreviewOutput el ed sn
data PreviewSlotId el ed sn

-- | # Clipboard
-- |
-- | TODO: description

type ClipboardSlot el ed sn = H.Slot (ClipboardQuery el ed sn) (ClipboardOutput el ed sn) (ClipboardSlotId el ed sn)
newtype ClipboardInput el ed sn = ClipboardInput {}
data ClipboardQuery el ed sn a
data ClipboardOutput el ed sn
data ClipboardSlotId el ed sn

-- | # Terminal
-- |
-- | TODO: description

type TerminalSlot = H.Slot TerminalQuery TerminalOutput TerminalSlotId
newtype TerminalInput = TerminalInput {}
data TerminalQuery a
  -- Write a new item to the terminal
  = WriteTerminal TerminalItem a
  -- Toggles the terminal open/close, or set it to a given value for isOpen if
  -- provided.
  | ToggleOpenTerminal (Maybe Boolean) a
type TerminalOutput = Void
type TerminalSlotId = Unit

data TerminalItemTag = DebugTerminalItemTag
newtype TerminalItem = TerminalItem {tag :: TerminalItemTag, html :: HH.PlainHTML}
