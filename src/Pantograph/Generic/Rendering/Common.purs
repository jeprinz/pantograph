module Pantograph.Generic.Rendering.Common where

import Data.Either.Nested
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (ElementId(..))
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)

-- RenderM

type RenderM ctx env r n d s = 
  ReaderT (RenderCtx ctx r n d s) (
  StateT (RenderEnv env r n d s) (
  Identity))

runRenderM :: forall ctx env r n d s a. RenderCtx ctx r n d s -> RenderEnv env r n d s -> RenderM ctx env r n d s a -> a
runRenderM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

mapRenderM :: forall ctx env r n d s a b. (a -> b) -> (RenderM ctx env r n d s a -> RenderM ctx env r n d s b)
mapRenderM f = map f

type RenderCtx ctx r n d s =
  { depth :: Int
  , bufferId :: HK.StateId (Buffer r n d s)
  , outputToken :: HK.OutputToken (BufferOutput r n d s)
  | ctx }

type RenderEnv env r n d s =
  { holeCount :: Int
  | env }

-- Renderer

newtype Renderer r n d s = Renderer
  { arrangeExpr :: forall ctx env a.
      ExprNode r n d s ->
      Array (RenderM ctx env r n d s (ExprNode r n d s /\ a)) ->
      RenderM ctx env r n d s (Array (Array (Html r n d s) \/ a))
  }

  -- TODO: not sure if `arrangeSort` is necessary
  -- arrangeSort :: forall ctx env a. SortNode n -> Array (Sort n /\ a) -> RenderM ctx env r n (Array (a \/ (Html r n)))

-- RenderNode

type RenderData d = (elemId :: ElementId | d)

arrangeRenderExpr :: forall ctx env r n d s a.
  Renderer r n d s ->
  ExprNode r n (RenderData d) s ->
  Array (RenderM ctx env r n d s (a /\ ExprNode r n (RenderData d) s)) ->
  RenderM ctx env r n d s (Array (a \/ Array (Html r n d s)))
arrangeRenderExpr = hole "TODO"

-- Html

type Html r n d s = HH.ComponentHTML (HK.HookM Aff Unit) (Slots r n d s) Aff

type Slots r n d s =
  ( editor :: EditorSlot
  , buffer :: BufferSlot r n d s
  , toolbox :: ToolboxSlot r n
  , preview :: PreviewSlot r n
  , clipboard :: ClipboardSlot r n
  , console :: ConsoleSlot r n )

-- Editor

type EditorSlot = H.Slot EditorQuery EditorOutput EditorSlotId
newtype EditorInput ctx env r n d s = EditorInput
  { ctx :: RenderCtx ctx r n d s 
  , env :: RenderEnv env r n d s
  , language :: Language r n d
  , renderer :: Renderer r n d s }
type EditorQuery = Const Void
type EditorOutput = Void
type EditorSlotId = Unit

-- Buffer

type BufferSlot r n d s = H.Slot (BufferQuery r n d s) (BufferOutput r n d s) BufferSlotId
newtype BufferInput ctx env r n d s = BufferInput 
  { expr :: Expr r n d s
  , ctx :: RenderCtx ctx r n d s 
  , env :: RenderEnv env r n d s
  , renderer :: Renderer r n d s }
data BufferQuery r n d s a
  = SetBuffer (Buffer r n d s) a
data BufferOutput r n d s
  = WriteConsoleFromBuffer ConsoleItem
data BufferSlotId

data Buffer r n d s
  = TopBuffer (Expr r n d s)
  | CursorBuffer (Expr r n (CursorData d) s)
  | SelectBuffer (Expr r n (SelectData d) s)

-- Toolbox

type ToolboxSlot r n = H.Slot (ToolboxQuery r n) (ToolboxOutput r n) (ToolboxSlotId r n)
newtype ToolboxInput r n = ToolboxInput {}
data ToolboxQuery r n a
data ToolboxOutput r n
data ToolboxSlotId r n

-- Preview

type PreviewSlot r n = H.Slot (PreviewQuery r n) (PreviewOutput r n) (PreviewSlotId r n)
newtype PreviewInput r n = PreviewInput {}
data PreviewQuery r n a
data PreviewOutput r n
data PreviewSlotId r n

-- Clipboard

type ClipboardSlot r n = H.Slot (ClipboardQuery r n) (ClipboardOutput r n) (ClipboardSlotId r n)
newtype ClipboardInput r n = ClipboardInput {}
data ClipboardQuery r n a
data ClipboardOutput r n
data ClipboardSlotId r n

-- Console

type ConsoleSlot r n = H.Slot ConsoleQuery ConsoleOutput ConsoleSlotId
newtype ConsoleInput = ConsoleInput {}
data ConsoleQuery a
  = WriteConsole ConsoleItem a
type ConsoleOutput = Void
type ConsoleSlotId = Unit

data ConsoleItemTag = DebugConsoleItemTag
newtype ConsoleItem = ConsoleItem {tag :: ConsoleItemTag, html :: HH.PlainHTML}
