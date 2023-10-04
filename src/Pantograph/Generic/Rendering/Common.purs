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

type RenderM ctx env r n d = 
  ReaderT (RenderCtx ctx r n d) (
  StateT (RenderEnv env r n d) (
  Identity))

runRenderM :: forall ctx env r n d a. RenderCtx ctx r n d -> RenderEnv env r n d -> RenderM ctx env r n d a -> a
runRenderM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

mapRenderM :: forall ctx env r n d a b. (a -> b) -> (RenderM ctx env r n d a -> RenderM ctx env r n d b)
mapRenderM f = map f

type RenderCtx ctx r n d =
  { depth :: Int
  , outputToken :: HK.OutputToken (BufferOutput r n d)
  | ctx }

type RenderEnv env r n d =
  { holeCount :: Int
  | env }

-- Renderer

newtype Renderer ctx env r n d = Renderer
  { name :: String
  , topCtx :: Record ctx
  , topEnv :: Record env
  , arrangeExpr :: forall a.
      ExprNode r n d (Sort n d) ->
      Array (RenderM ctx env r n d (ExprNode r n d (Sort n d) /\ a)) ->
      RenderM ctx env r n d (Array (Array (Html r n d) \/ a))
  }

  -- TODO: not sure if `arrangeSort` is necessary
  -- arrangeSort :: forall ctx env a. SortNode n -> Array (Sort n /\ a) -> RenderM ctx env r n (Array (a \/ (Html r n)))

-- RenderNode

type RenderData d = (elemId :: ElementId | d)

arrangeRenderExpr :: forall ctx env r n d a.
  Renderer ctx env r n d ->
  ExprNode r n (RenderData d) (Sort n d) ->
  Array (RenderM ctx env r n d (a /\ ExprNode r n (RenderData d) (Sort n d))) ->
  RenderM ctx env r n d (Array (a \/ Array (Html r n d)))
arrangeRenderExpr = hole "TODO"

-- Html

type Html r n d = HH.ComponentHTML (HK.HookM Aff Unit) (Slots r n d) Aff

type Slots r n d =
  ( editor :: EditorSlot
  , buffer :: BufferSlot r n d
  , toolbox :: ToolboxSlot r n
  , preview :: PreviewSlot r n
  , clipboard :: ClipboardSlot r n
  , console :: ConsoleSlot r n )

-- Editor

type EditorSlot = H.Slot EditorQuery EditorOutput EditorSlotId
newtype EditorInput ctx env r n d = EditorInput
  { editor :: Editor ctx env r n d }
type EditorQuery = Const Void
type EditorOutput = Void
type EditorSlotId = Unit

newtype Editor ctx env r n d = Editor
  { name :: String
  , language :: Language r n d
  , renderer :: Renderer ctx env r n d }

-- Buffer

type BufferSlot r n d = H.Slot (BufferQuery r n d) (BufferOutput r n d) BufferSlotId
newtype BufferInput ctx env r n d = BufferInput 
  { editor :: Editor ctx env r n d
  , expr :: Expr r n d (Sort n d) }
data BufferQuery r n d a
  = SetBuffer (Buffer r n d) a
data BufferOutput r n d
  = WriteConsoleFromBuffer ConsoleItem
data BufferSlotId

data Buffer r n d
  = TopBuffer (Expr r n d (Sort n d))
  | CursorBuffer (Expr r n (CursorData d) (Sort n d))
  | SelectBuffer (Expr r n (SelectData d) (Sort n d))

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
