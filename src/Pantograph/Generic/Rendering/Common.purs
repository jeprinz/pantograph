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
import Data.Maybe (Maybe)
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
import Record as R
import Type.Proxy (Proxy(..))

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

enRenderCtx :: forall ctx r n d. Lacks "depth" ctx => Lacks "outputToken" ctx => RenderCtx () r n d -> Record ctx -> RenderCtx ctx r n d
enRenderCtx {depth, outputToken} =
  R.insert (Proxy :: Proxy "depth") depth >>>
  R.insert (Proxy :: Proxy "outputToken") outputToken

type RenderEnv env r n d =
  { holeCount :: Int
  | env }

enRenderEnv :: forall env r n d. Lacks "holeCount" env => RenderEnv () r n d -> Record env -> RenderEnv env r n d
enRenderEnv {holeCount} =
  R.insert (Proxy :: Proxy "holeCount") holeCount

-- Renderer

newtype Renderer ctx env r n d = Renderer
  { name :: String
  , topCtx :: Record ctx
  , topEnv :: Record env
  , arrangeExpr :: forall a.
      ExprNode r n d (Sort n d) ->
      Array (RenderM ctx env r n d (ExprNode r n d (Sort n d) /\ a)) ->
      RenderM ctx env r n d (Array (Array (BufferHtml r n) \/ a))
  }

  -- TODO: not sure if `arrangeSort` is necessary
  -- arrangeSort :: forall ctx env a. SortNode n -> Array (Sort n /\ a) -> RenderM ctx env r n (Array (a \/ (Html r n)))

-- RenderNode

type RenderData d = (elemId :: ElementId | d)

unRenderData :: forall d. Lacks "elemId" d => Record (RenderData d) -> Record d
unRenderData = R.delete (Proxy :: Proxy "elemId")

enRenderData :: forall d. Lacks "elemId" d => Record (RenderData ()) -> Record d -> Record (RenderData d)
enRenderData {elemId} = 
  R.insert (Proxy :: Proxy "elemId") elemId

arrangeRenderExpr :: forall ctx env r n d a.
  Renderer ctx env r n d ->
  ExprNode r n (RenderData d) (Sort n d) ->
  Array (RenderM ctx env r n d (a /\ ExprNode r n (RenderData d) (Sort n d))) ->
  RenderM ctx env r n d (Array (a \/ Array (BufferHtml r n)))
arrangeRenderExpr = hole "TODO"

-- Editor

type EditorSlot = H.Slot EditorQuery EditorOutput EditorSlotId
newtype EditorInput ctx env r n d = EditorInput
  { engine :: Engine ctx env r n d }
type EditorQuery = Const Void
type EditorOutput = Void
type EditorSlotId = Unit

newtype Engine ctx env r n d = Engine
  { name :: String
  , language :: Language r n d
  , renderer :: Renderer ctx env r n d }

engineDescription (Engine {name: engineName, language: Language {name: languageName}, renderer: Renderer {name: rendererName}}) =
  "{engine: " <> engineName <> " {" <>
    "language: " <> languageName <> ", " <>
    "renderer: " <> rendererName <> "}}"

-- Buffer

type BufferSlot r n d = H.Slot (BufferQuery r n d) (BufferOutput r n d) BufferSlotId
newtype BufferInput ctx env r n d = BufferInput 
  { name :: String
  , engine :: Engine ctx env r n d
  , expr :: Expr r n d (Sort n d) }
data BufferQuery r n d a
  = SetBuffer (Buffer r n d (Sort n d)) a
data BufferOutput r n d
  = WriteTerminalFromBuffer TerminalItem
data BufferSlotId

data Buffer r n d s
  = TopBuffer (Expr r n d s)
  | CursorBuffer (Expr r n (CursorData d) s)
  | SelectBuffer (Expr r n (SelectData d) s)

type BufferHtml r n = 
  HH.ComponentHTML
    (HK.HookM Aff Unit) 
    ( toolbox :: ToolboxSlot r n
    , preview :: PreviewSlot r n )
    Aff

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

-- Terminal

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
