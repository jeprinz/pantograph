module Pantograph.Generic.Rendering.Common where

import Data.Either.Nested
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT, runStateT)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Lazy (Lazy)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks, class Union)
import Record as R
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent as KeyboardEvent

class ToClassNames a where
  toClassNames :: a -> Array HH.ClassName

-- | # M

type M ctx env m = ReaderT (Record ctx) (StateT (Record env) m)

runM :: forall sn el ctx env m a. Record ctx -> Record env -> M ctx env m a -> m (a /\ Record env)
runM ctx env = flip runReaderT ctx >>> flip runStateT env

-- | ## Sync
-- |
-- | Syncronizing happens once per state change. The `SyncExprAnn` corresponds
-- | the tree to the rendered DOM.

-- | `ExprNode` annotation data that is generated during syncronizing.
type SyncExprRow sn el er = 
  ( elemId :: HU.ElementId 
  | er )
type SyncExpr sn el er = AnnExpr sn el (SyncExprRow sn el er)
type SyncExprNode sn el er = AnnExprNode sn el (SyncExprRow sn el er)
type SyncExprTooth sn el er = AnnExprTooth sn el (SyncExprRow sn el er)
type SyncExprPath sn el er = AnnExprPath sn el (SyncExprRow sn el er)
type SyncExprCursor sn el er = AnnExprCursor sn el (SyncExprRow sn el er)
type SyncExprSelect sn el er = AnnExprSelect sn el (SyncExprRow sn el er)
type SyncExprGyro sn el er = AnnExprGyro sn el (SyncExprRow sn el er)

-- | ## Hydrate
-- |
-- | Initial hydrating happens once per state change and once per UI interaction
-- | that modifies hydrate data.
-- |
-- | Hydrate data can be used in re-hydrating and generic (not specific)
-- | rendering.

type HydrateM sn el = 
  M
    (HydrateCtx sn el)
    (HydrateEnv sn el)
    (HK.HookM Aff)

type HydrateCtx sn el =
  ( gyroPosition :: GyroPosition )

type HydrateEnv sn el =
  ( )

-- | `ExprNode` annotation data that is generated during initial hydrating and
-- | mapped when re-hydrating.
type HydrateExprRow sn el er =
  ( gyroPosition :: GyroPosition
  | SyncExprRow sn el er )
type HydrateExpr sn el er = AnnExpr sn el (HydrateExprRow sn el er)
type HydrateExprNode sn el er = AnnExprNode sn el (HydrateExprRow sn el er)
type HydrateExprTooth sn el er = AnnExprTooth sn el (HydrateExprRow sn el er)
type HydrateExprPath sn el er = AnnExprPath sn el (HydrateExprRow sn el er)
type HydrateExprCursor sn el er = AnnExprCursor sn el (HydrateExprRow sn el er)
type HydrateExprSelect sn el er = AnnExprSelect sn el (HydrateExprRow sn el er)
type HydrateExprGyro sn el er = AnnExprGyro sn el (HydrateExprRow sn el er)

data GyroPosition
  = InsideRoot
  | AtCursor | OutsideCursor | InsideCursor
  | OutsideSelect | AtOutsideSelect | BetweenSelect | AtInsideSelect | InsideSelect

derive instance Generic GyroPosition _
instance Show GyroPosition where show = genericShow
instance ToClassNames GyroPosition where
  toClassNames gp = [HH.ClassName "Expr", HH.ClassName (show gp)]

-- | ## Render
-- |
-- | Rendering happens once per state change.

type RenderM sn el ctx env =
  M
    (RenderCtx sn el ctx)
    (RenderEnv sn el env)
    Identity

type RenderCtx sn el ctx =
  ( depth :: Int
  , outputToken :: HK.OutputToken (BufferOutput sn el)
  , setExprGyro :: ExprGyro sn el -> HK.HookM Aff Unit
  | ctx )

type RenderEnv sn el env =
  ( holeCount :: Int
  | env )

-- | # Renderer
-- |
-- | TODO: description

newtype Renderer sn el ctx env = Renderer
  { name :: String
  , language :: Language sn el
  , topCtx :: Record ctx
  , topEnv :: Record env
  , arrangeExpr :: forall er a.
      AnnExprNode sn el er ->
      Array (RenderM sn el ctx env (a /\ AnnExprNode sn el er)) ->
      RenderM sn el ctx env (Array (ArrangeKid sn el a))
  }

data ArrangeKid sn el a
  = ExprKidArrangeKid a
  | PunctuationArrangeKid (Array (BufferHtml sn el))
  | IndentationArrangeKid (Array (BufferHtml sn el))

rendererFullName :: forall sn el ctx env. Renderer sn el ctx env -> String
rendererFullName (Renderer renderer@{language: Language language}) =
  "{" <> 
  "language: " <> language.name <>
  ", " <> 
  "renderer: " <> renderer.name <> 
  "}"

-- | # Editor
-- |
-- | TODO: description

type EditorSlot = H.Slot EditorQuery EditorOutput EditorSlotId
newtype EditorInput sn el ctx env = EditorInput
  { renderer :: Renderer sn el ctx env }
type EditorQuery :: Type -> Type
type EditorQuery = Const Void
type EditorOutput = Void
type EditorSlotId = Unit

-- | # Buffer
-- |
-- | A "Buffer" is an editable window of code.

type BufferSlot sn el = H.Slot (BufferQuery sn el) (BufferOutput sn el) BufferSlotId
newtype BufferInput sn el ctx env = BufferInput 
  { name :: String
  , renderer :: Renderer sn el ctx env
  , expr :: Expr sn el }
data BufferQuery sn el a
  = SetExprGyro (ExprGyro sn el) a
  | KeyboardEventBufferQuery KeyboardEvent.KeyboardEvent a
data BufferOutput sn el
  = WriteTerminalFromBuffer TerminalItem
data BufferSlotId

-- data BufferMode sn el
--   = 

type BufferHtml sn el = 
  HH.ComponentHTML
    (HK.HookM Aff Unit) 
    ( toolbox :: ToolboxSlot sn el
    , preview :: PreviewSlot sn el )
    Aff

-- | # Toolbox
-- |
-- | A "Toolbox" is a little box of completions that appears when you start
-- | typing at a cursor.

type ToolboxSlot sn el = H.Slot (ToolboxQuery sn el) (ToolboxOutput sn el) (ToolboxSlotId sn el)
newtype ToolboxInput sn el = ToolboxInput 
  { items :: ToolboxItem sn el }
data ToolboxQuery sn el a
data ToolboxOutput sn el
  = SubmitToolboxItem (ToolboxItem sn el)
data ToolboxSlotId sn el = ToolboxSlotId (ExprPath sn el)

data ToolboxItem sn el
  = ReplaceToolboxItem (Expr sn el)
  | InsertToolboxItem (ExprPath sn el)

-- | # Preview
-- |
-- | TODO: description

type PreviewSlot sn el = H.Slot (PreviewQuery sn el) (PreviewOutput sn el) (PreviewSlotId sn el)
newtype PreviewInput sn el = PreviewInput {}
data PreviewQuery sn el a
data PreviewOutput sn el
data PreviewSlotId sn el

-- | # Clipboard
-- |
-- | TODO: description

type ClipboardSlot sn el = H.Slot (ClipboardQuery sn el) (ClipboardOutput sn el) (ClipboardSlotId sn el)
newtype ClipboardInput sn el = ClipboardInput {}
data ClipboardQuery sn el a
data ClipboardOutput sn el
data ClipboardSlotId sn el

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