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

class ToClassName a where
  toClassName :: a -> HH.ClassName

-- | # M

type M ctx env el ed sn m = ReaderT (Record ctx) (StateT (Record env) m)

runM :: forall ctx env el ed sn m a. Record ctx -> Record env -> M ctx env el ed sn m a -> m (a /\ Record env)
runM ctx env = flip runReaderT ctx >>> flip runStateT env

-- | ## Sync
-- |
-- | Syncronizing happens once per state change. The `SyncExprData` corresponds
-- | the tree to the rendered DOM.

-- | `ExprNode` annotation data that is generated during syncronizing.
type SyncExprData ed = (elemId :: HU.ElementId | ed)

-- TODO: hydration is now simple a pass rahter than annotator
-- -- | ## Hydrate
-- -- |
-- -- | Initial hydrating happens once per state change and once per UI interaction
-- -- | that modifies hydrate data.
-- -- |
-- -- | Hydrate data can be used in re-hydrating and generic (not specific)
-- -- | rendering.

-- type HydrateM ctx env el ed sn = 
--   M
--     (HydrateCtx el ed sn ctx)
--     (HydrateEnv el ed sn env)
--     el ed sn (HK.HookM Aff)

-- type HydrateCtx el ed sn ctx =
--   ( gyroPosition :: GyroPosition 
--   | ctx )

-- type HydrateEnv el ed sn env =
--   ( 
--   | env )

-- -- | `ExprNode` annotation data that is generated during initial hydrating and
-- -- | mapped when re-hydrating.
-- type HydrateExprData ed = (gyroPosition :: GyroPosition | ed)

data GyroPosition
  = InsideRoot
  | AtCursor | OutsideCursor | InsideCursor
  | OutsideSelect | AtOutsideSelect | BetweenSelect | AtInsideSelect | InsideSelect

derive instance Generic GyroPosition _
instance Show GyroPosition where show = genericShow
instance ToClassName GyroPosition where toClassName = HH.ClassName <<< show

-- | ## Render
-- |
-- | Rendering happens once per state change.

type RenderM ctx env el ed sn =
  M
    (RenderCtx el ed sn ctx)
    (RenderEnv el ed sn env)
    el ed sn Identity

type RenderCtx el ed sn ctx =
  ( depth :: Int
  | ctx )

type RenderEnv el ed sn env =
  ( holeCount :: Int
  | env )

-- | # Renderer
-- |
-- | TODO: description

newtype Renderer ctx env el ed sn = Renderer
  { name :: String
  , language :: Language el ed sn
  , topCtx :: Record ctx
  , topEnv :: Record env
  , arrangeExpr :: forall ctx_ ctx' env_ env' ed' a.
      Union ctx ctx_ ctx' => Union env env_ env' =>
      ExprNode el ed' sn ->
      Array (RenderM ctx' env' el ed sn (a /\ ExprNode el ed' sn)) ->
      RenderM ctx env el ed sn (Array (ArrangeKid el ed sn a))
  }

data ArrangeKid el ed sn a
  = ExprKidArrangeKid a
  | PunctuationArrangeKid (Array (BufferHtml el ed sn))
  | IndentationArrangeKid (Array (BufferHtml el ed sn))

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
  = SetGyro (ExprGyro el ed sn) a
data BufferOutput el ed sn
  = WriteTerminalFromBuffer TerminalItem
data BufferSlotId

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
