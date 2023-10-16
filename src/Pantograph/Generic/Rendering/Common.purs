module Pantograph.Generic.Rendering.Common where

import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tree (Orientation)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.Utilities as HU
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
type SyncExprNonEmptyPath sn el er = AnnExprNonEmptyPath sn el (SyncExprRow sn el er)
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

-- | `ExprNode` annotation data that is generated during initial hydrating and
-- | mapped when re-hydrating.
type HydrateExprRow' sn el er =
  ( beginsLine :: Orientation -> Boolean
  , validCursor :: Orientation -> Boolean
  , validSelect :: Boolean
  | er )

type HydrateExprRow sn el er = HydrateExprRow' sn el (SyncExprRow sn el er)
type HydrateExpr sn el er = AnnExpr sn el (HydrateExprRow sn el er)
type HydrateExprNode sn el er = AnnExprNode sn el (HydrateExprRow sn el er)
type HydrateExprTooth sn el er = AnnExprTooth sn el (HydrateExprRow sn el er)
type HydrateExprPath sn el er = AnnExprPath sn el (HydrateExprRow sn el er)
type HydrateExprNonEmptyPath sn el er = AnnExprNonEmptyPath sn el (HydrateExprRow sn el er)
type HydrateExprCursor sn el er = AnnExprCursor sn el (HydrateExprRow sn el er)
type HydrateExprSelect sn el er = AnnExprSelect sn el (HydrateExprRow sn el er)
type HydrateExprGyro sn el er = AnnExprGyro sn el (HydrateExprRow sn el er)

-- | ## Render
-- |
-- | Rendering happens once per state change.

type RenderM sn el ctx env = M (RenderCtx sn el ctx) (RenderEnv sn el env) Identity

type RenderCtx sn el ctx =
  ( depth :: Int
  , outputToken :: HK.OutputToken (BufferOutput sn el)
  , slotToken :: HK.SlotToken (BufferSlots sn el)
  , modifyExprGyro :: (ExprGyro sn el -> Maybe (ExprGyro sn el)) -> HK.HookM Aff Unit
  , modifySyncedExprGyro :: (SyncExprGyro sn el () -> Maybe (SyncExprGyro sn el ())) -> HK.HookM Aff Unit
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
  -- , beginsLine :: forall er1 er2. {outside :: AnnExprPath sn el er1, middle :: AnnExprTooth sn el er2, inside :: AnnExpr sn el er2} -> Boolean
  , beginsLine :: forall er. AnnExprCursor sn el er -> Boolean
  }

data ArrangeKid sn el a
  = ExprKidArrangeKid a
  | HtmlArrangeKid (Array (BufferHtml sn el))
  | IndentationArrangeKid (Array (BufferHtml sn el))

derive instance Functor (ArrangeKid sn el)

isExprKidArrangeKidSuchThat :: forall sn el a. (a -> Boolean) -> ArrangeKid sn el a -> Boolean
isExprKidArrangeKidSuchThat cond = case _ of
  ExprKidArrangeKid a -> cond a
  _ -> false

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
newtype BufferQuery sn el a = BufferQuery (Variant
  ( "set exprGyro" :: ExprGyro sn el /\ a
  , "keyboard" :: KeyboardEvent.KeyboardEvent /\ a ))
newtype BufferOutput sn el = BufferOutput (Variant
  ( "write terminal" :: TerminalItem ))
data BufferSlotId

-- data BufferMode sn el
--   = 

type BufferHtml sn el = HH.ComponentHTML (HK.HookM Aff Unit) (BufferSlots sn el) Aff
type BufferSlots sn el = 
  ( toolbox :: ToolboxSlot sn el
  , preview :: PreviewSlot sn el )

-- | # Toolbox
-- |
-- | A "Toolbox" is a little box of completions that appears when you start
-- | typing at a cursor.

type ToolboxSlot sn el = H.Slot (ToolboxQuery sn el) (ToolboxOutput sn el) ToolboxSlotId
newtype ToolboxInput sn el ctx env = ToolboxInput 
  { renderer :: Renderer sn el ctx env
  , ctx :: Record (RenderCtx sn el ctx)
  , env :: Record (RenderEnv sn el env)
  , outside :: SyncExprPath sn el ()
  , inside :: SyncExpr sn el ()
  , isEnabled :: Boolean
  , edits :: Array (NonEmptyArray (ExprEdit sn el)) }
newtype ToolboxQuery sn el a = ToolboxQuery (Variant
  ( "modify isEnabled" :: (Boolean -> Boolean) /\ a
  , "get isEnabled" :: (Boolean -> a)
  , "modify select" :: (ToolboxSelect -> ToolboxSelect) /\ a
  , "submit edit" :: Unit /\ a))
newtype ToolboxOutput sn el = ToolboxOutput (Variant
  ( "submit edit" :: ExprEdit sn el
  , "preview edit" :: Maybe (ExprEdit sn el) ))
type ToolboxSlotId = Unit

data ToolboxSelect = ToolboxSelect Int Int

-- | # Preview
-- |
-- | TODO: description

type PreviewSlot sn el = H.Slot (PreviewQuery sn el) PreviewOutput PreviewSlotId
newtype PreviewInput sn el ctx env = PreviewInput
  { renderer :: Renderer sn el ctx env
  , ctx :: Record (RenderCtx sn el ctx)
  , env :: Record (RenderEnv sn el env)
  , outside :: SyncExprPath sn el ()
  , inside :: SyncExpr sn el ()
  , position :: PreviewPosition
  , maybeEdit :: Maybe (ExprEdit sn el) }
newtype PreviewQuery sn el a = PreviewQuery (Variant
  ( "modify maybeEdit" :: (Maybe (ExprEdit sn el) -> Maybe (ExprEdit sn el)) /\ a ))
type PreviewOutput = Void
type PreviewSlotId = PreviewPosition

data PreviewPosition = LeftPreviewPosition | RightPreviewPosition
derive instance Eq PreviewPosition
derive instance Ord PreviewPosition

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
newtype TerminalQuery a = TerminalQuery (Variant
  ( "write" :: TerminalItem /\ a
  , "toggle isOpen" :: Maybe Boolean /\ a
  , "get inputIsFocused" :: Boolean -> a ))
type TerminalOutput = Void
type TerminalSlotId = Unit

data TerminalItemTag = DebugTerminalItemTag
newtype TerminalItem = TerminalItem {tag :: TerminalItemTag, html :: HH.PlainHTML}

terminalItem = {debug, debugString}
  where
  debug html = TerminalItem {tag: DebugTerminalItemTag, html}
  debugString = debug <<< HH.text

-- | # Utilities

tell slotToken slotProxy slotId constr variantProxy a =
  HK.tell slotToken slotProxy slotId $ constr <<< inj variantProxy <<< (a /\ _)

request slotToken slotProxy slotId constr variantProxy =
  HK.request slotToken slotProxy slotId $ constr <<< inj variantProxy
