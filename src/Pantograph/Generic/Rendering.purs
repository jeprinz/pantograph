module Pantograph.Generic.Rendering where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, local, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Bifunctor (bimap)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as Prop
import Halogen.Hooks as HK
import Hole (hole)
import Prim.Row (class Lacks, class Nub)
import Record as R
import Text.Pretty (class Pretty)
import Type.Proxy (Proxy(..))

html :: forall w i. EditorHtmlConfig -> Array (HH.HTML w i) -> HH.HTML w i
html config kids = HH.div attrs kids
  where
  attrs = case config of
    TermHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Term"] ]
    SortHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Sort"] ]
    CursorHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Cursor"] ]
    SelectTopHtmlConfig args ->
      [ Prop.classes [HH.ClassName "SelectTop"] ]
    SelectBotHtmlConfig args ->
      [ Prop.classes [HH.ClassName "SelectBot"] ]
    ConsoleHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Console"] ]
    ConsoleLogHtmlConfig args ->
      [ Prop.classes [HH.ClassName "ConsoleLog"] ]
    ClipboardHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Clipboard"] ]
    ToolboxHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Toolbox"] ]
    PreviewHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Preview"] ]
    EditorHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Editor"] ]

data EditorHtmlConfig
  = TermHtmlConfig {}
  | SortHtmlConfig {}
  | CursorHtmlConfig {}
  | SelectTopHtmlConfig {}
  | SelectBotHtmlConfig {}
  | ConsoleHtmlConfig {}
  | ConsoleLogHtmlConfig {label :: ConsoleLogLabel}
  | ClipboardHtmlConfig {}
  | ToolboxHtmlConfig {}
  | PreviewHtmlConfig {position :: PreviewPosition}
  | EditorHtmlConfig {}

-- RenderingCtx, RenderingEnv

type RenderingCtx ctx =
  { depth :: Int
  | ctx }

type RenderingEnv env =
  { holeCount :: Int
  | env }

type RenderingM ctx env = ReaderT (RenderingCtx ctx) (StateT (RenderingEnv env) Identity)

runRenderingM :: forall ctx env rule joint a. IsEditor ctx env rule joint => RenderingCtx ctx -> RenderingEnv env -> RenderingM ctx env a -> a
runRenderingM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

-- IsEditor

class
    ( IsLanguage rule joint
    , Nub ctx ctx, Lacks "depth" ctx
    , Nub env env, Lacks "holeCount" env )
    <= IsEditor (ctx :: Row Type) (env :: Row Type) rule joint 
    | joint -> ctx env rule, ctx -> joint env rule, env -> joint ctx rule, rule -> joint ctx env
  where
  arrangeTerm :: TermJoint rule joint (TermRenderer ctx env rule joint) -> TermRenderer ctx env rule joint

-- TermHtml

type TermHtml rule joint = {term :: Term rule joint, html :: EditorHtml rule joint}

type TermRenderer ctx env rule joint = RenderingM ctx env (TermHtml rule joint)

-- EditorHtml

type EditorSlots rule joint = 
  ( toolbox :: H.Slot (ToolboxQuery rule joint) (ToolboxOutput rule joint) ToolboxSlot
  , preview :: H.Slot (PreviewQuery rule joint) Unit PreviewSlot
  , clipboard :: H.Slot (ClipboardQuery rule joint) (ClipboardOutput rule joint) ClipboardSlot
  , console :: H.Slot ConsoleQuery ConsoleOutput ConsoleSlot )

type EditorHtml rule joint = HH.ComponentHTML (HK.HookM Aff Unit) (EditorSlots rule joint) Aff

-- Editor

type EditorSlot = Unit
data EditorQuery rule joint a
type EditorInput ctx env rule joint = 
  { term :: Term rule joint
  , ctx :: Record ctx
  , env :: Record env
  }
data EditorOutput rule joint

-- | A `Buffer` is an independent section of code.
data Buffer rule joint
  = CursorBuffer (TermCursor rule joint)
  | SelectBuffer (TermSelect rule joint)
  | TopBuffer (Term rule joint)

editorComponent :: forall ctx env rule joint. IsEditor ctx env rule joint => H.Component (EditorQuery rule joint) (EditorInput ctx env rule joint) (EditorOutput rule joint) Aff
editorComponent = HK.component \token input -> HK.do
  buffer /\ buffer_id <- HK.useState (TopBuffer input.term)
  ctx /\ ctx_id <- HK.useState 
    ( R.insert (Proxy :: Proxy "depth") 0 
    $ input.ctx
    :: RenderingCtx ctx )
  env /\ env_id <- HK.useState
    ( R.insert (Proxy :: Proxy "holeCount") 0 
    $ input.env
    :: RenderingEnv env )
  HK.pure do
    html
      (EditorHtmlConfig {}) 
      [(runRenderingM ctx env (renderBuffer buffer)).html]

renderBuffer :: forall ctx env rule joint. IsEditor ctx env rule joint => Buffer rule joint -> TermRenderer ctx env rule joint
renderBuffer buffer = case buffer of
  CursorBuffer (Cursor path term) -> renderUpPath path (renderTerm term)
  SelectBuffer (Select top mid term) -> renderUpPath top (renderSomePath mid (renderTerm term))
  TopBuffer term -> renderTerm term

renderCursorWrapper :: forall ctx env rule joint. IsEditor ctx env rule joint => TermRenderer ctx env rule joint -> TermRenderer ctx env rule joint
renderCursorWrapper = map \termHtml -> termHtml {html = html (CursorHtmlConfig {}) [termHtml.html]}

renderTooth :: forall ctx env rule joint. IsEditor ctx env rule joint => TermTooth rule joint -> TermRenderer ctx env rule joint -> TermRenderer ctx env rule joint
renderTooth (Tooth j) inside = arrangeTerm (j <#> maybe inside renderTerm)

renderUpPath :: forall ctx env rule joint. IsEditor ctx env rule joint => TermPath UpPathDir rule joint -> TermRenderer ctx env rule joint -> TermRenderer ctx env rule joint
renderUpPath (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> renderUpPath (Path ths') (renderTooth th inside)

renderDownPath :: forall ctx env rule joint. IsEditor ctx env rule joint => TermPath DownPathDir rule joint -> TermRenderer ctx env rule joint -> TermRenderer ctx env rule joint
renderDownPath (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> renderTooth th (renderDownPath (Path ths') inside)

renderSomePath :: forall ctx env rule joint. IsEditor ctx env rule joint => SomeTermPath rule joint -> TermRenderer ctx env rule joint -> TermRenderer ctx env rule joint
renderSomePath (UpPath p) = renderUpPath p
renderSomePath (DownPath p) = renderDownPath p

renderTerm :: forall ctx env rule joint. IsEditor ctx env rule joint => Term rule joint -> TermRenderer ctx env rule joint
renderTerm (Fix (Term rule sigma j)) = arrangeTerm (Term rule sigma (renderTerm <$> j))

-- Toolbox

-- | A Toolbox is a drop-down menu of possible edits. It appears when you start
-- | typing at a cursor.

type ToolboxSlot = Unit
type ToolboxInput = Unit
data ToolboxQuery rule joint a
  = SetToolbox
data ToolboxOutput rule joint

data ToolboxValue rule joint

toolboxComponent :: forall rule joint. H.Component (ToolboxQuery rule joint) ToolboxInput (ToolboxOutput rule joint) Aff
toolboxComponent = HK.component \token pos -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (ToolboxValue rule joint))
  HK.pure do
    html
      (ToolboxHtmlConfig {})
      [HH.text "TODO: toolboxComponent"]

-- Preview

-- | A Preview is on either side of the cursor when an insertion edit is being
-- | considered. It renders the result of the insertion edit in-place either
-- | outside the cursor for an outwards insertion or inside the cursor for an
-- | inwards.

type PreviewSlot = PreviewPosition
data PreviewQuery rule joint a
  = SetPreviewValue (PreviewValue rule joint) a
type PreviewInput = PreviewPosition
data PreviewOutput rule joint

data PreviewPosition 
  = OutsideBeforePreviewPosition | OutsideAfterPreviewPosition
  | InsideBeforePreviewPosition | InsideAfterPreviewPosition

data PreviewValue rule joint = PreviewValue (TermTooth rule joint)

previewComponent :: forall rule joint. H.Component (PreviewQuery rule joint) PreviewInput (PreviewOutput rule joint) Aff
previewComponent = HK.component \token pos -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (PreviewValue rule joint))
  HK.pure do
    html
      (PreviewHtmlConfig {position: pos})
      [HH.text "TODO: previewComponent"]

-- Clipboard

type ClipboardSlot = Unit
data ClipboardQuery rule joint a
  = SetClipboardValue (ClipboardValue rule joint) a
  | GetClipboardValue (ClipboardValue rule joint -> a)
type ClipboardInput = Unit
data ClipboardOutput rule joint

data ClipboardValue rule joint
  = TermClipboardValue (Term rule joint)
  | PathClipboardValue (SomePath joint (Term rule joint))

clipboardComponent :: forall rule joint. H.Component (ClipboardQuery rule joint) ClipboardInput (ClipboardOutput rule joint) Aff
clipboardComponent = HK.component \token _ -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (ClipboardValue rule joint))
  HK.pure do
    html
      (ClipboardHtmlConfig {})
      [HH.text "TODO: clipboardComponent"]

-- Console

type ConsoleSlot = Unit
data ConsoleQuery a
  = LogConsole HH.PlainHTML a
  | ClearConsole a
type ConsoleInput = Unit
data ConsoleOutput

type ConsoleLog = {label :: ConsoleLogLabel, html :: HH.PlainHTML}
data ConsoleLogLabel = ErrorConsoleLogLabel | WarningConsoleLogLabel | InfoConsoleLogLabel | DebugConsoleLogLabel

consoleComponent :: H.Component ConsoleQuery ConsoleInput ConsoleOutput Aff
consoleComponent = HK.component \token _ -> HK.do
  logs /\ logs_id <- HK.useState (mempty :: List ConsoleLog)
  HK.pure do
    html
      (ConsoleHtmlConfig {})
      (List.toUnfoldable $ logs <#> \log -> 
        html
          (ConsoleLogHtmlConfig {label: log.label})
          [bimap absurd absurd log.html])