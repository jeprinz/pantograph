module Pantograph.Generic.Rendering where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Control.Monad.Reader (ReaderT, local, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Data.Bifunctor (bimap)
import Data.Derivative (integrate)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
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

data EditorHtmlConfig
  = HoleExprHtmlConfig {}
  | SymbolHtmlConfig {}
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

html :: forall w i. EditorHtmlConfig -> Array (HH.HTML w i) -> HH.HTML w i
html config kids = HH.div attrs kids
  where
  attrs = case config of
    HoleExprHtmlConfig args ->
      [ Prop.classes [HH.ClassName "Expr"] ]
    SymbolHtmlConfig args ->
      [ Prop.classes [HH.ClassName "SymbolExpr"] ]
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

-- RenderingCtx, RenderingEnv

type RenderingCtx ctx =
  { depth :: Int
  | ctx }

type RenderingEnv env =
  { holeCount :: Int
  | env }

type RenderingM ctx env = ReaderT (RenderingCtx ctx) (StateT (RenderingEnv env) Identity)

runRenderingM :: forall ctx env a. RenderingCtx ctx -> RenderingEnv env -> RenderingM ctx env a -> a
runRenderingM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

-- IsEditor

class
    ( IsLanguage rule joint joint' )
    <= IsEditor rule joint joint'
    | rule -> joint joint', joint -> rule joint', joint' -> rule joint
  where
  arrangeExpr :: forall ctx env.
    rule ->
    Subst RuleVar (HoleSort joint) ->
    joint (HoleExprHtml rule joint joint') ->
    RenderingM ctx env (EditorHtml rule joint joint')

arrangeHoleExpr :: forall ctx env rule joint joint'. IsEditor rule joint joint' =>
  HoleExprJoint rule joint (HoleExprRenderer ctx env rule joint joint') ->
  HoleExprRenderer ctx env rule joint joint'
arrangeHoleExpr (Expr rule sigma (Hole holeVar)) =
  pure
    { expr: Fix (Expr rule sigma (Hole holeVar))
    , html: html (HoleExprHtmlConfig {}) [] }
arrangeHoleExpr (Expr rule sigma (InjectHoleJoint m_kids)) = do
  kids <- sequence m_kids
  html <- arrangeExpr rule sigma kids
  pure
    { expr: Fix (Expr rule sigma (InjectHoleJoint (kids <#> _.expr)))
    , html }
arrangeHoleExpr (SymbolExpr str) =
  pure
    { expr: Fix (SymbolExpr str)
    , html: html (SymbolHtmlConfig {}) [HH.text str] }

-- HoleExprHtml

type HoleExprHtml rule joint joint' = {expr :: HoleExpr rule joint, html :: EditorHtml rule joint joint'}
type ExprHtml rule joint joint' = {expr :: Expr rule joint, html :: EditorHtml rule joint joint'}

type HoleExprRenderer ctx env rule joint joint' = RenderingM ctx env (HoleExprHtml rule joint joint')

-- EditorHtml

type EditorSlots rule joint joint' =
  ( toolbox :: H.Slot (ToolboxQuery rule joint joint') (ToolboxOutput rule joint joint') ToolboxSlot
  , preview :: H.Slot (PreviewQuery rule joint joint') Unit PreviewSlot
  , clipboard :: H.Slot (ClipboardQuery rule joint) (ClipboardOutput rule joint) ClipboardSlot
  , console :: H.Slot ConsoleQuery ConsoleOutput ConsoleSlot )

type EditorHtml rule joint joint' = HH.ComponentHTML (HK.HookM Aff Unit) (EditorSlots rule joint joint') Aff

-- Editor

type EditorSlot = Unit
data EditorQuery rule joint a
type EditorInput ctx env rule joint = 
  { expr :: HoleExpr rule joint
  , ctx :: Record ctx
  , env :: Record env }
data EditorOutput rule joint

-- | A `Buffer` is an independent section of code.
data Buffer rule joint joint'
  = CursorBuffer (HoleExprCursor rule joint joint')
  | SelectBuffer (HoleExprSelect rule joint joint')
  | TopBuffer (HoleExpr rule joint)

editorComponent :: forall ctx env rule joint joint'. 
  IsEditor rule joint joint' =>
  Lacks "depth" ctx =>
  Lacks "holeCount" env =>
  H.Component (EditorQuery rule joint) (EditorInput ctx env rule joint) (EditorOutput rule joint) Aff
editorComponent = HK.component \token input -> HK.do
  buffer /\ buffer_id <- HK.useState (TopBuffer input.expr)
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

renderBuffer :: forall ctx env rule joint joint'. IsEditor rule joint joint' => Buffer rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderBuffer buffer = case buffer of
  CursorBuffer (Cursor path expr) -> renderUpPath path (renderExpr expr)
  SelectBuffer (Select top mid expr) -> renderUpPath top (renderSomePath mid (renderExpr expr))
  TopBuffer expr -> renderExpr expr

renderCursorWrapper :: forall ctx env rule joint joint'. IsEditor rule joint joint' => HoleExprRenderer ctx env rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderCursorWrapper = map \exprHtml -> exprHtml {html = html (CursorHtmlConfig {}) [exprHtml.html]}

-- Toolbox

-- | A Toolbox is a drop-down menu of possible edits. It appears when you start
-- | typing at a cursor.

type ToolboxSlot = Unit
type ToolboxInput = Unit
data ToolboxQuery rule joint joint' a
  = SetToolbox
data ToolboxOutput rule joint joint'

data ToolboxValue rule joint joint'

toolboxComponent :: forall ctx env rule joint joint'. IsEditor rule joint joint' => H.Component (ToolboxQuery rule joint joint') ToolboxInput (ToolboxOutput rule joint joint') Aff
toolboxComponent = HK.component \token pos -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (ToolboxValue rule joint joint'))
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
data PreviewQuery rule joint joint' a
  = SetPreviewValue (PreviewValue rule joint joint') a
type PreviewInput = PreviewPosition
data PreviewOutput rule joint

data PreviewPosition 
  = OutsideBeforePreviewPosition | OutsideAfterPreviewPosition
  | InsideBeforePreviewPosition | InsideAfterPreviewPosition

data PreviewValue rule joint joint' = PreviewValue (joint' (HoleExpr rule joint))

previewComponent :: forall rule joint joint'. H.Component (PreviewQuery rule joint joint') PreviewInput (PreviewOutput rule joint) Aff
previewComponent = HK.component \token pos -> HK.do
  value /\ value_id <- HK.useState (Nothing :: Maybe (PreviewValue rule joint joint'))
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
  = ExprClipboardValue (HoleExpr rule joint)
  | PathClipboardValue (SomePath joint (HoleExpr rule joint))

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

-- render Language pieces

renderTooth :: forall ctx env rule joint joint'. IsEditor rule joint joint' => HoleExprJoint' rule joint joint' (HoleExpr rule joint) -> HoleExprRenderer ctx env rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderTooth j' inside = arrangeHoleExpr $ integrate inside (renderExpr <$> j')

renderUpPath :: forall ctx env rule joint joint'. IsEditor rule joint joint' => HoleExprPath UpPathDir rule joint joint' -> HoleExprRenderer ctx env rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderUpPath (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> renderUpPath (Path ths') (renderTooth th inside)

renderDownPath :: forall ctx env rule joint joint'. IsEditor rule joint joint' => HoleExprPath DownPathDir rule joint joint' -> HoleExprRenderer ctx env rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderDownPath (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> renderTooth th (renderDownPath (Path ths') inside)

renderSomePath :: forall ctx env rule joint joint'. IsEditor rule joint joint' => SomeHoleExprPath rule joint joint' -> HoleExprRenderer ctx env rule joint joint' -> HoleExprRenderer ctx env rule joint joint'
renderSomePath (UpPath p) = renderUpPath p
renderSomePath (DownPath p) = renderDownPath p

renderExpr :: forall ctx env rule joint joint'. IsEditor rule joint joint' => HoleExpr rule joint -> HoleExprRenderer ctx env rule joint joint'
renderExpr (Fix (Expr rule sigma j)) = arrangeHoleExpr (Expr rule sigma (renderExpr <$> j))
renderExpr (Fix (SymbolExpr str)) = arrangeHoleExpr (SymbolExpr str)