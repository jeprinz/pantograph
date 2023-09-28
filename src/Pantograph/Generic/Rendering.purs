module Pantograph.Generic.Rendering where

import Pantograph.Generic.Language
import Prelude hiding (div)

import Bug (bug)
import Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, gets, modify, modify_)
import Data.Bifunctor (bimap)
import Data.Const (Const)
import Data.Derivative (differentiate, integrate)
import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.Lazy (Lazy)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Text.Pretty (pretty, pretty1)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- NodeRenderer

type NodeRenderer args ctx env rule joint tooth = Record args -> Array (RenderingM ctx env rule joint tooth (EditorHtml rule joint tooth)) -> RenderingM ctx env rule joint tooth (EditorHtml rule joint tooth)

divOpenExpr :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer (cursor :: OpenExprCursor rule joint tooth) ctx env rule joint tooth
divOpenExpr {cursor} kids = do
  ctx <- ask
  HH.div
    [ HU.classNames ["Expr"] 
    , HE.onClick \event -> do
        H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
        Console.log $ "[Expr:onClick] cursor = " <> pretty1 cursor
        HK.modify_ ctx.buffer_id \_ -> CursorBuffer cursor
    ]
    <$> sequence kids

divOpenSort :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divOpenSort {} kids = do
  HH.div
    [ HU.classNames ["Sort"] ]
    <$> sequence kids

divCursor :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divCursor {} kids = do
  HH.div
    [ HU.classNames ["Cursor"] ]
    <$> sequence kids

divSelectTop :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divSelectTop {} kids = do
  HH.div
    [ HU.classNames ["SelectTop"] ]
    <$> sequence kids

divConsoleLog :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divConsoleLog {label} kids = do
  HH.div
    [ HU.classNames ["ConsoleLog"] ]
    <$> sequence kids

divClipboard :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divClipboard {} kids = do
  HH.div
    [ HU.classNames ["Clipboard"] ]
    <$> sequence kids

divToolbox :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divToolbox {} kids = do
  HH.div
    [ HU.classNames ["Toolbox"] ]
    <$> sequence kids

divPreview :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divPreview {position} kids = do
  HH.div
    [ HU.classNames ["Preview"] ]
    <$> sequence kids

divBuffer :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divBuffer {} kids = do
  HH.div
    [ HU.classNames ["Buffer"] ]
    <$> sequence kids

divEditor :: forall ctx env rule joint tooth. IsEditor rule joint tooth => NodeRenderer _ ctx env rule joint tooth
divEditor  {} kids = do
  HH.div
    [ HU.classNames ["Editor"] ]
    <$> sequence kids

divPlaceholder :: forall ctx env rule joint tooth. String -> NodeRenderer _ ctx env rule joint tooth
divPlaceholder str {} kids = do
  Debug.traceM $ "Placeholder: " <> str
  HH.div
    [ HU.classNames ["Editor"] ]
    <$> sequence kids

-- RenderingCtx, RenderingEnv

type RenderingCtx ctx rule joint tooth =
  { depth :: Int
  , buffer_id :: HK.StateId (Buffer rule joint tooth)
  | ctx }

type RenderingEnv env =
  { holeCount :: Int
  | env }

type RenderingM ctx env rule joint tooth = ReaderT (RenderingCtx ctx rule joint tooth) (StateT (RenderingEnv env) Identity)

runRenderingM :: forall ctx env rule joint tooth a. RenderingCtx ctx rule joint tooth -> RenderingEnv env -> RenderingM ctx env rule joint tooth a -> a
runRenderingM ctx env = flip runReaderT ctx >>> flip evalStateT env >>> unwrap

-- IsEditor

class
    ( IsLanguage rule joint tooth )
    <= IsEditor rule joint tooth
    | rule -> joint tooth, joint -> rule tooth, tooth -> rule joint
  where
  arrangeExpr :: forall ctx env.
    rule ->
    RuleVarSubst (OpenSort joint) ->
    joint (RenderedOpenExpr rule joint tooth) ->
    RenderingM ctx env rule joint tooth (EditorHtml rule joint tooth)

arrangeOpenExpr :: forall ctx env rule joint tooth. IsEditor rule joint tooth =>
  OpenExprPath UpPathDir rule joint tooth ->
  OpenExprJoint rule joint (OpenExprRenderer ctx env rule joint tooth) ->
  OpenExprRenderer ctx env rule joint tooth
arrangeOpenExpr path (Expr rule sigma (Hole holeVar)) = do
  holeIndex <- gets _.holeCount
  modify_ _ {holeCount = holeIndex + 1}
  let expr = Fix (Expr rule sigma (Hole holeVar))
  -- html <- div (RenderedOpenExprConfig {cursor: Cursor path expr}) [pure $ HH.text $ "?" <> show holeIndex]
  html <- divOpenExpr
    {cursor: Cursor path expr} 
    [pure $ HH.text $ "?" <> show holeIndex]
  pure {expr, html}
arrangeOpenExpr path (Expr rule sigma (InjectOpenJoint m_kids)) = do
  kids <- sequence m_kids
  let expr = Fix (Expr rule sigma (InjectOpenJoint (kids <#> _.expr)))
  html <- divOpenExpr
    {cursor: Cursor path expr}
    [arrangeExpr rule sigma kids]
  pure {expr, html}
arrangeOpenExpr path (SymbolExpr str) = do
  let expr = Fix (SymbolExpr str)
  html <- divOpenExpr
    {cursor: Cursor path expr}
    [pure $ HH.text str]
  pure {expr, html}

-- RenderedOpenExpr

type RenderedExpr rule joint tooth = {expr :: Expr rule joint, html :: EditorHtml rule joint tooth }
type RenderedOpenExpr rule joint tooth = {expr :: OpenExpr rule joint, html :: EditorHtml rule joint tooth}
type OpenExprRenderer ctx env rule joint tooth = RenderingM ctx env rule joint tooth (RenderedOpenExpr rule joint tooth)



-- data RenderedExprJoint rule joint a = HtmlJoint

-- EditorHtml

type EditorSlots rule joint (tooth :: Type -> Type) =
  ( editor :: H.Slot EditorQuery EditorOutput EditorSlot
  , buffer :: H.Slot (BufferQuery rule joint) (BufferOutput rule joint) BufferSlot
  , toolbox :: H.Slot (ToolboxQuery rule joint tooth) (ToolboxOutput rule joint tooth) ToolboxSlot
  , preview :: H.Slot (PreviewQuery rule joint tooth) Unit PreviewSlot
  , clipboard :: H.Slot (ClipboardQuery rule joint) (ClipboardOutput rule joint) ClipboardSlot
  , console :: H.Slot ConsoleQuery ConsoleOutput ConsoleSlot )

type EditorHtml rule joint tooth = HH.ComponentHTML (HK.HookM Aff Unit) (EditorSlots rule joint tooth) Aff

-- Editor

type EditorSlot = Unit
type EditorQuery :: Type -> Type
type EditorQuery = Const Void
type EditorInput ctx env rule joint =
  { buffer :: BufferInput ctx env rule joint }
type EditorOutput = Void

editorComponent :: forall ctx env rule joint tooth.
  IsEditor rule joint tooth => Lacks "depth" ctx => Lacks "buffer_id" ctx => Lacks "holeCount" env =>
  H.Component EditorQuery (EditorInput ctx env rule joint) EditorOutput Aff
editorComponent = HK.component \_token input -> HK.do
  HK.pure do
    HH.div 
      [HU.classNames ["Editor"]]
      [HH.slot (Proxy :: Proxy "buffer") unit bufferComponent input.buffer absurd]

-- Buffer

type BufferSlot = Unit
data BufferQuery (rule :: Type) (joint :: Type -> Type) (a :: Type)
type BufferInput ctx env rule joint = 
  { expr :: OpenExpr rule joint
  , ctx :: Record ctx
  , env :: Record env }
type BufferOutput (rule :: Type) (joint :: Type -> Type) = Void

-- | A `Buffer` is an independent section of code.
data Buffer rule joint tooth
  = CursorBuffer (OpenExprCursor rule joint tooth)
  | SelectBuffer (OpenExprSelect rule joint tooth)
  | TopBuffer (OpenExpr rule joint)

bufferComponent :: forall ctx env rule joint tooth. 
  IsEditor rule joint tooth => Lacks "depth" ctx => Lacks "buffer_id" ctx => Lacks "holeCount" env =>
  H.Component (BufferQuery rule joint) (BufferInput ctx env rule joint) (BufferOutput rule joint) Aff
bufferComponent = HK.component \_token input -> HK.do
  buffer /\ buffer_id <- HK.useState (CursorBuffer (Cursor mempty input.expr))
  ctx /\ _ctx_id <- HK.useState 
    ( R.insert (Proxy :: Proxy "depth") 0 
    $ R.insert (Proxy :: Proxy "buffer_id") buffer_id
    $ input.ctx
    :: RenderingCtx ctx rule joint tooth)
  env /\ _env_id <- HK.useState
    ( R.insert (Proxy :: Proxy "holeCount") 0 
    $ input.env
    :: RenderingEnv env )
  HK.pure $ runRenderingM ctx env $
    divBuffer
      {}
      [(renderBuffer buffer) <#> _.html]

renderBuffer :: forall ctx env rule joint tooth. IsEditor rule joint tooth => Buffer rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderBuffer buffer = case buffer of
  CursorBuffer (Cursor path expr) -> renderUpPath mempty path $ renderCursorWrapper $ renderExpr path expr
  SelectBuffer (Select top mid expr) -> renderUpPath mempty top $ renderSelectTopWrapper $ renderSomePath top mid $ renderSelectBotWrapper $ renderExpr (toSomeUpPath mid <> top) expr
  TopBuffer expr -> renderExpr mempty expr

renderCursorWrapper :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderCursorWrapper ren = do
  ctx <- ask
  env <- get
  {expr, html} <- ren
  html' <- 
    divCursor
      {}
      [ pure $ HH.slot (Proxy :: Proxy "toolbox") unit toolboxComponent {ctx, env} handleToolbox
      , pure html ]
  pure {expr, html: html'}
  where
  handleToolbox = case _ of
    SubmitToolboxItem (ToolboxItem _item) -> hole "TODO: handleToolbox SubmitToolboxItem"

renderSelectTopWrapper :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderSelectTopWrapper ren = do
  ctx <- ask
  env <- get
  {expr, html} <- ren
  html' <- divSelectTop
    {}
    [pure html]
  pure {expr, html: html'}

renderSelectBotWrapper :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderSelectBotWrapper ren = do
  ctx <- ask
  env <- get
  {expr, html} <- ren
  html' <- divSelectTop
    {}
    [pure html]
  pure {expr, html: html'}

-- Toolbox

-- | A Toolbox is a drop-down menu of possible edits. It appears when you start
-- | typing at a cursor.

type ToolboxSlot = Unit
type ToolboxInput ctx env rule joint tooth =
  { ctx :: RenderingCtx ctx rule joint tooth
  , env :: RenderingEnv env }
data ToolboxQuery rule joint (tooth :: Type -> Type) a
  = SetToolbox (Array (Array (ToolboxItem rule joint tooth))) a
data ToolboxOutput rule joint tooth
  = SubmitToolboxItem (ToolboxItem rule joint tooth)

type ToolboxItems rule joint tooth = Array (Array (ToolboxItem rule joint tooth))
data ToolboxItem rule joint tooth = ToolboxItem
  { exprHtml :: RenderedExpr rule joint tooth
  , preview :: Preview rule joint tooth
  , buffer :: Lazy (Buffer rule joint tooth) }

toolboxComponent :: forall ctx env rule joint tooth. IsEditor rule joint tooth => H.Component (ToolboxQuery rule joint tooth) (ToolboxInput ctx env rule joint tooth) (ToolboxOutput rule joint tooth) Aff
toolboxComponent = HK.component \token input -> HK.do
  enabled /\ enabled_id <- HK.useState false
  cursor /\ cursor_id <- HK.useState 0
  query /\ query_id <- HK.useState ""
  toolbox /\ toolbox_id <- HK.useState (Nothing :: Maybe (ToolboxItems rule joint tooth))

  HK.useQuery token.queryToken case _ of
    SetToolbox items a -> do
      HK.modify_ toolbox_id \_ -> Just items
      HK.modify_ cursor_id \_ -> 0
      pure $ Just a

  HK.pure $ runRenderingM input.ctx input.env $ case toolbox of
    Nothing ->
      divToolbox
        {}
        []
    Just itemRows ->
      divToolbox
        {}
        [ divPlaceholder "toolbox query"
            {}
            [ pure $ HH.input
                [ HE.onValueChange \event -> do
                    Debug.traceM "-------------------------------------------"
                    lift $ Console.log $ "[onValueChange] event = " <> show event
                    -- TODO: update query
                    pure unit ] ]
        , divPlaceholder "toolbox item rows"
            {}
            (itemRows <#> \itemRow ->
              divPlaceholder "toolbox item row"
                {}
                (itemRow <#> \(ToolboxItem item) -> 
                  divPlaceholder "toolbox item"
                    {}
                    [pure item.exprHtml.html])) ]

-- Preview

-- | A Preview is on either side of the cursor when an insertion edit is being
-- | considered. It renders the result of the insertion edit in-place either
-- | outside the cursor for an outwards insertion or inside the cursor for an
-- | inwards.

type PreviewSlot = PreviewPosition
data PreviewQuery rule joint tooth a
  = SetPreview (Preview rule joint tooth) a
type PreviewInput ctx env rule joint tooth =
  { position :: PreviewPosition
  , ctx :: RenderingCtx ctx rule joint tooth
  , env :: RenderingEnv env }
data PreviewOutput rule joint

data PreviewPosition 
  = InsertOutsideBeforePreviewPosition | InsertOutsideAfterPreviewPosition
  | InsertInsideBeforePreviewPosition | InsertInsideAfterPreviewPosition
  | ReplacePreviewPosition

data Preview rule joint tooth
  = PreviewInsert (tooth (OpenExpr rule joint))
  | PreviewReplace (OpenExpr rule joint)

previewComponent :: forall ctx env rule joint tooth. IsEditor rule joint tooth => H.Component (PreviewQuery rule joint tooth) (PreviewInput ctx env rule joint tooth) (PreviewOutput rule joint) Aff
previewComponent = HK.component \_token input -> HK.do
  preview /\ _preview_id <- HK.useState (Nothing :: Maybe (Preview rule joint tooth))
  HK.pure do
    runRenderingM input.ctx input.env $
      divPreview
        {position: input.position}
        [pure $ HH.text "TODO: previewComponent"]

-- Clipboard

type ClipboardSlot = Unit
data ClipboardQuery rule joint a
  = SetClipboard (Clipboard rule joint) a
  | GetClipboard (Clipboard rule joint -> a)
type ClipboardInput ctx env rule joint tooth =
  { ctx :: RenderingCtx ctx rule joint tooth
  , env :: RenderingEnv env }
data ClipboardOutput rule joint

data Clipboard rule joint
  = ExprClipboard (OpenExpr rule joint)
  | PathClipboard (SomePath joint (OpenExpr rule joint))

clipboardComponent :: forall ctx env rule joint tooth. IsEditor rule joint tooth => H.Component (ClipboardQuery rule joint) (ClipboardInput ctx env rule joint tooth) (ClipboardOutput rule joint) Aff
clipboardComponent = HK.component \_token input -> HK.do
  clipboard /\ _clipboard_id <- HK.useState (Nothing :: Maybe (Clipboard rule joint))
  HK.pure $ runRenderingM input.ctx input.env $
    divClipboard
      {}
      [pure $ HH.text "TODO: clipboardComponent"]

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
consoleComponent = HK.component \_token _ -> HK.do
  logs /\ _logs_id <- HK.useState (mempty :: List ConsoleLog)
  HK.pure do
    HH.div
      [HU.classNames ["Console"]]
      (List.toUnfoldable $ logs <#> \log -> 
        HH.div 
          [HU.classNames ["ConsoleItem"]]
          [bimap absurd absurd log.html])

-- render Language pieces

renderTooth :: forall ctx env rule joint tooth. IsEditor rule joint tooth =>  OpenExprPath UpPathDir rule joint tooth -> OpenExprTooth rule joint tooth (OpenExpr rule joint) -> OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderTooth path (Expr' rule sigma th) ren = do
  Debug.traceM $ "[renderTooth] path, th = " <> pretty1 path <> ", " <> pretty1 th
  exprHtml <- ren
  let kids = integrate (Right exprHtml.expr) (Left <$> th)
  arrangeOpenExpr path (Expr rule sigma (differentiate kids <#> \(kid /\ th') -> case kid of
    Left kid' -> renderExpr (consPath (Expr' rule sigma (either identity identity <$> th')) path) kid'
    Right _ -> pure exprHtml))

renderUpPath :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprPath UpPathDir rule joint tooth -> OpenExprPath UpPathDir rule joint tooth -> OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderUpPath up (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> do
    Debug.traceM $ "[renderUpPath] th = " <> pretty1 th
    let path' = Path ths'
    renderUpPath up path' (renderTooth (path' <> up) th inside)

renderDownPath :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprPath UpPathDir rule joint tooth -> OpenExprPath DownPathDir rule joint tooth -> OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderDownPath up (Path ths) inside = case ths of
  List.Nil -> inside
  List.Cons th ths' -> do
    let up' = consPath th up
    renderTooth up' th (renderDownPath up' (Path ths') inside)

renderSomePath :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprPath UpPathDir rule joint tooth -> SomeOpenExprPath rule joint tooth -> OpenExprRenderer ctx env rule joint tooth -> OpenExprRenderer ctx env rule joint tooth
renderSomePath up (UpPath p) = renderUpPath up p
renderSomePath up (DownPath p) = renderDownPath up p

renderExpr :: forall ctx env rule joint tooth. IsEditor rule joint tooth => OpenExprPath UpPathDir rule joint tooth -> OpenExpr rule joint -> OpenExprRenderer ctx env rule joint tooth
renderExpr path expr@(Fix (Expr rule sigma kids)) = do
  Debug.traceM $ "[renderExpr] path, expr = " <> pretty1 path <> ", " <> pretty expr
  arrangeOpenExpr path (Expr rule sigma (differentiate kids <#> \(kid /\ th) -> renderExpr (consPath (Expr' rule sigma th) path) kid))
renderExpr path expr@(Fix (SymbolExpr str)) = do
  Debug.traceM $ "[renderExpr] path, expr = " <> pretty1 path <> ", " <> pretty expr
  arrangeOpenExpr path (SymbolExpr str)