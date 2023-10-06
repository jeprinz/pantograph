module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util
import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT)
import Data.Foldable (foldM, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Pantograph.Generic.Language (Sort)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as R
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- sync

syncExprGyro :: forall el ed sn. ExprGyro el ed sn -> ExprGyro el (SyncExprData ed) sn
syncExprGyro = map \(ExprNode node) -> ExprNode node {dat = R.union {elemId: HU.freshElementId unit} node.dat}

-- hydrate

type HydrateM = M (gyroPosition :: GyroPosition) () (HK.HookM Aff)

hydrateExprGyro :: forall el ed sn. ExprGyro el (SyncExprData ed) sn -> HK.HookM Aff Unit
hydrateExprGyro = case _ of
  (RootGyro expr) -> do
    void 
      $ runM {gyroPosition: InsideRoot} {}
      $ hydrateExpr expr
  (CursorGyro (Cursor cursor)) -> do
    void
      $ runM {gyroPosition: OutsideCursor} {}
      $ hydrateExprPath cursor.outside
    void
      $ runM {gyroPosition: AtCursor} {}
      $ hydrateExpr cursor.inside
  (SelectGyro (Select select)) -> do
    void
      $ runM {gyroPosition: OutsideSelect} {}
      $ hydrateExprPath select.outside
    void
      $ runM {gyroPosition: AtOutsideSelect} {}
      $ hydrateExprPath select.middle
    void
      $ runM {gyroPosition: AtInsideSelect} {}
      $ hydrateExpr select.inside

hydrateExpr :: forall el ed sn. Expr el (SyncExprData ed) sn -> HydrateM Unit
hydrateExpr expr = hydrateExprNode `traverse_` expr

hydrateExprPath :: forall el ed sn. ExprPath el (SyncExprData ed) sn -> HydrateM Unit
hydrateExprPath (Path Nil) = pure unit
hydrateExprPath (Path (Cons (Tooth tooth) ts)) = do
  hydrateExprNode tooth.node
  local (\ctx -> ctx {gyroPosition = kidGyroPosition ctx.gyroPosition}) $
    hydrateExprPath (Path ts)

hydrateExprNode :: forall el ed sn. ExprNode el (SyncExprData ed) sn -> HydrateM Unit
hydrateExprNode (ExprNode node) = do
  ctx <- ask
  void $ liftEffect $ HU.setClassName node.dat.elemId $ toClassName ctx.gyroPosition

-- | Given the parent's GyroPosition, naively computes each kid's GyroPosition.
-- | For the following transitions, need to be handled before calling the
-- | appropriate hydrate functions.
kidGyroPosition :: GyroPosition -> GyroPosition
kidGyroPosition = case _ of
  InsideRoot -> InsideRoot
  OutsideCursor -> OutsideCursor
  AtCursor -> InsideCursor
  InsideCursor -> InsideCursor
  OutsideSelect -> OutsideCursor
  AtOutsideSelect -> BetweenSelect
  BetweenSelect -> BetweenSelect
  AtInsideSelect -> InsideSelect
  InsideSelect -> InsideSelect

-- render

renderExprGyro :: forall ctx env el ed sn. Renderer ctx env el ed sn -> ExprGyro el (SyncExprData ed) sn -> RenderM ctx env el ed sn (BufferHtml el ed sn)
renderExprGyro renderer (RootGyro expr) = renderExpr renderer expr
renderExprGyro renderer (CursorGyro (Cursor cursor)) = hole "TODO: renderExprGyro"
renderExprGyro renderer (SelectGyro (Select select)) = hole "TODO: renderExprGyro"

renderExprNode :: forall ctx env el ed sn.
  Renderer ctx env el ed sn ->
  ExprNode el (SyncExprData ed) sn ->
  { arrangedKids :: Array (ArrangeKid el ed sn (BufferHtml el ed sn)) } ->
  RenderM ctx env el ed sn (BufferHtml el ed sn)
renderExprNode (Renderer renderer) (ExprNode node) {arrangedKids} = do
  -- TODO: do some styling to reflect what kind of ArrangeKid they are, for the same of highlighting
  let htmlKids = arrangedKids # foldMap case _ of
        (ExprKidArrangeKid html) -> [html]
        (PunctuationArrangeKid htmls) -> htmls
        (IndentationArrangeKid htmls) -> htmls
  
  pure $ HH.div
    [ HU.id $ node.dat.elemId
    , HE.onMouseDown \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        hole "TODO: renderExpr onMouseDown"
    , HE.onMouseUp \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        hole "TODO: renderExpr onMouseUp"
    ]
    htmlKids

renderExpr :: forall ctx env el ed sn. Renderer ctx env el ed sn -> Expr el (SyncExprData ed) sn -> RenderM ctx env el ed sn (BufferHtml el ed sn)
renderExpr (Renderer renderer) (Tree expr@{node: ExprNode node}) = do
  let arrangeKids = expr.kids <#> \kid@(Tree {node}) -> renderExpr (Renderer renderer) kid <#> (_ /\ node)
  arrangedKids <- renderer.arrangeExpr expr.node arrangeKids
  renderExprNode (Renderer renderer) (ExprNode node)
    {arrangedKids}

-- renderExprPath :: forall ctx env el ed sn. Renderer ctx env el ed sn -> ExprPath el (SyncExprData ed) sn -> RenderM ctx env el ed sn (BufferHtml el ed sn) -> RenderM ctx env el ed sn (BufferHtml el ed sn)
-- renderExprPath (Renderer renderer) (Path Nil) inside = inside
-- renderExprPath (Renderer renderer) (Path (Cons (Tooth tooth) ts)) inside = hole inside

-- component

bufferComponent :: forall el ed sn ctx env. H.Component (BufferQuery el ed sn) (BufferInput ctx env el ed sn) (BufferOutput el ed sn) Aff
bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  renderCtx /\ renderCtxStateId <- HK.useState $
    R.union
      { depth: 0 }
      renderer.topCtx 
  renderEnv /\ renderEnvStateId <- HK.useState $
    R.union
      { holeCount: 0 }
      renderer.topEnv

  exprGyro /\ exprGyroStateId <- HK.useState $ RootGyro input.expr

  -- runs before each render
  let syncedExprGyro = syncExprGyro exprGyro
  let gyroHtml /\ _ = unwrap $ runM renderCtx renderEnv $ renderExprGyro (Renderer renderer) syncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydrateExprGyro syncedExprGyro
    pure Nothing

  -- render
  HK.pure $ do
    HH.div 
      [HP.classes [HH.ClassName "Panel Buffer"]]
      [ HH.div
          [HP.classes [HH.ClassName "PanelHeader"]]
          [ HH.div
              [ HP.classes [HH.ClassName "button"]
              , HE.onClick \_ -> Console.log "TODO: close buffer" ]
              [HH.text "X"]
          , HH.text "Buffer" ]
      , HH.div
          [HP.classes [HH.ClassName "PanelContent"]]
          [gyroHtml] ]


{-

-- NOTE: this is old work i did to combine sync+hydrate+render into one pass


-- abstract

arrangeExpr :: forall ctx env el ed ed' sn a.
  ExprNode el ed' sn ->
  Array (RenderM ctx env el ed sn (a /\ ExprNode el ed' sn)) ->
  RenderM ctx env el ed sn (Array (ArrangeKid el ed sn a))
arrangeExpr = hole "TODO: placeholder arrangeExpr"

-- functions

syncExprNode :: forall ctx env el ed ed' sn.
  ExprNode el ed' sn ->
  M ctx env el ed sn (ExprNode el (SyncExprData ed') sn)
syncExprNode = hole "TODO: hydrateExprNode"

hydrateExprNode :: forall ctx env el ed ed' sn.
  ExprNode el (SyncExprData ed') sn ->
  HydrateM ctx env el ed sn  (ExprNode el (HydrateExprData ed') sn)
hydrateExprNode = hole "TODO: hydrateExprNode"

setupExprKid :: forall ctx env el ed ed' sn a.
  ExprNode el ed' sn ->
  HydrateM ctx env el ed sn a ->
  HydrateM ctx env el ed sn a
setupExprKid = hole "TODO: setupExprKid"

hydrateClassName :: forall el ed sn. ExprNode el (HydrateExprData ed) sn -> HH.ClassName
hydrateClassName = hole "TODO: hydrateClassName"

-- sync, hydrate, render
renderExpr :: forall ctx env el ed sn.
  Expr el (SyncExprData ed) sn ->
  RenderHydrateM ctx env el ed sn {html :: BufferHtml el ed sn, hydratedExpr :: Expr el (SyncExprData ed) sn}
renderExpr (Tree expr) = do
  hydratedExprNode <- -- :: ExprNode el (HydrateExprData (SyncExprData ed)) sn <- 
    expr.node # (syncExprNode >=> hydrateExprNode)
  
  arrangedHtmls <- do
    let 
      -- arrangeKids :: Array (M (RenderCtx el ed sn ctx) (RenderEnv el ed sn env) el ed sn ({html :: BufferHtml el ed sn, expr :: Expr el (HydrateExprData (SyncExprData ed)) sn} /\ ExprNode el (HydrateExprData (SyncExprData ed)) sn))
      arrangeKids = expr.kids <#>
        renderExpr >>>
        setupExprKid expr.node >>>
        map \renderedKid@{hydratedExpr: Tree hydratedKid} -> renderedKid /\ hydratedKid.node
    arrangeExpr hydratedExprNode arrangeKids

  let
    htmlKids = arrangedHtmls # foldMap case _ of
      ExprKidArrangeKid {html} -> [html]
      PunctuationArrangeKid htmls -> htmls
      IndentationArrangeKid htmls -> htmls

  let
    hydratedExprKids :: Array (Expr el (HydrateExprData (SyncExprData ed)) sn)
    hydratedExprKids = arrangedHtmls # foldMap case _ of
      ExprKidArrangeKid {hydratedExpr: hydratedKid} -> [hydratedKid]
      PunctuationArrangeKid _ -> []
      IndentationArrangeKid _ -> []

  let
    html = HH.div
      [ HU.id $ (unwrap hydratedExprNode).dat.elemId
      , HP.class_ $ hydrateClassName hydratedExprNode
      , HE.onMouseDown \mouseEvent -> do
          liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
          hole "TODO: renderExpr onMouseDown"
      , HE.onMouseUp \mouseEvent -> do
          liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
          hole "TODO: renderExpr onMouseUp"
      ]
      htmlKids

  pure
    { html
    , hydratedExpr: Tree {node: hydratedExprNode, kids: hydratedExprKids} }

renderGyro = case _ of
  RootGyro expr -> do
    {hydratedExpr, html} <- renderExpr expr
    pure {hydratedGyro: RootGyro hydratedExpr, html}
  CursorGyro _ -> hole "TODO"
  SelectGyro _ -> hole "TODO"

-}