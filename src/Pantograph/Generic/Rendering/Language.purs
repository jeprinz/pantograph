module Pantograph.Generic.Rendering.Language where

import Data.Tree
import Pantograph.Generic.Language
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Keyboard
import Prelude
import Util

import Control.Monad.Reader (ask, local)
import DOM.HTML.Indexed as HPI
import Control.Monad.State (get)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Record as R
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- sync

syncExprGyro :: forall sn el er. AnnExprGyro sn el er -> SyncExprGyro sn el er
syncExprGyro = map \(AnnExprNode node) -> AnnExprNode $ R.union {elemId: HU.freshElementId unit} node

-- render

type MakeAnnExprProps sn el er ctx env =
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  RenderM sn el ctx env (Array (HH.IProp HPI.HTMLdiv (HK.HookM Aff Unit)))

type MakeSyncExprProps sn el er ctx env = MakeAnnExprProps sn el (SyncExprRow sn el er) ctx env

renderAnnExprHelper :: forall sn el er ctx env.
  Show sn => Show el => PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  Array (ArrangeKid sn el (Array (BufferHtml sn el))) ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprHelper renderer outside expr@(Tree {node: AnnExprNode node}) makeAnnExprProps arrangedKids = do
  props <- makeAnnExprProps renderer outside expr
  let htmls = arrangedKids # foldMap case _ of
        ExprKidArrangeKid html -> html
        PunctuationArrangeKid htmls' -> htmls'
        IndentationArrangeKid htmls' -> 
          [ HH.span [HP.classes [HH.ClassName "newline-header"]] [HH.text "↪"]
          , HH.br_ ] <>
          htmls'
  pure $ [HH.div props htmls]

renderAnnExpr :: forall sn el er ctx env.
  Show sn => Show el => PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExpr (Renderer renderer) outside expr@(Tree {node, kids}) makeAnnExprProps = do
  arrangedKids <- renderer.arrangeExpr node $
    kids # Array.mapWithIndex \i kid@(Tree {node: kidNode}) -> do
      let tooth = Tooth {node, i, kids: deleteAt "renderAnnExpr" i kids}
      local
        ( R.modify (Proxy :: Proxy "depth") (1 + _) )
        $ renderAnnExpr (Renderer renderer) (consPath outside tooth) kid makeAnnExprProps <#> (_ /\ kidNode)
  renderAnnExprHelper (Renderer renderer) outside expr makeAnnExprProps arrangedKids

renderAnnExprPath :: forall sn el er ctx env.
  Show sn => Show el => PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el)) -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprPath _ _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPath (Renderer renderer) outside (Path (Cons tooth@(Tooth {node, kids, i}) tooths)) expr makeAnnExprProps renderInside = do
  let path' = Path tooths
  let expr' = unTooth tooth expr
  renderAnnExprPath (Renderer renderer) outside path' expr' makeAnnExprProps do
    let kids' = insertAt "renderAnnExprPath" i expr kids
    arrangedKids <- renderer.arrangeExpr node $
      kids # map Just # insertAt "renderAnnExprPath" i Nothing # Array.mapWithIndex \i' -> case _ of
        Nothing -> renderInside <#> (_ /\ node)
        Just kid@(Tree {node: kidNode}) -> do
          let tooth' = Tooth {node, i: i', kids: deleteAt "renderAnnExprPath" i kids'}
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (Renderer renderer) (consPath outside tooth') kid makeAnnExprProps <#> (_ /\ kidNode)        
    renderAnnExprHelper (Renderer renderer) (outside <> path') expr' makeAnnExprProps arrangedKids

makeSyncExprProps :: forall sn el er ctx env. MakeSyncExprProps sn el er ctx env
makeSyncExprProps (Renderer renderer) outside inside@(Tree {node: AnnExprNode {elemId}}) = do
  ctx <- ask
  env <- get
  pure 
    [ HU.id $ elemId
    , HE.onClick \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent

        if true then do
          -- NOTE: `modifyExprGyro` modifies the state, which causes a re-render
          ctx.modifyExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside}
        else do
          -- NOTE: `modifySyncedExprGyro` only modifies a ref, which doesn not cause a re-render
          ctx.modifySyncedExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside}

    , HE.onMouseOver \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just true)
    , HE.onMouseOut \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just false)
    ]

renderSyncExpr renderer outside inside = renderAnnExpr renderer outside inside makeSyncExprProps
renderSyncExprPath renderer outside middle inside = renderAnnExprPath renderer outside middle inside makeSyncExprProps
