module Pantograph.Generic.Rendering.Language where

import Data.Tree
import Pantograph.Generic.Language
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Keyboard
import Prelude
import Util

import Control.Monad.Reader (ask, local)
import Control.Monad.State (get)
import DOM.HTML.Indexed as HPI
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Pantograph.Generic.Rendering.Style (className)
import Record as R
import Text.Pretty (pretty)
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
  PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  Array (ArrangeKid sn el (Array (BufferHtml sn el))) ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprHelper renderer outside expr makeAnnExprProps arrangedKids = do
  props <- makeAnnExprProps renderer outside expr
  let htmls = arrangedKids # foldMap case _ of
        ExprKidArrangeKid html -> html
        HtmlArrangeKid htmls' ->
          [ HH.div [HP.classes [HH.ClassName "HtmlArrangeKid"]]
              htmls' ]
        IndentationArrangeKid htmls' -> 
          [ HH.span [HP.classes [HH.ClassName "newline-header"]] [HH.text "↪"]
          , HH.br_ ] <>
          htmls'
  pure $ [HH.div props htmls]

renderAnnExpr :: forall sn el er ctx env.
  PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExpr (Renderer renderer) outside expr@(Tree node _) makeAnnExprProps = do
  arrangedKids <- renderer.arrangeExpr node $
    tooths expr <#> \(tooth /\ kid@(Tree kidNode _)) -> do
      local
        ( R.modify (Proxy :: Proxy "depth") (1 + _) )
        $ renderAnnExpr (Renderer renderer) (consPath outside tooth) kid makeAnnExprProps <#> (_ /\ kidNode)
  renderAnnExprHelper (Renderer renderer) outside expr makeAnnExprProps arrangedKids

renderAnnExprPath :: forall sn el er ctx env.
  PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el)) -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprPath _ _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPath (Renderer renderer) outside (Path (Cons tooth@(Tooth node i _) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath (Renderer renderer) outside path' expr' makeAnnExprProps do
    arrangedKids <- renderer.arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ i' _) /\ kid@(Tree kidNode _)) -> do
        if i == i' then 
          renderInside <#> (_ /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (Renderer renderer) (consPath interiorOutside tooth') kid makeAnnExprProps <#> (_ /\ kidNode)
    renderAnnExprHelper (Renderer renderer) interiorOutside expr' makeAnnExprProps arrangedKids

-- `takeWhile` and then one more
takeWhile1 :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile1 cond arr = 
  let {init, rest} = Array.span cond arr in
  init <> Array.take 1 rest

-- `dropWhile` but one less
dropWhile1 :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile1 cond arr =
  let {init, rest} = Array.span cond arr in
  Array.takeEnd 1 init <> rest

renderAnnExprPathLeft :: forall sn el er ctx env.
  PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el)) -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprPathLeft _ _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPathLeft (Renderer renderer) outside (Path (Cons tooth@(Tooth node i _) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath (Renderer renderer) outside path' expr' makeAnnExprProps do
    arrangedKids <-
      map (map (map _.item)) $
      map (takeWhile1 case _ of
        ExprKidArrangeKid {keep} -> keep
        _ -> true) $
      renderer.arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ i' _) /\ kid@(Tree kidNode _)) -> do
        let keep = i' < i -- strictly to the left
        if i == i' then 
          renderInside <#> (\item -> {item, keep} /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (Renderer renderer) (consPath interiorOutside tooth') kid makeAnnExprProps <#> (\item -> {item, keep} /\ kidNode)
    renderAnnExprHelper (Renderer renderer) interiorOutside expr' makeAnnExprProps arrangedKids

renderAnnExprPathRight :: forall sn el er ctx env.
  PrettyTreeNode el =>
  Renderer sn el ctx env ->
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env ->
  RenderM sn el ctx env (Array (BufferHtml sn el)) -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderAnnExprPathRight _ _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPathRight (Renderer renderer) outside (Path (Cons tooth@(Tooth node i _) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath (Renderer renderer) outside path' expr' makeAnnExprProps do
    arrangedKids <-
      map (map (map _.item)) $
      map (Array.dropWhile case _ of
        ExprKidArrangeKid {drop} -> drop
        _ -> true) $ 
      renderer.arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ i' _) /\ kid@(Tree kidNode _)) -> do
        let drop = i' < i -- middle or to the left
        if i == i' then 
          renderInside <#> (\item -> {item, drop} /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (Renderer renderer) (consPath interiorOutside tooth') kid makeAnnExprProps <#> (\item -> {item, drop} /\kidNode)
    renderAnnExprHelper (Renderer renderer) interiorOutside expr' makeAnnExprProps arrangedKids

-- makeSyncExprProps

makeSyncExprProps :: forall sn el er ctx env. PrettyTreeNode el => MakeSyncExprProps sn el er ctx env
makeSyncExprProps (Renderer renderer) outside inside@(Tree (AnnExprNode {elemId}) _) = do
  ctx <- ask
  env <- get
  pure 
    [ HU.id $ elemId
    , HP.classes [className.expr]
    , HE.onClick \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent

        HK.raise ctx.outputToken $ BufferOutput $ inj (Proxy :: Proxy "write terminal") $ terminalItem.debug $ HH.div_
          [ HH.text "SyncExpr/onClick"
          , HH.ul_
              [ HH.li_ [HH.text $ "outside: " <> pretty (shrinkAnnExprPath outside :: ExprPath sn el)]
              , HH.li_ [HH.text $ "inside: " <> pretty (shrinkAnnExpr inside :: Expr sn el)]
              ]
          ]

        let isModifyExprGyro = true
        if isModifyExprGyro then do
          -- NOTE: `modifyExprGyro` modifies the state, which causes a re-render
          ctx.modifyExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside, orientation: Outside}
        else do
          -- NOTE: `modifySyncedExprGyro` only modifies a ref, which doesn not cause a re-render
          ctx.modifySyncedExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside, orientation: Outside}

    , HE.onMouseOver \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just true)
    , HE.onMouseOut \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just false)
    ]

renderSyncExpr renderer outside inside = renderAnnExpr renderer outside inside makeSyncExprProps
renderSyncExprPath renderer outside middle inside = renderAnnExprPath renderer outside middle inside makeSyncExprProps
