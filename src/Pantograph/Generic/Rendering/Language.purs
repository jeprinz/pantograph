module Pantograph.Generic.Rendering.Language where

import Data.Tree
import Pantograph.Generic.Language
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Keyboard
import Prelude
import Util

import Control.Monad.Identity.Trans (runIdentityT)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (get)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Display (Html, embedHtml)
import Data.Foldable (foldMap, null)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tree.Traverse (traverseGyro)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (inj)
import Effect.Aff (Aff)
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Pantograph.Generic.Rendering.TerminalItems (terminalItem)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

displayAnnExpr :: forall sn el ctx env er. Rendering sn el ctx env => AnnExpr sn el er -> Html
displayAnnExpr e = rmap runIdentityT $ El.ι $ fst $ snd (runRenderM :: Proxy sn /\ _) $ renderAnnExpr mempty e mempty

-- sync

syncExprGyro :: forall sn el ctx env er. Rendering sn el ctx env => AnnExprGyro sn el er -> SyncExprGyro sn el er
syncExprGyro = unwrap <<< traverseGyro \{outside, middle, inside: inside@(EN label sigma er % _)} -> do
  let greaterOutside = outside <> middle
  Identity $ 
    EN label sigma $ er # R.union 
      { elemId: HU.freshElementId unit
      , beginsLine: \orientation -> getBeginsLine (Cursor {outside: greaterOutside, inside, orientation}) || null greaterOutside
      , validCursor: \orientation -> validGyro (CursorGyro (Cursor {outside: greaterOutside, inside, orientation}))
      , validSelect: case fromPathMaybe middle of
          Nothing -> true
          Just middle' -> validGyro (SelectGyro (Select {outside, middle: middle', inside, orientation: Outside}))
      }

-- render

type MakeAnnExprProps sn el er ctx env i =
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  RenderM sn el ctx env (El.Props i)

type MakeSyncExprProps sn el er ctx env i = MakeAnnExprProps sn el (SyncExprRow sn el er) ctx env i

renderAnnExprHelper :: forall sn el er ctx env w t.
  Rendering sn el ctx env => MonadTrans t =>
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env (t Aff Unit) ->
  Array (ArrangeKid sn el (Array (HH.HTML w (t Aff Unit)))) ->
  RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit)))
renderAnnExprHelper outside expr makeAnnExprProps arrangedKids = do
  props <- makeAnnExprProps outside expr
  let htmls = arrangedKids # foldMap case _ of
        ArrangeKid htmls' -> htmls'
        ArrangeHtml htmls' -> embedHtml lift <$> htmls'
  pure $ [El.ℓ props htmls]

renderAnnExpr :: forall sn el er ctx env w t.
  Rendering sn el ctx env => MonadTrans t =>
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env (t Aff Unit) ->
  RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit)))
renderAnnExpr outside expr@(Tree node _) makeAnnExprProps = do
  arrangedKids <- arrangeExpr node $
    tooths expr <#> \(tooth /\ kid@(Tree kidNode _)) -> do
      local
        ( R.modify (Proxy :: Proxy "depth") (1 + _) )
        $ renderAnnExpr (consPath outside tooth) kid makeAnnExprProps <#> (_ /\ kidNode)
  renderAnnExprHelper outside expr makeAnnExprProps arrangedKids

renderAnnExprPath :: forall sn el er ctx env w t.
  Rendering sn el ctx env => MonadTrans t =>
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env (t Aff Unit) ->
  RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit))) -> RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit)))
renderAnnExprPath _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPath outside (Path (Cons tooth@(Tooth node (i /\ _)) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath outside path' expr' makeAnnExprProps do
    arrangedKids <- arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ (i' /\ _)) /\ kid@(Tree kidNode _)) -> do
        if i == i' then 
          renderInside <#> (_ /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (consPath interiorOutside tooth') kid makeAnnExprProps <#> (_ /\ kidNode)
    renderAnnExprHelper interiorOutside expr' makeAnnExprProps arrangedKids

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

renderAnnExprPathLeft :: forall sn el er ctx env w t.
  Rendering sn el ctx env => MonadTrans t =>
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env (t Aff Unit) ->
  RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit))) -> RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit)))
renderAnnExprPathLeft _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPathLeft outside (Path (Cons tooth@(Tooth node (i /\ _)) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath outside path' expr' makeAnnExprProps do
    arrangedKids <-
      map (map (map _.item)) $
      map (takeWhile1 case _ of
        ArrangeKid {keep} -> keep
        _ -> true) $
      arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ (i' /\ _)) /\ kid@(Tree kidNode _)) -> do
        let keep = i' < i -- strictly to the left
        if i == i' then 
          renderInside <#> (\item -> {item, keep} /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (consPath interiorOutside tooth') kid makeAnnExprProps <#> (\item -> {item, keep} /\ kidNode)
    renderAnnExprHelper interiorOutside expr' makeAnnExprProps arrangedKids

renderAnnExprPathRight :: forall sn el er ctx env w t.
  Rendering sn el ctx env => MonadTrans t =>
  AnnExprPath sn el er ->
  AnnExprPath sn el er ->
  AnnExpr sn el er ->
  MakeAnnExprProps sn el er ctx env (t Aff Unit) ->
  RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit))) -> RenderM sn el ctx env (Array (HH.HTML w (t Aff Unit)))
renderAnnExprPathRight _ (Path Nil) _ _ renderInside = renderInside
renderAnnExprPathRight outside (Path (Cons tooth@(Tooth node (i /\ _)) ts)) expr makeAnnExprProps renderInside = do
  let path' = Path ts
  let expr' = unTooth tooth expr
  let interiorOutside = outside <> path'
  renderAnnExprPath outside path' expr' makeAnnExprProps do
    arrangedKids <-
      map (map (map _.item)) $
      map (Array.dropWhile case _ of
        ArrangeKid {drop} -> drop
        _ -> true) $ 
      arrangeExpr node $
      tooths expr' <#> \(tooth'@(Tooth _ (i' /\ _)) /\ kid@(Tree kidNode _)) -> do
        let drop = i' < i -- middle or to the left
        if i == i' then 
          renderInside <#> (\item -> {item, drop} /\ kidNode)
        else
          local
            ( R.modify (Proxy :: Proxy "depth") (1 + _) )
            $ renderAnnExpr (consPath interiorOutside tooth') kid makeAnnExprProps <#> (\item -> {item, drop} /\kidNode)
    renderAnnExprHelper interiorOutside expr' makeAnnExprProps arrangedKids

-- makeSyncExprProps

makeSyncExprProps :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> MakeSyncExprProps sn el er ctx env (HK.HookM Aff Unit)
makeSyncExprProps local outside inside@(Tree (EN _ _ {elemId}) _) = do
  ctx <- ask
  env <- get
  pure
    [ El.Id elemId
    , El.Classes [El.Expr]
    , El.StrictHover mempty
    , El.OnMouseDown \mouseEvent -> do
        HK.raise local.tokens.outputToken $ BufferOutput $ inj (Proxy :: Proxy "write terminal") $ terminalItem.debug $ HH.div_
          [ HH.text "SyncExpr/onClick"
          , HH.ul_
              [ HH.li_ [HH.text $ "outside: " <> pretty (shrinkAnnExprPath outside :: ExprPath sn el)]
              , HH.li_ [HH.text $ "inside: " <> pretty (shrinkAnnExpr inside :: Expr sn el)]
              ]
          ]

        let isModifyExprGyro = true
        if isModifyExprGyro then do
          -- NOTE: `modifyExprGyro` modifies the state, which causes a re-render
          local.modifyExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside, orientation: Outside}
        else do
          -- NOTE: `modifySyncedExprGyro` only modifies a ref, which doesn not cause a re-render
          local.modifySyncedExprGyro $ const $ Just $ CursorGyro $ Cursor {outside: shrinkAnnExprPath outside, inside: shrinkAnnExpr inside, orientation: Outside}
    ]

renderSyncExpr local outside inside = renderAnnExpr outside inside (makeSyncExprProps local)
renderSyncExprPath local outside middle inside = renderAnnExprPath outside middle inside (makeSyncExprProps local)
