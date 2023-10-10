module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Util

import Bug (bug)
import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT)
import Data.Array as Array
import Data.Foldable (foldM, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tree.Move (moveGyroLeft, moveGyroRight)
import Data.Tuple (fst, snd)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (defer, liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Pantograph.Generic.Language (Sort)
import Pantograph.Generic.Language.Common (unAnnExprTooth)
import Pantograph.Generic.Rendering.Keyboard (getKeyInfo)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as R
import Text.Pretty (pretty)
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- component

bufferComponent :: forall sn el ctx env. Show sn => Show el => PrettyTreeNode el => H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  exprGyro /\ exprGyroStateId <- HK.useState (RootGyro input.expr)

  _ /\ hydratedExprGyroRef <- HK.useRef (Nothing :: Maybe (HydrateExprGyro sn el ()))
  -- The last `HydrateExpr`, which is used for moving the cursor
  _ /\ lastHydratedExprRef <- HK.useRef (Nothing :: Maybe (HydrateExpr sn el ()))
  -- The last `HydratedExprCursor`, which is used for moving the select
  _ /\ lastHydratedExprCursorRef <- HK.useRef (Nothing :: Maybe (HydrateExprCursor sn el ()))

  let modifyHydratedExprGyro f = do
        hydratedExprGyro <- liftEffect (Ref.read hydratedExprGyroRef) >>= case _ of
          Nothing -> bug "[rehydrate] hydratedExprGyroRef should already be `Just _` by now"
          Just hydratedExprGyro -> pure hydratedExprGyro
        case f hydratedExprGyro of
          Nothing -> pure unit
          Just hydratedExprGyro' -> do
            hydratedExprGyro <- rehydrateExprGyro hydratedExprGyro'
            liftEffect $ Ref.write (Just hydratedExprGyro) hydratedExprGyroRef

  renderCtx /\ renderCtxStateId <- HK.useState $
    renderer.topCtx # R.union
      { depth: 0
      , outputToken
      , setExprGyro: \exprGyro' -> HK.modify_ exprGyroStateId (const exprGyro')
      }
  renderEnv /\ renderEnvStateId <- HK.useState $
    renderer.topEnv # R.union
      { holeCount: 0 
      }

  let runRenderM = unwrap <<< runM renderCtx renderEnv

  -- runs before each render
  let syncedExprGyro = syncExprGyro exprGyro
  let gyroHtml /\ _ = runRenderM $ renderExprGyro (Renderer renderer) syncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydratedExprGyro <- hydrateExprGyro syncedExprGyro
    liftEffect $ Ref.write (Just hydratedExprGyro) hydratedExprGyroRef
    case hydratedExprGyro of
      RootGyro hydratedExpr -> liftEffect $ Ref.write (Just hydratedExpr) lastHydratedExprRef
      CursorGyro hydratedExprCursor -> liftEffect $ Ref.write (Just hydratedExprCursor) lastHydratedExprCursorRef
      SelectGyro hydratedExprSelect -> pure unit
    pure Nothing

  HK.useQuery queryToken case _ of
    SetExprGyro exprGyro a -> do
      HK.modify_ exprGyroStateId (const exprGyro)
      pure $ Just a
    KeyboardEventBufferQuery keyboardEvent a -> do
      let ki = getKeyInfo keyboardEvent

      if false then pure unit
      else if ki.key == "ArrowLeft" then modifyHydratedExprGyro moveGyroLeft
      else if ki.key == "ArrowRight" then modifyHydratedExprGyro moveGyroRight
      else pure unit

      pure $ Just a

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

-- sync

syncExprGyro :: forall sn el er. AnnExprGyro sn el er -> SyncExprGyro sn el er
syncExprGyro = map \(AnnExprNode node) -> AnnExprNode $ R.union {elemId: HU.freshElementId unit} node

-- hydrate

flushExprNode :: forall sn el er. HydrateExprNode sn el er -> HydrateM sn el Unit
flushExprNode (AnnExprNode node) = do
  -- gyroPosition
  liftEffect $ HU.setClassNames node.elemId (node.gyroPosition # toClassNames)

hydrateExprGyro :: forall sn el er. SyncExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
hydrateExprGyro (RootGyro expr) = do
  expr' /\ _ <-
    ( let ctx = {gyroPosition: InsideRoot} in
      let env = {} in
      runM ctx env ) $
    hydrateExpr expr
  pure $ RootGyro expr'
hydrateExprGyro (CursorGyro (Cursor {outside, inside})) = do
  (outside' /\ inside') /\ _ <-
    ( let ctx = {gyroPosition: OutsideCursor} in 
      let env = {} in 
      runM ctx env ) $
    hydratePath outside \outside' -> (outside' /\ _) <$>
      local (\ctx -> ctx {gyroPosition = AtCursor})
        (hydrateExpr inside)
  pure $ CursorGyro $ Cursor {outside: outside', inside: inside'}
hydrateExprGyro (SelectGyro (Select {outside, middle, inside})) = hole "TODO: hydrateExprGyro"

-- | hydrate and flush
hydrateExprNode :: forall sn el er. SyncExprNode sn el er -> HydrateM sn el (HydrateExprNode sn el er)
hydrateExprNode (AnnExprNode node) = do
  ctx <- ask
  let exprNode = AnnExprNode $ node # R.union
        { gyroPosition: ctx.gyroPosition }
  flushExprNode exprNode
  pure exprNode

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

hydrateBelow :: forall sn el er a. HydrateExprNode sn el er -> HydrateM sn el a -> HydrateM sn el a
hydrateBelow (AnnExprNode _node) ma = do
  ma #
    (local $
      R.modify (Proxy :: Proxy "gyroPosition") kidGyroPosition)

hydrateExpr :: forall sn el er. SyncExpr sn el er -> HydrateM sn el (HydrateExpr sn el er)
hydrateExpr (Tree {node, kids}) = do
  hydratedNode <- hydrateExprNode node
  hydratedKids <- hydrateBelow hydratedNode $ kids # traverse hydrateExpr
  pure $ Tree {node: hydratedNode, kids: hydratedKids}

hydratePath :: forall sn el er a. SyncExprPath sn el er -> (HydrateExprPath sn el er -> HydrateM sn el a) -> HydrateM sn el a
hydratePath (Path Nil) k = k $ Path Nil
hydratePath (Path (Cons (Tooth {node, i, kids}) ts)) k =
  hydratePath (Path ts) \(Path ts') -> do
    hydratedNode <- hydrateExprNode node
    hydratedKids <- hydrateBelow hydratedNode $ kids # traverse hydrateExpr
    let tooth = Tooth {node: hydratedNode, i, kids: hydratedKids}
    k $ Path (Cons tooth ts')

-- TODO: should this do anything special?
rehydrateExprGyro :: forall sn el er. HydrateExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
rehydrateExprGyro = unsafeCoerce >>> hydrateExprGyro

-- -- | hydrate and flush (if updated)
-- rehydrateExprNode :: forall sn el er. HydrateExprNode sn el er -> HydrateM sn el (HydrateExprNode sn el er)
-- rehydrateExprNode = hole "TODO: rehydrateExprNode"

-- render

renderExprGyro :: forall sn el er ctx env. Show sn => Show el => PrettyTreeNode el => Renderer sn el ctx env -> SyncExprGyro sn el er -> RenderM sn el ctx env (BufferHtml sn el)
renderExprGyro renderer (RootGyro expr) = renderExpr renderer (Path Nil) expr
renderExprGyro renderer (CursorGyro (Cursor {outside, inside})) = renderExpr renderer (Path Nil) $ unPath outside inside
renderExprGyro renderer (SelectGyro (Select {outside, middle, inside})) = renderExpr renderer (Path Nil) $ unPath outside $ unPath middle inside

renderExpr :: forall sn el er ctx env.
  Show sn => Show el => PrettyTreeNode el =>
  Renderer sn el ctx env ->
  SyncExprPath sn el er ->
  SyncExpr sn el er ->
  RenderM sn el ctx env (BufferHtml sn el)
renderExpr (Renderer renderer) path expr@(Tree {node: node@(AnnExprNode {elemId}), kids}) = do
  ctx <- ask
  arrangedKids <- renderer.arrangeExpr node $
    kids # Array.mapWithIndex \i kid@(Tree {node: kidNode}) -> do
      let tooth = Tooth {node, i, kids: deleteAt "renderExpr" i kids}
      local
        ( R.modify (Proxy :: Proxy "depth") (1 + _) )
        $ renderExpr (Renderer renderer) (consPath path tooth) kid <#> (_ /\ kidNode)
  let htmls = arrangedKids # foldMap case _ of
        ExprKidArrangeKid html -> [html]
        PunctuationArrangeKid htmls -> htmls
        IndentationArrangeKid htmls -> 
          [ HH.span [HP.classes [HH.ClassName "newline-header"]] [HH.text "\\n"]
          , HH.br_ ] <>
          htmls
  pure $ HH.div
    [ HU.id $ elemId
    -- , HE.onMouseDown \mouseEvent -> do
    --     liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
    --     HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseDown] id = " <> show elemId}
    -- , HE.onMouseUp \mouseEvent -> do
    --     liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
    --     HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseUp] id = " <> show elemId}
    , HE.onClick \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        ctx.setExprGyro (CursorGyro (Cursor {outside: unAnnExprPath path, inside: unAnnExpr expr}))
    , HE.onMouseOver \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just true)
    , HE.onMouseOut \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        liftEffect $ HU.updateClassName elemId (HH.ClassName "hover") (Just false)
    ]
    htmls
