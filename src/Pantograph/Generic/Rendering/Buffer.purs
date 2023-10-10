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
import Pantograph.Generic.Rendering.Keyboard (getKeyInfo)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as R
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

-- component

bufferComponent :: forall sn el ctx env. H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
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
        hydratedExprGyro <- rehydrateExprGyro (f hydratedExprGyro)
        liftEffect $ Ref.write (Just hydratedExprGyro) hydratedExprGyroRef

  renderCtx /\ renderCtxStateId <- HK.useState $
    R.union
      { depth: 0, outputToken, modifyHydratedExprGyro }
      renderer.topCtx 
  renderEnv /\ renderEnvStateId <- HK.useState $
    R.union
      { holeCount: 0 }
      renderer.topEnv

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
      else if ki.key == "ArrowLeft" then hole "TODO: move cursor to the left"
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
  liftEffect $ HU.setClassName node.elemId (node.gyroPosition # toClassName)

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

-- | hydrate and flush (if updated)
rehydrateExprNode :: forall sn el er. HydrateExprNode sn el er -> HydrateM sn el (HydrateExprNode sn el er)
rehydrateExprNode = hole "TODO: rehydrateExprNode"

-- render

renderExprGyro :: forall sn el er ctx env. Renderer sn el ctx env -> SyncExprGyro sn el er -> RenderM sn el ctx env (BufferHtml sn el)
renderExprGyro renderer (RootGyro expr) = renderExpr renderer (Path Nil) expr
renderExprGyro renderer (CursorGyro (Cursor {outside, inside})) = renderExpr renderer (Path Nil) $ unPath outside inside
renderExprGyro renderer (SelectGyro (Select {outside, middle, inside})) = renderExpr renderer (Path Nil) $ unPath outside $ unPath middle inside

renderExpr :: forall sn el er ctx env.
  Renderer sn el ctx env ->
  SyncExprPath sn el er ->
  SyncExpr sn el er ->
  RenderM sn el ctx env (BufferHtml sn el)
renderExpr (Renderer renderer) (Path tooths) (Tree {node: node@(AnnExprNode {elemId}), kids}) = do
  ctx <- ask
  arrangedKids <- renderer.arrangeExpr node $
    kids # Array.mapWithIndex \i kid@(Tree {node: kidNode}) -> do
      let tooth = Tooth {node: kidNode, i, kids: fromJust' "renderExpr" $ Array.deleteAt i kids}
      renderExpr (Renderer renderer) (Path (Cons tooth tooths)) kid <#> (_ /\ kidNode)
  let htmls = arrangedKids # foldMap case _ of
        ExprKidArrangeKid html -> [html]
        PunctuationArrangeKid htmls -> htmls
        IndentationArrangeKid htmls -> htmls
  pure $ HH.div
    [ HU.id $ elemId
    , HE.onMouseDown \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseDown] id = " <> show elemId}
    , HE.onMouseUp \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseUp] id = " <> show elemId}
    ]
    htmls

-- renderExprNode :: forall sn el er ctx env.
--   Renderer sn el ctx env -> 
--   SyncExprNode sn el er ->
--   Array (RenderM sn el ctx env (BufferHtml sn el /\ SyncExprNode sn el er)) ->
--   RenderM sn el ctx env (BufferHtml sn el)
-- renderExprNode (Renderer renderer) node@(AnnExprNode {elemId}) prearrangedKids = do
--   ctx <- ask
--   arrangedKids <- renderer.arrangeExpr node prearrangedKids
--   let htmls = arrangedKids # foldMap case _ of
--         ExprKidArrangeKid html -> [html]
--         PunctuationArrangeKid htmls -> htmls
--         IndentationArrangeKid htmls -> htmls
--   pure $ HH.div
--     [ HU.id $ elemId
--     , HE.onMouseDown \mouseEvent -> do
--         liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
--         HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseDown] id = " <> show elemId}
--     , HE.onMouseUp \mouseEvent -> do
--         liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
--         HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseUp] id = " <> show elemId}
--     ]
--     htmls

-- renderExprTooth :: forall sn el er ctx env.
--   Renderer sn el ctx env -> 
--   SyncExprTooth sn el er ->
--   SyncExprNode sn el er ->
--   RenderM sn el ctx env (BufferHtml sn el) ->
--   RenderM sn el ctx env (BufferHtml sn el)
-- renderExprTooth renderer (Tooth {node, kids, i}) innerNode renderInnerHtml =
--   renderExprNode renderer node $
--     fromJust' "renderExprTooth" $ Array.insertAt i (renderInnerHtml <#> (_ /\ innerNode)) $
--       kids <#> \kid@(Tree {node: kidNode}) -> renderExpr renderer kid <#> (_ /\ kidNode)

-- renderExprPath :: forall sn el er ctx env.
--   Renderer sn el ctx env -> 
--   SyncExprPath sn el er ->
--   SyncExprNode sn el er ->
--   RenderM sn el ctx env (BufferHtml sn el) ->
--   RenderM sn el ctx env (BufferHtml sn el)
-- renderExprPath _ (Path Nil) _ _ = bug "renderExprPath Nil"
-- renderExprPath renderer (Path (Cons t Nil)) innerNode renderInnerHtml =
--   renderExprTooth renderer t innerNode renderInnerHtml
-- renderExprPath renderer (Path (Cons t ts@(Cons (Tooth {node: tsNode}) _))) innerNode renderInnerHtml =
--   renderExprTooth renderer t tsNode $
--     renderExprPath renderer (Path ts) innerNode renderInnerHtml

{-

-- hydrate

hydrateExprGyro :: forall el ed sn. SyncExprGyro sn el -> HK.HookM Aff (HydrateExprGyro sn el)
hydrateExprGyro = case _ of
  (RootGyro expr) -> do
    fst <$> runM {gyroPosition: InsideRoot} {} (hydrateExpr expr)
  (CursorGyro (Cursor cursor)) -> do
    -- void
    --   $ runM {gyroPosition: OutsideCursor} {}
    --   $ hydrateExprPath cursor.outside
    -- void
    --   $ runM {gyroPosition: AtCursor} {}
    --   $ hydrateExpr cursor.inside
    hole "TODO"
  (SelectGyro (Select select)) -> do
    -- void
    --   $ runM {gyroPosition: OutsideSelect} {}
    --   $ hydrateExprPath select.outside
    -- void
    --   $ runM {gyroPosition: AtOutsideSelect} {}
    --   $ hydrateExprPath select.middle
    -- void
    --   $ runM {gyroPosition: AtInsideSelect} {}
    --   $ hydrateExpr select.inside
    hole "TODO"

hydrateExpr :: forall sn el er. SyncExpr sn el er -> HydrateM sn el (HydrateExprGyro sn el er)
hydrateExpr expr = hole "hydrateExprNode `traverse_` expr"

-- hydrateExprPath :: forall el ed sn. ExprPath el (SyncExprAnn ed) sn -> HydrateM el ed sn Unit
-- hydrateExprPath (Path Nil) = pure unit
-- hydrateExprPath (Path (Cons (Tooth tooth) ts)) = do
--   hydrateExprNode tooth.node
--   local (\ctx -> ctx {gyroPosition = kidGyroPosition ctx.gyroPosition}) $
--     hydrateExprPath (Path ts)

-- hydrateExprNode :: forall el ed sn. ExprNode el (SyncExprAnn ed) sn -> HydrateM el ed sn Unit
-- hydrateExprNode (ExprNode node) = do
--   ctx <- ask
--   void $ liftEffect $ HU.setClassName node.dat.elemId $ toClassName ctx.gyroPosition

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

renderExprGyro :: forall sn el ctx env. Renderer sn el ctx env -> SyncExprGyro sn el er -> RenderM ctx env el ed sn (BufferHtml sn el)
renderExprGyro renderer (RootGyro expr) = renderExpr renderer expr
renderExprGyro renderer (CursorGyro (Cursor cursor)) = hole "TODO: renderExprGyro"
renderExprGyro renderer (SelectGyro (Select select)) = hole "TODO: renderExprGyro"

renderExprNode :: forall ctx env el ed sn.
  Renderer ctx env el ed sn ->
  ExprNode el (SyncExprAnn ed) sn ->
  { arrangedKids :: Array (ArrangeKid el ed sn (BufferHtml sn el)) } ->
  RenderM ctx env el ed sn (BufferHtml sn el)
renderExprNode (Renderer renderer) (ExprNode node) {arrangedKids} = do
  ctx <- ask

  -- TODO: do some styling to reflect what kind of ArrangeKid they are, for the same of highlighting
  let htmlKids = arrangedKids # foldMap case _ of
        (ExprKidArrangeKid html) -> [html]
        (PunctuationArrangeKid htmls) -> htmls
        (IndentationArrangeKid htmls) -> htmls
  
  pure $ HH.div
    [ HU.id $ node.dat.elemId
    , HE.onMouseDown \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        -- Console.log "TODO: renderExpr onMouseDown"
        HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseDown] id = " <> show node.dat.elemId}
    , HE.onMouseUp \mouseEvent -> do
        liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
        -- Console.log "TODO: renderExpr onMouseUp"
        HK.raise ctx.outputToken $ WriteTerminalFromBuffer $ TerminalItem {tag: DebugTerminalItemTag, html: HH.text $ "[onMouseUp] id = " <> show node.dat.elemId}
    ]
    htmlKids

renderExpr :: forall ctx env el ed sn. Renderer ctx env el ed sn -> Expr el (SyncExprAnn ed) sn -> RenderM ctx env el ed sn (BufferHtml sn el)
renderExpr (Renderer renderer) (Tree expr@{node: ExprNode node}) = do
  let arrangeKids = expr.kids <#> \kid@(Tree {node}) -> renderExpr (Renderer renderer) kid <#> (_ /\ node)
  arrangedKids <- renderer.arrangeExpr expr.node arrangeKids
  renderExprNode (Renderer renderer) (ExprNode node)
    {arrangedKids}

-- renderExprPath :: forall ctx env el ed sn. Renderer ctx env el ed sn -> ExprPath el (SyncExprAnn ed) sn -> RenderM ctx env el ed sn (BufferHtml sn el) -> RenderM ctx env el ed sn (BufferHtml sn el)
-- renderExprPath (Renderer renderer) (Path Nil) inside = inside
-- renderExprPath (Renderer renderer) (Path (Cons (Tooth tooth) ts)) inside = hole inside

-- component

bufferComponent :: forall el ed sn ctx env. H.Component (BufferQuery el ed sn) (BufferInput ctx env el ed sn) (BufferOutput el ed sn) Aff
bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  renderCtx /\ renderCtxStateId <- HK.useState $
    R.union
      { depth: 0, outputToken }
      renderer.topCtx 
  renderEnv /\ renderEnvStateId <- HK.useState $
    R.union
      { holeCount: 0 }
      renderer.topEnv

  exprGyro /\ exprGyroStateId <- HK.useState (RootGyro input.expr)
  _ /\ hydratedExprGyroRef <- HK.useRef (Nothing :: Maybe (ExprGyro el (HydrateExprAnn (SyncExprAnn ed)) sn))

  -- runs before each render
  let syncedExprGyro = syncExprGyro exprGyro
  let gyroHtml /\ _ = unwrap $ runM renderCtx renderEnv $ renderExprGyro (Renderer renderer) syncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydratedExprGyro <- hydrateExprGyro syncedExprGyro
    liftEffect $ Ref.write (Just hydratedExprGyro) hydratedExprGyroRef
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
  M ctx env el ed sn (ExprNode el (SyncExprAnn ed') sn)
syncExprNode = hole "TODO: hydrateExprNode"

hydrateExprNode :: forall ctx env el ed ed' sn.
  ExprNode el (SyncExprAnn ed') sn ->
  HydrateM ctx env el ed sn  (ExprNode el (HydrateExprAnn ed') sn)
hydrateExprNode = hole "TODO: hydrateExprNode"

setupExprKid :: forall ctx env el ed ed' sn a.
  ExprNode el ed' sn ->
  HydrateM ctx env el ed sn a ->
  HydrateM ctx env el ed sn a
setupExprKid = hole "TODO: setupExprKid"

hydrateClassName :: forall el ed sn. ExprNode el (HydrateExprAnn ed) sn -> HH.ClassName
hydrateClassName = hole "TODO: hydrateClassName"

-- sync, hydrate, render
renderExpr :: forall ctx env el ed sn.
  Expr el (SyncExprAnn ed) sn ->
  RenderHydrateM ctx env el ed sn {html :: BufferHtml sn el, hydratedExpr :: Expr el (SyncExprAnn ed) sn}
renderExpr (Tree expr) = do
  hydratedExprNode <- -- :: ExprNode el (HydrateExprAnn (SyncExprAnn ed)) sn <- 
    expr.node # (syncExprNode >=> hydrateExprNode)
  
  arrangedHtmls <- do
    let 
      -- arrangeKids :: Array (M (RenderCtx el ed sn ctx) (RenderEnv el ed sn env) el ed sn ({html :: BufferHtml sn el, expr :: Expr el (HydrateExprAnn (SyncExprAnn ed)) sn} /\ ExprNode el (HydrateExprAnn (SyncExprAnn ed)) sn))
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
    hydratedExprKids :: Array (Expr el (HydrateExprAnn (SyncExprAnn ed)) sn)
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