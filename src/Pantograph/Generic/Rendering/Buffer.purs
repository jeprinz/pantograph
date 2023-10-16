module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tree.Move
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Keyboard
import Pantograph.Generic.Rendering.Language
import Prelude
import Util

import Bug (bug)
import Control.Monad.Reader (Reader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (StateT, get)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (elem, foldM, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (case_, inj, on)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (defer, liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookF(..))
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Pantograph.Generic.Language.Edit (applyEdit)
import Pantograph.Generic.Rendering.Html as HH
import Pantograph.Generic.Rendering.Preview (previewComponent)
import Pantograph.Generic.Rendering.Style (className)
import Pantograph.Generic.Rendering.Toolbox (toolboxComponent)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as R
import Text.Pretty (pretty)
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent

-- component

bufferComponent :: forall sn el ctx env. PrettyTreeNode el => H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
bufferComponent = HK.component \{queryToken, slotToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  -- The original ExprGyro before rendering.
  exprGyro /\ exprGyroStateId <- HK.useState (RootGyro input.expr)

  -- The SyncExprGyro of the current facade.
  initialSyncedExprGyro /\ syncedExprGyroRef <- HK.useRef (syncExprGyro exprGyro)

  -- The HydrateExprGyro of the current facade.
  _ /\ hydratedExprGyroRef <- HK.useRef (Nothing :: Maybe (HydrateExprGyro sn el ()))

  let
    getHydratedExprGyro = do
      liftEffect (Ref.read hydratedExprGyroRef) >>= case _ of
        Nothing -> bug "[modifyHydratedExprGyro] hydratedExprGyroRef should already be `Just _` by now"
        Just hydratedExprGyro -> pure hydratedExprGyro

    modifyExprGyro f = do
      exprGyro <- getHydratedExprGyro <#> shrinkAnnExprGyro
      case f exprGyro of
        Nothing -> pure unit
        Just exprGyro' -> do
          let syncedExprGyro' = syncExprGyro exprGyro'
          liftEffect $ Ref.write syncedExprGyro' syncedExprGyroRef
          HK.modify_ exprGyroStateId (const exprGyro') -- causes a re-render

    -- If the exprGyro is not fresh, updates the exprGyro to correspond to the
    -- syncedExprGyro.
    ensureFreshExprGyro = do
      hydratedExprGyro <- getHydratedExprGyro
      modifyExprGyro (const (Just (shrinkAnnExprGyro hydratedExprGyro)))

    ensureExprGyroIsCursor = do
      hydratedExprGyro <- getHydratedExprGyro
      case ensureGyroIsCursor hydratedExprGyro of
        Nothing -> ensureFreshExprGyro
        Just hydratedExprGyro' -> modifyExprGyro (const (Just (shrinkAnnExprGyro hydratedExprGyro')))

    modifySyncedExprGyro f = do
      syncedExprGyro <- getHydratedExprGyro <#> shrinkAnnExprGyro
      case f syncedExprGyro of
        Nothing -> pure unit
        Just syncedExprGyro' -> do
          liftEffect $ Ref.write syncedExprGyro' syncedExprGyroRef
          hydratedExprGyro' <- hydrateExprGyro (Renderer renderer) syncedExprGyro'
          modifyHydratedExprGyro (const (Just hydratedExprGyro'))

    modifyHydratedExprGyro f = do
      hydratedExprGyro <- getHydratedExprGyro
      case f hydratedExprGyro of
        Nothing -> pure unit
        Just hydratedExprGyro' -> do
          rehydrateExprGyro (Renderer renderer) (Just hydratedExprGyro) hydratedExprGyro'
          liftEffect $ Ref.write (Just hydratedExprGyro') hydratedExprGyroRef

  renderCtx /\ renderCtxStateId <- HK.useState $
    renderer.topCtx # R.union
      { depth: 0
      , outputToken
      , slotToken
      , modifyExprGyro
      , modifySyncedExprGyro
      }

  renderEnv /\ renderEnvStateId <- HK.useState $
    renderer.topEnv # R.union
      { holeCount: 0 
      }

  let
    runRenderM = unwrap <<< runM renderCtx renderEnv
    gyroHtmls /\ _ = runRenderM $ renderSyncExprGyro (Renderer renderer) initialSyncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydratedExprGyro <- hydrateExprGyro (Renderer renderer) initialSyncedExprGyro
    liftEffect $ Ref.write (Just hydratedExprGyro) hydratedExprGyroRef
    pure Nothing

  HK.useQuery queryToken \(BufferQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "set exprGyro") (\(exprGyro' /\ a) -> do
        HK.modify_ exprGyroStateId (const exprGyro')
        pure $ Just a
      )
    # on (Proxy :: Proxy "keyboard") (\(keyboardEvent /\ a) -> do
        let event = KeyboardEvent.toEvent keyboardEvent
        let ki = getKeyInfo keyboardEvent

        maybeIsEnabledToolbox <- request slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "get isEnabled")
        let isEnabledToolbox = maybeIsEnabledToolbox == Just true
        let isExistingToolbox = isJust maybeIsEnabledToolbox 

        if isEnabledToolbox then do
          if false then pure unit
          else if ki.key == "Escape" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify isEnabled") $ const false
          else if ki.key == "Enter" || ki.key == " " then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "submit edit") $ unit
          else if ki.key == "ArrowLeft" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect rowIx (colIx - 1)
          else if ki.key == "ArrowRight" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect rowIx (colIx + 1)
          else if ki.key == "ArrowDown" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect (rowIx + 1) colIx
          else if ki.key == "ArrowUp" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect (rowIx - 1) colIx
          else pure unit

        else do
          if false then pure unit
          else if ki.key == "Escape" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro escapeGyro
          else if ki.key == "ArrowLeft" && ki.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro grabGyroLeft
          else if ki.key == "ArrowRight" && ki.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro grabGyroRight
          else if ki.key == "ArrowUp" && ki.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroLeftUntil \(AnnExprNode {beginsLine}) -> beginsLine
          else if ki.key == "ArrowDown" && ki.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroRightUntil \(AnnExprNode {beginsLine}) -> beginsLine

          else if ki.key == "ArrowLeft" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro moveGyroLeft
          else if ki.key == "ArrowRight" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro moveGyroRight
          else if ki.key == "ArrowUp" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroLeftUntil \(AnnExprNode {beginsLine}) -> beginsLine
          else if ki.key == "ArrowDown" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroRightUntil \(AnnExprNode {beginsLine}) -> beginsLine

          else if ki.key == " " then do
            liftEffect $ Event.preventDefault event
            ensureExprGyroIsCursor
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify isEnabled") $ const true
          else pure unit

        pure $ Just a
      )

  -- render
  HK.pure $ HH.panel
    { name: "BufferPanel"
    , info:
        [ HH.div [HP.classes [HH.ClassName "subtitle"]] [HH.text input.name] 
        ]
    , control:
        [ HH.div [HP.classes [HH.ClassName "button"]] [HH.text "x"] ]
    , content:
        [HH.div [HP.classes [HH.ClassName "program"]] gyroHtmls]
    }

-- hydrate

type HydrateM sn el er = ReaderT (HydrateCtx sn el er) (HK.HookM Aff)

-- this data is _only_ used for calculating `HydrateExprRow`
type HydrateCtx sn el er =
  { cursor :: SyncExprCursor sn el er
  , maybeSelect :: Maybe (SyncExprGyro sn el er) }

hydrateExprGyro :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> SyncExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
hydrateExprGyro (Renderer renderer) gyro = do
  hydratedExprGyro <- case gyro of
    RootGyro expr -> do
      expr' <- flip runReaderT
          { cursor: Cursor {outside: mempty, inside: expr, orientation: Outside} 
          , maybeSelect: Just $ CursorGyro $ Cursor {outside: mempty, inside: expr, orientation: Outside} } $
        hydrateExpr (Renderer renderer)
      pure $ RootGyro $ expr'
    CursorGyro cursor@(Cursor {outside, inside, orientation}) -> do
      outside' /\ inside' <- 
        flip runReaderT 
          { cursor: Cursor {outside: mempty, inside: escapeCursor cursor, orientation: Outside}
          , maybeSelect: case fromPathMaybe outside of
              Nothing -> Just $ CursorGyro $ Cursor {outside: mempty, inside: escapeCursor cursor, orientation: Outside}
              Just middle -> Just $ SelectGyro $ Select {outside: mempty, middle, inside, orientation: Outside} } $
        hydrateExprPath (Renderer renderer) outside \outside' -> (outside' /\ _) <$>
        hydrateExpr (Renderer renderer)
      pure $ CursorGyro $ Cursor {outside: outside', inside: inside', orientation}
    SelectGyro select@(Select {outside, middle, inside, orientation}) -> do
      outside' /\ middle' /\ inside' <- 
        flip runReaderT 
          { cursor: Cursor {outside: mempty, inside: escapeCursor $ escapeSelect $ select, orientation: Outside}
          , maybeSelect: Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "hydrateExprGyro" $ outside <> toPath middle, inside, orientation: Outside} } $
        hydrateExprPath (Renderer renderer) outside \outside' -> (outside' /\ _) <$>
        hydrateExprPath (Renderer renderer) (toPath middle) \middle' -> (fromPath "hydrateExprGyro" middle' /\ _) <$>
        hydrateExpr (Renderer renderer)
      pure $ SelectGyro $ Select {outside: outside', middle: middle', inside: inside', orientation}
  rehydrateExprGyro (Renderer renderer) Nothing hydratedExprGyro
  pure hydratedExprGyro

hydrateExprNode :: forall sn el er ctx env. Renderer sn el ctx env -> HydrateM sn el er (HydrateExprNode sn el er)
hydrateExprNode (Renderer renderer) = do
  let Language language = renderer.language
  Cursor cursor <- asks _.cursor 
  maybeSelect <- asks _.maybeSelect

  let
    AnnExprNode node = cursor.inside # treeNode
    beginsLine = renderer.beginsLine (Cursor cursor)

    exprNode = AnnExprNode $ node # R.union
      { beginsLine
      , validCursor: language.validGyro (CursorGyro (Cursor cursor))
      , validSelect: maybeSelect # maybe false language.validGyro }

  when beginsLine do
    liftEffect $ HU.updateClassName node.elemId (HH.ClassName "beginsLine") (Just true)

  pure exprNode

hydrateStep :: forall sn el er a. PrettyTreeNode el => Int -> HydrateM sn el er a -> HydrateM sn el er a
hydrateStep i = local $
  R.modify (Proxy :: Proxy "cursor") (moveCursorDownOuter i) >>>
  R.modify (Proxy :: Proxy "maybeSelect") (grabGyroDown i =<< _)

hydrateExprPath :: forall sn el er ctx env a. PrettyTreeNode el => Renderer sn el ctx env -> SyncExprPath sn el er -> (HydrateExprPath sn el er -> HydrateM sn el er a) -> HydrateM sn el er a
hydrateExprPath (Renderer renderer) (Path ts0) k = go mempty (List.reverse ts0)
  where
  go path Nil = k path
  go path (Cons (Tooth {i}) ts) = do
    Cursor cursor0 <- asks _.cursor
    maybeSelect0 <- asks _.maybeSelect

    Debug.traceM $ "[hydrateExprPath]" <>
      "\n  • i = " <> show i <>
      "\n  • cursor0 = " <> pretty (Cursor cursor0) <>
      "\n  • select0 = " <> pretty maybeSelect0

    hydrateStep i do
      Cursor cursor <- asks _.cursor
      maybeSelect <- asks _.maybeSelect

      hydratedNode <- hydrateExprNode (Renderer renderer)
      hydratedKids <- tooths cursor.inside # deleteAt "hydrateExprPath" i # traverse \{tooth: Tooth {i: i'}} -> do
        Debug.traceM $ "[hydrateExprPath]" <>
          "\n  • i = " <> show i' <>
          "\n  • cursor = " <> pretty (Cursor cursor) <>
          "\n  • select = " <> pretty maybeSelect

        hydrateStep i' $ hydrateExpr (Renderer renderer)

      let tooth' = Tooth {node: hydratedNode, i, kids: hydratedKids}
      go (consPath path tooth') ts

hydrateExpr :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> HydrateM sn el er (HydrateExpr sn el er)
hydrateExpr (Renderer renderer) = do
  Cursor cursor <- asks _.cursor
  maybeSelect <- asks _.maybeSelect

  hydratedNode <- hydrateExprNode (Renderer renderer)
  hydratedKids <- tooths cursor.inside # traverse \{tooth: Tooth {i}} -> do
    Debug.traceM $ "[hydrateExpr]" <>
      "\n  • i = " <> show i <>
      "\n  • cursor = " <> pretty (Cursor cursor) <>
      "\n  • select = " <> pretty maybeSelect

    hydrateStep i $ hydrateExpr (Renderer renderer)
  pure $ Tree {node: hydratedNode, kids: hydratedKids}

-- | The subsequent hydrations (per render) only updates styles (doesn't modify
-- | hydrate data). The first `HydrateExprGyro` is old and the second
-- | `HydrateExprGyro` is new.
rehydrateExprGyro :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> Maybe (HydrateExprGyro sn el er) -> HydrateExprGyro sn el er -> HK.HookM Aff Unit
rehydrateExprGyro (Renderer renderer) m_hydratedExprGyro hydratedExprGyro' = do
  unhydrateExprGyro
  rehydrateExprGyro'
  where
  unhydrateExprGyro = case m_hydratedExprGyro of
    Nothing -> pure unit
    Just (RootGyro _expr) ->
      pure unit
    Just (CursorGyro (Cursor cursor)) -> do
      liftEffect $ HU.updateClassName (cursor.inside # treeNode # unwrap # _.elemId) (className.orientationCursor cursor.orientation) (Just false)
    Just (SelectGyro (Select select)) -> do
      liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # unwrap # _.elemId) (className.orientationSelect Outside) (Just false)
      liftEffect $ HU.updateClassName (select.inside # treeNode # unwrap # _.elemId) (className.orientationSelect Inside) (Just false)

  rehydrateExprGyro' = case hydratedExprGyro' of
    RootGyro _expr ->
      pure unit
    CursorGyro (Cursor cursor) -> do
      liftEffect $ HU.updateClassName (cursor.inside # treeNode # unwrap # _.elemId) (className.orientationCursor cursor.orientation) (Just true)
    SelectGyro (Select select) -> do
      liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # unwrap # _.elemId) (className.orientationSelect Outside) (Just true)
      liftEffect $ HU.updateClassName (select.inside # treeNode # unwrap # _.elemId) (className.orientationSelect Inside) (Just true)

-- render

renderSyncExprGyro :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> SyncExprGyro sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprGyro renderer (RootGyro expr) = renderSyncExpr renderer (Path Nil) expr
renderSyncExprGyro renderer (CursorGyro cursor) = renderSyncExprCursor renderer cursor
renderSyncExprGyro renderer (SelectGyro select) = renderSyncExprSelect renderer select

renderSyncExprSelect :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> SyncExprSelect sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprSelect (Renderer renderer) (Select {outside, middle, inside, orientation}) = do
  ctx <- ask
  env <- get
  let outside_middle = outside <> (toPath middle)
  renderSyncExprPath (Renderer renderer) mempty outside (unPath outside_middle inside) $
    renderSyncExprPath (Renderer renderer) outside (toPath middle) inside $
      renderSyncExpr (Renderer renderer) outside_middle inside

renderSyncExprCursor :: forall sn el er ctx env. PrettyTreeNode el => Renderer sn el ctx env -> SyncExprCursor sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprCursor (Renderer renderer) (Cursor {outside, inside, orientation}) = do
  let Language language = renderer.language
  ctx <- ask
  env <- get
  let toolboxHandler (ToolboxOutput output) = (output # _) $ case_
        # on (Proxy :: Proxy "submit edit") (\edit -> do
            tell ctx.slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify isEnabled") $ const false
            ctx.modifyExprGyro $ applyEdit (Language language) edit
          )
        # on (Proxy :: Proxy "preview edit") (\maybeEdit -> do 
            tell ctx.slotToken (Proxy :: Proxy "preview") LeftPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
            tell ctx.slotToken (Proxy :: Proxy "preview") RightPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
          )
  let wrapCursor htmls = do
        let toolboxInput = ToolboxInput
              { renderer: Renderer renderer
              , ctx
              , env
              , outside: shrinkAnnExprPath outside
              , inside: shrinkAnnExpr inside
              , isEnabled: false
              , edits: language.getEdits (getExprSort (Language language) inside) orientation }
        let previewInput position = PreviewInput 
              { renderer: Renderer renderer
              , ctx
              , env
              , position
              , outside: shrinkAnnExprPath outside
              , inside: shrinkAnnExpr inside
              , maybeEdit: Nothing }
        Array.concat
          [ [HH.slot (Proxy :: Proxy "toolbox") unit toolboxComponent toolboxInput toolboxHandler]
          , [HH.slot_ (Proxy :: Proxy "preview") LeftPreviewPosition previewComponent (previewInput LeftPreviewPosition)]
          , htmls
          , [HH.slot_ (Proxy :: Proxy "preview") RightPreviewPosition previewComponent (previewInput RightPreviewPosition)] 
          ]
  renderSyncExprPath (Renderer renderer) mempty outside inside $
    wrapCursor <$>
      renderSyncExpr (Renderer renderer) outside inside
