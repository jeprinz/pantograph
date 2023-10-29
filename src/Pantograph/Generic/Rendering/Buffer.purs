module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tree.Move
import Data.Tuple.Nested
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
import Data.CodePoint.Unicode as CodePoint
import Data.Either (Either(..))
import Data.Foldable (elem, foldM, foldMap, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.Regex as Regex
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
import Pantograph.Generic.Language.Common (annExprAnn, annExprNodeAnn)
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
import Todo (todo)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent

-- component

type BufferLocal sn el =
  { slotToken :: HK.SlotToken (BufferSlots sn el) }

bufferComponent :: forall sn el ctx env. Rendering sn el ctx env => H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
bufferComponent = HK.component \{queryToken, slotToken, outputToken} (BufferInput input) -> HK.do
  let 
    local :: BufferLocal sn el
    local = {slotToken}

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
      hydratedExprGyro <- getHydratedExprGyro
      rehydrateExprGyro local (Just hydratedExprGyro) Nothing
      let exprGyro' = shrinkAnnExprGyro hydratedExprGyro
      case f exprGyro' of
        Nothing -> pure unit
        Just exprGyro'' -> do
          let syncedExprGyro' = syncExprGyro exprGyro''
          liftEffect $ Ref.write syncedExprGyro' syncedExprGyroRef
          HK.modify_ exprGyroStateId (const exprGyro'') -- causes a re-render

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
          hydratedExprGyro' <- hydrateExprGyro local syncedExprGyro'
          modifyHydratedExprGyro (const (Just hydratedExprGyro'))

    modifyHydratedExprGyro f = do
      hydratedExprGyro <- getHydratedExprGyro
      case f hydratedExprGyro of
        Nothing -> pure unit
        Just hydratedExprGyro' -> do
          rehydrateExprGyro local (Just hydratedExprGyro) (Just hydratedExprGyro')
          liftEffect $ Ref.write (Just hydratedExprGyro') hydratedExprGyroRef

  renderCtx /\ renderCtxStateId <- HK.useState $
    snd (topCtx :: Proxy sn /\ Record ctx) # R.union
      { depth: 0
      , outputToken
      , slotToken
      , modifyExprGyro
      , modifySyncedExprGyro
      }

  renderEnv /\ renderEnvStateId <- HK.useState $
    snd (topEnv :: Proxy sn /\ Record env) # R.union
      { holeCount: 0 
      }

  let
    runRenderM = unwrap <<< runM renderCtx renderEnv
    gyroHtmls /\ _ = runRenderM $ renderSyncExprGyro initialSyncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydratedExprGyro <- hydrateExprGyro local initialSyncedExprGyro
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

        maybeEnabledToolbox <- request slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "get enabled")
        let enabledToolbox = maybeEnabledToolbox == Just true
        let isExistingToolbox = isJust maybeEnabledToolbox 

        -- if the Toolbox is currently ENABLED
        if enabledToolbox then do
          if false then pure unit

          else if ki.key == "Escape" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const false

          -- Enter|Spacebar: 
          else if ki.key == "Enter" || ki.key == " " then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "submit edit") $ unit

          -- Shift+Arrow(Left|Right)|Arrow(Up|Down): move Toolbox select
          else if ki.mods.shift && ki.key == "ArrowLeft" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect rowIx (colIx - 1)
          else if ki.mods.shift && ki.key == "ArrowRight" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect rowIx (colIx + 1)
          else if ki.key == "ArrowUp" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect (rowIx - 1) colIx
          else if ki.key == "ArrowDown" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify select") $ \(ToolboxSelect rowIx colIx) -> ToolboxSelect (rowIx + 1) colIx

          else pure unit

        -- if the Toolbox is currently DISABLED
        else do
          if false then pure unit

          -- alphaNum: open Toolbox and start query with this key
          else if (not ki.mods.special) && (ki.point # maybe false CodePoint.isAlphaNum) then do
            liftEffect $ Event.preventDefault event
            ensureExprGyroIsCursor
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const true
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify query") $ (_ <> String.singleton (fromJust ki.point))

          -- Escape: escape the Gyro
          else if ki.key == "Escape" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro escapeGyro
          
          -- Shift+Arrow(Left|Right|Up|Down): grab Gyro
          else if ki.key == "ArrowLeft" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroLeftUntil \(EN _ _ {validSelect}) _ -> validSelect
          else if ki.key == "ArrowRight" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroRightUntil \(EN _ _ {validSelect}) _ -> validSelect
          else if ki.key == "ArrowUp" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroLeftUntil \(EN _ _ {beginsLine, validSelect}) orientation -> beginsLine orientation && validSelect
          else if ki.key == "ArrowDown" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroRightUntil \(EN _ _ {beginsLine, validSelect}) orientation -> beginsLine orientation && validSelect

          -- Arrow(Left|Right|Up|Down): move Gyro
          else if ki.key == "ArrowLeft" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroLeftUntil \(EN _ _ {validCursor}) -> validCursor
          else if ki.key == "ArrowRight" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroRightUntil \(EN _ _ {validCursor}) -> validCursor
          else if ki.key == "ArrowUp" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroLeftUntil \(EN _ _ {beginsLine, validCursor}) orientation -> beginsLine orientation && validCursor orientation
          else if ki.key == "ArrowDown" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroRightUntil \(EN _ _ {beginsLine, validCursor}) orientation -> beginsLine orientation && validCursor orientation

          -- Spacebar: open Toolbox (no initial query)
          else if ki.key == " " then do
            liftEffect $ Event.preventDefault event
            ensureExprGyroIsCursor
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const true

          -- specialEdits
          
          -- Backspace: special "delete" Edit at Cursor or Select
          else if ki.key == "Backspace" then do
            liftEffect $ Event.preventDefault event
            hydratedExprGyro <- getHydratedExprGyro
            let maybeEdit = case hydratedExprGyro of
                  RootGyro expr -> specialEdits.deleteExpr $ getExprSort expr
                  CursorGyro (Cursor cursor) -> specialEdits.deleteExpr $ getExprSort cursor.inside
                  SelectGyro (Select select) -> specialEdits.deleteExprPath $ getExprNonEmptyPathSortChange select.middle
            case maybeEdit of
              Nothing -> pure unit
              Just edit -> modifyExprGyro $ applyEdit edit
          
          -- Enter: special "enter" Edit
          else if ki.key == "Enter" then do
            liftEffect $ Event.preventDefault event
            case specialEdits.enter unit of
              Nothing -> pure unit
              Just edit -> modifyExprGyro $ applyEdit edit
          
          -- Tab: special "tab" Edit
          else if ki.key == "Tab" then do
            liftEffect $ Event.preventDefault event
            case specialEdits.tab unit of
              Nothing -> pure unit
              Just edit -> modifyExprGyro $ applyEdit edit

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

hydrateExprNode :: forall sn el er ctx env. Rendering sn el ctx env => HydrateM sn el er (HydrateExprNode sn el er)
hydrateExprNode = do
  Cursor cursor <- asks _.cursor 
  maybeSelect <- asks _.maybeSelect

  let
    beginsLine orientation = getBeginsLine (Cursor cursor {orientation = orientation}) || null cursor.outside
    validCursor orientation = validGyro (CursorGyro (Cursor cursor {orientation = orientation}))
    validSelect = maybeSelect # maybe false validGyro
    EN label sigma ann = cursor.inside # treeNode

  pure $ EN label sigma $ ann # R.union
    { beginsLine
    , validCursor
    , validSelect }

hydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> SyncExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
hydrateExprGyro local gyro = do
  hydratedExprGyro <- case gyro of
    RootGyro expr -> do
      expr' <- flip runReaderT
          { cursor: Cursor {outside: mempty, inside: expr, orientation: Outside} 
          , maybeSelect: Just $ CursorGyro $ Cursor {outside: mempty, inside: expr, orientation: Outside} } $
        hydrateExpr
      pure $ RootGyro $ expr'
    CursorGyro cursor@(Cursor {outside, inside, orientation}) -> do
      outside' /\ inside' <- 
        flip runReaderT 
          { cursor: Cursor {outside: mempty, inside: escapeCursor cursor, orientation: Outside}
          , maybeSelect: case fromPathMaybe outside of
              Nothing -> Just $ CursorGyro $ Cursor {outside: mempty, inside: escapeCursor cursor, orientation: Outside}
              Just middle -> Just $ SelectGyro $ Select {outside: mempty, middle, inside, orientation: Outside} } $
        hydrateExprPath outside \outside' -> (outside' /\ _) <$>
        hydrateExpr
      pure $ CursorGyro $ Cursor {outside: outside', inside: inside', orientation}
    SelectGyro select@(Select {outside, middle, inside, orientation}) -> do
      outside' /\ middle' /\ inside' <- 
        flip runReaderT 
          { cursor: Cursor {outside: mempty, inside: escapeCursor $ escapeSelect $ select, orientation: Outside}
          , maybeSelect: Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "hydrateExprGyro" $ outside <> toPath middle, inside, orientation: Outside} } $
        hydrateExprPath outside \outside' -> (outside' /\ _) <$>
        hydrateExprPath (toPath middle) \middle' -> (fromPath "hydrateExprGyro" middle' /\ _) <$>
        hydrateExpr
      pure $ SelectGyro $ Select {outside: outside', middle: middle', inside: inside', orientation}
  rehydrateExprGyro local Nothing (Just hydratedExprGyro)
  pure hydratedExprGyro

hydrateStep :: forall sn el er a. Language sn el => Int -> HydrateM sn el er a -> HydrateM sn el er a
hydrateStep i = local $
  R.modify (Proxy :: Proxy "cursor") (moveCursorDownOuter i) >>>
  R.modify (Proxy :: Proxy "maybeSelect") (grabGyroDown i =<< _)

hydrateExprPath :: forall sn el er ctx env a. Rendering sn el ctx env => SyncExprPath sn el er -> (HydrateExprPath sn el er -> HydrateM sn el er a) -> HydrateM sn el er a
hydrateExprPath (Path ts0) k = go mempty (List.reverse ts0)
  where
  go path Nil = k path
  go path (Cons (Tooth _ (i /\ _)) ts) = do
    Cursor cursor0 <- asks _.cursor
    maybeSelect0 <- asks _.maybeSelect

    -- Debug.traceM $ "[hydrateExprPath]" <>
    --   "\n  • i = " <> show i <>
    --   "\n  • cursor0 = " <> pretty (Cursor cursor0) <>
    --   "\n  • select0 = " <> pretty maybeSelect0

    do
      Cursor cursor <- asks _.cursor
      maybeSelect <- asks _.maybeSelect

      -- Debug.traceM $ "[hydrateExprPath]" <>
      --   "\n  • cursor = " <> pretty (Cursor cursor) <>
      --   "\n  • select = " <> pretty maybeSelect

      hydratedNode <- hydrateExprNode
      hydratedKids <- tooths cursor.inside # Array.deleteAt i # fromJust' "hydrateExprPath" # traverse \(Tooth _ (i' /\ _) /\ _) -> do
        -- Debug.traceM $ "[hydrateExprPath]" <>
        --   "\n  • i = " <> show i' <>
        --   "\n  • cursor = " <> pretty (Cursor cursor) <>
        --   "\n  • select = " <> pretty maybeSelect

        hydrateStep i' $ hydrateExpr

      let tooth' = Tooth hydratedNode (i /\ hydratedKids)
      hydrateStep i $ go (consPath path tooth') ts

hydrateExpr :: forall sn el er ctx env. Rendering sn el ctx env => HydrateM sn el er (HydrateExpr sn el er)
hydrateExpr = do
  Cursor cursor <- asks _.cursor
  maybeSelect <- asks _.maybeSelect

  hydratedNode <- hydrateExprNode
  hydratedKids <- tooths cursor.inside # traverse \(Tooth _ (i /\ _) /\ _) -> do
    -- Debug.traceM $ "[hydrateExpr]" <>
    --   "\n  • i = " <> show i <>
    --   "\n  • cursor = " <> pretty (Cursor cursor) <>
    --   "\n  • select = " <> pretty maybeSelect

    hydrateStep i $ hydrateExpr
  pure $ Tree hydratedNode hydratedKids

-- | The subsequent hydrations (per render) only updates styles (doesn't modify
-- | hydrate data). The first `HydrateExprGyro` is old and the second
-- | `HydrateExprGyro` is new.
rehydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> Maybe (HydrateExprGyro sn el er) -> Maybe (HydrateExprGyro sn el er) -> HK.HookM Aff Unit
rehydrateExprGyro local m_hydratedExprGyro m_hydratedExprGyro' = do
  when (isJust m_hydratedExprGyro || isJust m_hydratedExprGyro') do
    tell local.slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") (const false)
  unhydrateExprGyro
  rehydrateExprGyro'
  where
  unhydrateExprGyro = case m_hydratedExprGyro of
    Nothing -> pure unit
    Just (RootGyro _expr) ->
      pure unit
    Just (CursorGyro (Cursor cursor)) -> do
      liftEffect $ HU.updateClassName (cursor.inside # annExprAnn # _.elemId) (className.orientationCursor cursor.orientation) (Just false)
    Just (SelectGyro (Select select)) -> do
      liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) (className.orientationSelect Outside) (Just false)
      liftEffect $ HU.updateClassName (select.inside # annExprAnn # _.elemId) (className.orientationSelect Inside) (Just false)

  rehydrateExprGyro' = case m_hydratedExprGyro' of
    Nothing -> pure unit
    Just (RootGyro _expr) ->
      pure unit
    Just (CursorGyro (Cursor cursor)) -> do
      liftEffect $ HU.updateClassName (cursor.inside # annExprAnn # _.elemId) (className.orientationCursor cursor.orientation) (Just true)
    Just (SelectGyro (Select select)) -> do
      liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) (className.orientationSelect Outside) (Just true)
      liftEffect $ HU.updateClassName (select.inside # annExprAnn # _.elemId) (className.orientationSelect Inside) (Just true)

-- render

renderSyncExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => SyncExprGyro sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprGyro (RootGyro expr) = renderSyncExpr (Path Nil) expr
renderSyncExprGyro (CursorGyro cursor) = renderSyncExprCursor cursor
renderSyncExprGyro (SelectGyro select) = renderSyncExprSelect select

renderSyncExprSelect :: forall sn el er ctx env. Rendering sn el ctx env => SyncExprSelect sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprSelect (Select {outside, middle, inside, orientation}) = do
  ctx <- ask
  env <- get
  let outside_middle = outside <> (toPath middle)
  renderSyncExprPath mempty outside (unPath outside_middle inside) $
    renderSyncExprPath outside (toPath middle) inside $
      renderSyncExpr outside_middle inside

renderSyncExprCursor :: forall sn el er ctx env. Rendering sn el ctx env => SyncExprCursor sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprCursor cursor@(Cursor {outside, inside, orientation}) = do
  ctx <- ask
  env <- get
  let toolboxHandler (ToolboxOutput output) = (output # _) $ case_
        # on (Proxy :: Proxy "submit edit") (\edit -> do
            tell ctx.slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const false
            ctx.modifyExprGyro $ applyEdit edit
          )
        # on (Proxy :: Proxy "preview edit") (\maybeEdit -> do 
            tell ctx.slotToken (Proxy :: Proxy "preview") LeftPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
            tell ctx.slotToken (Proxy :: Proxy "preview") RightPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
          )
  let wrapCursor htmls = do
        let toolboxInput = ToolboxInput
              { ctx
              , env
              , outside: shrinkAnnExprPath outside
              , inside: shrinkAnnExpr inside
              , enabled: false
              , edits: getEditsAtSort (getExprSort inside) orientation
              , initialQuery: getInitialQuery cursor }
        let previewInput position = PreviewInput 
              { ctx
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
  renderSyncExprPath mempty outside inside $
    wrapCursor <$>
      renderSyncExpr outside inside
