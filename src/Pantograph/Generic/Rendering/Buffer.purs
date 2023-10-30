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
import Data.Tree.Traverse (traverseGyro)
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

bufferComponent :: forall sn el ctx env. Rendering sn el ctx env => H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
bufferComponent = HK.component \{queryToken, slotToken, outputToken} (BufferInput input) -> HK.do
  let 
    tokens :: BufferLocalTokens sn el
    tokens = {slotToken, outputToken}

  -- The original ExprGyro before rendering.
  exprGyro /\ exprGyroStateId <- HK.useState (RootGyro input.expr)

  -- The SyncExprGyro of the current facade.
  initialSyncedExprGyro /\ syncedExprGyroRef <- HK.useRef (syncExprGyro exprGyro)

  -- The HydrateExprGyro of the current facade.
  _ /\ hydExprGyroRef <- HK.useRef (Nothing :: Maybe (HydrateExprGyro sn el ()))

  let
    getHydratedExprGyro = do
      liftEffect (Ref.read hydExprGyroRef) >>= case _ of
        Nothing -> bug "[modifyHydratedExprGyro] hydExprGyroRef should already be `Just _` by now"
        Just hydExprGyro -> pure hydExprGyro

    modifyExprGyro f = do
      hydExprGyro <- getHydratedExprGyro
      rehydrateExprGyro tokens (Just hydExprGyro) Nothing
      let exprGyro' = shrinkAnnExprGyro hydExprGyro
      case f exprGyro' of
        Nothing -> pure unit
        Just exprGyro'' -> do
          let syncedExprGyro' = syncExprGyro exprGyro''
          liftEffect $ Ref.write syncedExprGyro' syncedExprGyroRef
          HK.modify_ exprGyroStateId (const exprGyro'') -- causes a re-render

    -- If the exprGyro is not fresh, updates the exprGyro to correspond to the
    -- syncedExprGyro.
    ensureFreshExprGyro = do
      hydExprGyro <- getHydratedExprGyro
      modifyExprGyro (const (Just (shrinkAnnExprGyro hydExprGyro)))

    ensureExprGyroIsCursor = do
      hydExprGyro <- getHydratedExprGyro
      case ensureGyroIsCursor hydExprGyro of
        Nothing -> ensureFreshExprGyro
        Just hydExprGyro' -> modifyExprGyro (const (Just (shrinkAnnExprGyro hydExprGyro')))

    modifySyncedExprGyro f = do
      syncedExprGyro <- getHydratedExprGyro <#> shrinkAnnExprGyro
      case f syncedExprGyro of
        Nothing -> pure unit
        Just syncedExprGyro' -> do
          liftEffect $ Ref.write syncedExprGyro' syncedExprGyroRef
          hydExprGyro' <- hydrateExprGyro syncedExprGyro'
          rehydrateExprGyro tokens Nothing (Just hydExprGyro')
          modifyHydratedExprGyro (const (Just hydExprGyro'))

    modifyHydratedExprGyro f = do
      hydExprGyro <- getHydratedExprGyro
      case f hydExprGyro of
        Nothing -> pure unit
        Just hydExprGyro' -> do
          rehydrateExprGyro tokens (Just hydExprGyro) (Just hydExprGyro')
          liftEffect $ Ref.write (Just hydExprGyro') hydExprGyroRef

  let
    local :: BufferLocal sn el
    local = 
      { tokens
      , modifyExprGyro
      , modifySyncedExprGyro }

  let
    -- runRenderM = unwrap <<< runM renderCtx renderEnv
    gyroHtmls /\ _ = snd (runRenderM :: Proxy sn /\ _) $ renderSyncExprGyro local initialSyncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydExprGyro <- hydrateExprGyro initialSyncedExprGyro
    liftEffect $ Ref.write (Just hydExprGyro) hydExprGyroRef
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
            hydExprGyro <- getHydratedExprGyro
            let maybeEdit = case hydExprGyro of
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

-- | Flush the `HydrateExprGyro`'s status to the DOM.
flushHydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => HydrateExprGyro sn el er -> HK.HookM Aff Unit
flushHydrateExprGyro = case _ of
  (RootGyro _expr) ->
    pure unit
  (CursorGyro (Cursor cursor)) -> do
    liftEffect $ HU.updateClassName (cursor.inside # annExprAnn # _.elemId) (className.orientationCursor cursor.orientation) (Just true)
  (SelectGyro (Select select)) -> do
    liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) (className.orientationSelect Outside) (Just true)
    liftEffect $ HU.updateClassName (select.inside # annExprAnn # _.elemId) (className.orientationSelect Inside) (Just true)

-- | Unflush the `HydrateExprGyro`'s status from the DOM.
unflushHydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => HydrateExprGyro sn el er -> HK.HookM Aff Unit
unflushHydrateExprGyro = case _ of
  (RootGyro _expr) ->
    pure unit
  (CursorGyro (Cursor cursor)) -> do
    liftEffect $ HU.updateClassName (cursor.inside # annExprAnn # _.elemId) (className.orientationCursor cursor.orientation) (Just false)
  (SelectGyro (Select select)) -> do
    liftEffect $ HU.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) (className.orientationSelect Outside) (Just false)
    liftEffect $ HU.updateClassName (select.inside # annExprAnn # _.elemId) (className.orientationSelect Inside) (Just false)

-- | The initial hydration (per render) initializes all the hydrate data and updates styles accordingly.
hydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => SyncExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
hydrateExprGyro syncExprGyro = syncExprGyro # traverseGyro \{inside: EN label sigma ann % _} -> pure $ EN label sigma $ ann # R.union {hydrated: unit}

-- | The subsequent hydrations (per render) only updates styles (doesn't modify
-- | hydrate data). The first `HydrateExprGyro` is old and the second
-- | `HydrateExprGyro` is new.
rehydrateExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocalTokens sn el -> Maybe (HydrateExprGyro sn el er) -> Maybe (HydrateExprGyro sn el er) -> HK.HookM Aff Unit
rehydrateExprGyro {slotToken} m_hydExprGyro m_hydExprGyro' = do
  when (isJust m_hydExprGyro || isJust m_hydExprGyro') do
    tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") (const false)
  case m_hydExprGyro of
    Nothing -> pure unit
    Just hydExprGyro -> unflushHydrateExprGyro hydExprGyro
  case m_hydExprGyro' of
    Nothing -> pure unit
    Just hydExprGyro -> flushHydrateExprGyro hydExprGyro

-- render

renderSyncExprGyro :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> SyncExprGyro sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprGyro local (RootGyro expr) = renderSyncExpr local (Path Nil) expr
renderSyncExprGyro local (CursorGyro cursor) = renderSyncExprCursor local cursor
renderSyncExprGyro local (SelectGyro select) = renderSyncExprSelect local select

renderSyncExprSelect :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> SyncExprSelect sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprSelect local (Select {outside, middle, inside, orientation}) = do
  let outside_middle = outside <> (toPath middle)
  renderSyncExprPath local mempty outside (unPath outside_middle inside) $
    renderSyncExprPath local outside (toPath middle) inside $
      renderSyncExpr local outside_middle inside

renderSyncExprCursor :: forall sn el er ctx env. Rendering sn el ctx env => BufferLocal sn el -> SyncExprCursor sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprCursor local cursor@(Cursor {outside, inside, orientation}) = do
  ctx <- ask
  env <- get
  let toolboxHandler (ToolboxOutput output) = (output # _) $ case_
        # on (Proxy :: Proxy "submit edit") (\edit -> do
            tell local.tokens.slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const false
            local.modifyExprGyro $ applyEdit edit
          )
        # on (Proxy :: Proxy "preview edit") (\maybeEdit -> do 
            tell local.tokens.slotToken (Proxy :: Proxy "preview") LeftPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
            tell local.tokens.slotToken (Proxy :: Proxy "preview") RightPreviewPosition PreviewQuery (Proxy :: Proxy "modify maybeEdit") $ const maybeEdit
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
  renderSyncExprPath local mempty outside inside $
    wrapCursor <$>
      renderSyncExpr local outside inside
