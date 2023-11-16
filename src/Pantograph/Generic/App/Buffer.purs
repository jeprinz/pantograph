module Pantograph.Generic.App.Buffer (bufferComponent) where

import Data.Either.Nested
import Data.Tree
import Data.Tree.Swivel
import Data.Tuple.Nested
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Preview
import Pantograph.Generic.App.Toolbox
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Util

import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Data.Array as Array
import Data.CodePoint.Unicode as CodePoint
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Tree.Traverse (traverseGyro)
import Data.Tuple (snd)
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.KeyInfo as KeyInfo
import Pantograph.Generic.App.BufferInfo (bufferInfoComponent)
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Language.Common (getEditsAtExprCursor, getShortcutEdit)
import Pantograph.Generic.Rendering.Hook.Keyboard as Keyboard
import Record as R
import Todo (todo)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- component

bufferComponent :: forall sn el ctx env. Dynamics sn el ctx env => H.Component (BufferQuery sn el) (BufferInput sn el ctx env) (BufferOutput sn el) Aff
bufferComponent = HK.component \{queryToken, slotToken, outputToken} (BufferInput input) -> debug "[render:buffer]" {} \_ -> HK.do
  let
    tokens :: BufferLocalTokens sn el
    tokens = {slotToken, outputToken}

  -- The original ExprGyro before rendering.
  exprGyro /\ exprGyroStateId <- HK.useState (fromTreeToGyro input.expr)

  -- The SyncExprGyro of the current facade.
  initialSyncedExprGyro /\ syncedExprGyroRef <- HK.useRef (syncExprGyro exprGyro)

  -- The HydrateExprGyro of the current facade.
  _ /\ hydExprGyroRef <- HK.useRef (Nothing :: Maybe (HydrateExprGyro sn el ()))

  let
    getHydratedExprGyro = do
      liftEffect (Ref.read hydExprGyroRef) >>= case _ of
        Nothing -> GMB.error $ El.text $ "[modifyHydratedExprGyro] hydExprGyroRef should already be `Just _` by now"
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
      , modifySyncedExprGyro
      }

  let
    gyroHtmls /\ _ = snd (runRenderM :: Proxy sn /\ _) $ renderSyncExprGyro local initialSyncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydExprGyro <- hydrateExprGyro initialSyncedExprGyro
    rehydrateExprGyro tokens Nothing (Just hydExprGyro)
    liftEffect $ Ref.write (Just hydExprGyro) hydExprGyroRef
    pure Nothing

  HK.useQuery queryToken \(BufferQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "set exprGyro") (\(exprGyro' /\ a) -> do
        HK.modify_ exprGyroStateId (const exprGyro')
        pure $ Just a
      )
    # on (Proxy :: Proxy "copy") (\k -> do
        hydExprGyro <- getHydratedExprGyro
        let clipboard = generalizeClipboard $ case hydExprGyro of
              CursorGyro (Cursor cursor) -> ExprClipboard $ shrinkAnnExpr cursor.inside
              SelectGyro (Select select) -> ExprNonEmptyPathClipboard $ shrinkAnnExprNonEmptyPath select.middle
        tell slotToken (Proxy :: Proxy "info") unit BufferInfoQuery (Proxy :: Proxy "set mbClipboard") (Just clipboard)
        pure $ Just $ k clipboard
      )
    # on (Proxy :: Proxy "cut") (\k -> do
        hydExprGyro <- getHydratedExprGyro
        case specialEdits.cut (shrinkAnnExprGyro hydExprGyro) of
          Nothing -> pure unit
          Just edit -> modifyExprGyro $ applyEdit edit
        let clipboard = generalizeClipboard $ case hydExprGyro of
              CursorGyro (Cursor cursor) -> ExprClipboard $ shrinkAnnExpr cursor.inside
              SelectGyro (Select select) -> ExprNonEmptyPathClipboard $ shrinkAnnExprNonEmptyPath select.middle
        tell slotToken (Proxy :: Proxy "info") unit BufferInfoQuery (Proxy :: Proxy "set mbClipboard") (Just clipboard)
        pure $ Just $ k clipboard
      )
    # on (Proxy :: Proxy "paste") (\(clipboard /\ a) -> do
        hydExprGyro <- getHydratedExprGyro
        case specialEdits.paste (shrinkAnnExprGyro hydExprGyro) clipboard of
          Nothing -> pure unit
          Just edit -> modifyExprGyro $ applyEdit edit
        pure $ Just a
      )
    # on (Proxy :: Proxy "keyboard") (\(keyboardEvent /\ a) -> do
        let
          event = KeyboardEvent.toEvent keyboardEvent
          ki = KeyInfo.getKeyInfo keyboardEvent

        maybeEnabledToolbox <- request slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "get enabled")
        let 
          enabledToolbox = maybeEnabledToolbox == Just true
          isExistingToolbox = isJust maybeEnabledToolbox 

        let 
          gyroIsValidCursor :: forall er. SyncExprGyro sn el er -> Boolean
          gyroIsValidCursor = gyroNode >>> \(EN _ _ {validCursor}) -> validCursor Outside
          gyroIsValidCursorAndBeginsLine :: forall er. SyncExprGyro sn el er -> Boolean
          gyroIsValidCursorAndBeginsLine = gyroNode >>> \(EN _ _ {validCursor, beginsLine}) -> validCursor Outside && beginsLine Outside
        let
          gyroIsValidSelect :: forall er. SyncExprGyro sn el er -> Boolean
          gyroIsValidSelect = gyroNode >>> \(EN _ _ {validSelect}) -> validSelect
          gyroIsValidSelectAndBeginsLine :: forall er. SyncExprGyro sn el er -> Boolean
          gyroIsValidSelectAndBeginsLine = gyroNode >>> \(EN _ _ {validSelect, beginsLine}) -> validSelect && beginsLine Outside
        let
          selectToCursor :: forall er. SyncExprGyro sn el er -> SyncExprGyro sn el er
          selectToCursor (CursorGyro cursor) = CursorGyro cursor
          selectToCursor (SelectGyro select) = CursorGyro (escapeSelect select)

          -- fromGyroToSelectOrCursor

        -- if the Toolbox is currently ENABLED
        if enabledToolbox then do
          if false then pure unit

          else if ki.key == "Escape" then do
            liftEffect $ Event.preventDefault event
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const false

          -- Enter: submit Toolbox edit
          else if ki.key == "Enter" then do
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

          -- alphaNum|etc: open Toolbox and start query with this key
          else if not ki.mods.special && maybe false (flip Array.any [CodePoint.isAlphaNum, flip Array.elem [codepoint."_", codepoint."/"]] <<< (#)) ki.point then do
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
            modifyHydratedExprGyro $ grabGyroPrev `until` gyroIsValidSelect
          else if ki.key == "ArrowRight" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroNext `until` gyroIsValidSelect
          else if ki.key == "ArrowUp" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroPrev `until` gyroIsValidSelectAndBeginsLine
          else if ki.key == "ArrowDown" && ki.mods.shift then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ grabGyroNext `until` gyroIsValidSelectAndBeginsLine

          -- Arrow(Left|Right|Up|Down): move Gyro
          else if ki.key == "ArrowLeft" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroPrev `until` gyroIsValidCursor
          else if ki.key == "ArrowRight" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroNext `until` gyroIsValidCursor
          else if ki.key == "ArrowUp" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroPrev `until` gyroIsValidCursorAndBeginsLine
          else if ki.key == "ArrowDown" then do
            liftEffect $ Event.preventDefault event
            modifyHydratedExprGyro $ moveGyroNext `until` gyroIsValidCursorAndBeginsLine
          -- Shift+Enter: open Toolbox (no initial query)
          else if ki.mods.shift && ki.key == "Enter" then do
            liftEffect $ Event.preventDefault event
            ensureExprGyroIsCursor
            tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") $ const true
          else do
            -- shortcuts
            gyro <- getHydratedExprGyro
            case getShortcutEdit (shrinkAnnExprGyro gyro) ki of
              Nothing -> pure unit
              Just edit -> do
                liftEffect $ Event.preventDefault event
                modifyExprGyro $ applyEdit edit
            pure unit

        pure $ Just a
      )

  -- render
  HK.pure $ makePanel
    { className: El.BufferPanel
    , info:
        [ El.ℓ [El.Classes [El.Subtitle]] [El.text input.name] 
        ]
    , control:
        [ El.ℓ [El.Classes [El.Button]] [El.text "⇑"]
        , El.ℓ [El.Classes [El.Button]] [El.text "⇓"]
        , El.ℓ [El.Classes [El.Button]] [El.text "×"] ]
    , content:
        [ El.ℓ [El.Classes [El.Program]] gyroHtmls
        , HH.slot_ (Proxy :: Proxy "info") unit bufferInfoComponent $ BufferInfoInput 
            { mbSort: Just $ getExprSort input.expr }
        ]
    }

-- hydrate

-- | Flush the `HydrateExprGyro`'s status to the DOM.
flushHydrateExprGyro :: forall sn el er ctx env. Dynamics sn el ctx env => HydrateExprGyro sn el er -> HK.HookM Aff Unit
flushHydrateExprGyro = case _ of
  (CursorGyro (Cursor cursor)) -> do
    liftEffect $ El.updateClassName (cursor.inside # annExprAnn # _.elemId) (fromOrientationToCursorClassName cursor.orientation) (Just true)
  (SelectGyro (Select select)) -> do
    liftEffect $ El.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) El.OutsideSelect (Just true)
    liftEffect $ El.updateClassName (select.inside # annExprAnn # _.elemId) El.InsideSelect (Just true)

-- | Unflush the `HydrateExprGyro`'s status from the DOM.
unflushHydrateExprGyro :: forall sn el er ctx env. Dynamics sn el ctx env => HydrateExprGyro sn el er -> HK.HookM Aff Unit
unflushHydrateExprGyro = case _ of
  (CursorGyro (Cursor cursor)) -> do
    liftEffect $ El.updateClassName (cursor.inside # annExprAnn # _.elemId) (fromOrientationToCursorClassName cursor.orientation) (Just false)
  (SelectGyro (Select select)) -> do
    liftEffect $ El.updateClassName (select.middle # nonEmptyPathOuterNode # annExprNodeAnn # _.elemId) El.OutsideSelect (Just false)
    liftEffect $ El.updateClassName (select.inside # annExprAnn # _.elemId) El.InsideSelect (Just false)

-- | The initial hydration (per render) initializes all the hydrate data and updates styles accordingly.
hydrateExprGyro :: forall sn el er ctx env. Dynamics sn el ctx env => SyncExprGyro sn el er -> HK.HookM Aff (HydrateExprGyro sn el er)
hydrateExprGyro syncExprGyro = syncExprGyro # traverseGyro \{inside: EN label sigma ann % _} -> pure $ EN label sigma $ ann # R.union {hydrated: unit}

-- | The subsequent hydrations (per render) only updates styles (doesn't modify
-- | hydrate data). The first `HydrateExprGyro` is old and the second
-- | `HydrateExprGyro` is new.
rehydrateExprGyro :: forall sn el er ctx env. Dynamics sn el ctx env => BufferLocalTokens sn el -> Maybe (HydrateExprGyro sn el er) -> Maybe (HydrateExprGyro sn el er) -> HK.HookM Aff Unit
rehydrateExprGyro localTokens@{slotToken} mb_hydExprGyro mb_hydExprGyro' = do
  when (isJust mb_hydExprGyro || isJust mb_hydExprGyro') do
    tell slotToken (Proxy :: Proxy "toolbox") unit ToolboxQuery (Proxy :: Proxy "modify enabled") (const false)
  case mb_hydExprGyro of
    Nothing -> pure unit
    Just hydExprGyro -> unflushHydrateExprGyro hydExprGyro
  case mb_hydExprGyro' of
    Nothing -> pure unit
    Just hydExprGyro -> flushHydrateExprGyro hydExprGyro
  updateBufferInfo localTokens mb_hydExprGyro'

updateBufferInfo :: forall sn el er. Language sn el => BufferLocalTokens sn el -> Maybe (AnnExprGyro sn el er) -> HK.HookM Aff Unit
updateBufferInfo {slotToken} = case _ of
  Just (CursorGyro (Cursor cursor)) -> tell slotToken (Proxy :: Proxy "info") unit BufferInfoQuery (Proxy :: Proxy "set mbSort") $ Just $ getExprSort cursor.inside
  _ -> tell slotToken (Proxy :: Proxy "info") unit BufferInfoQuery (Proxy :: Proxy "set mbSort") Nothing

-- render

renderSyncExprGyro :: forall sn el er ctx env. Dynamics sn el ctx env => BufferLocal sn el -> SyncExprGyro sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprGyro local (CursorGyro cursor) = renderSyncExprCursor local cursor
renderSyncExprGyro local (SelectGyro select) = renderSyncExprSelect local select

renderSyncExprSelect :: forall sn el er ctx env. Dynamics sn el ctx env => BufferLocal sn el -> SyncExprSelect sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
renderSyncExprSelect local (Select {outside, middle, inside, orientation}) = do
  let outside_middle = outside <> (toPath middle)
  renderSyncExprPath local mempty outside (unPath outside_middle inside) $
    renderSyncExprPath local outside (toPath middle) inside $
      renderSyncExpr local outside_middle inside

renderSyncExprCursor :: forall sn el er ctx env. Dynamics sn el ctx env => BufferLocal sn el -> SyncExprCursor sn el er -> RenderM sn el ctx env (Array (BufferHtml sn el))
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
  let wrapCursor htmls =
        let 
          toolboxInput = ToolboxInput
            { ctx
            , env
            , outside: shrinkAnnExprPath outside
            , inside: shrinkAnnExpr inside
            , enabled: false
            , edits: getEditsAtExprCursor (shrinkAnnExprCursor cursor)
            , initialQuery: getInitialQuery cursor }
          previewInput position = PreviewInput 
            { ctx
            , env
            , position
            , outside: shrinkAnnExprPath outside
            , inside: shrinkAnnExpr inside
            , maybeEdit: Nothing }
        in
        [ HH.slot (Proxy :: Proxy "toolbox") unit toolboxComponent toolboxInput toolboxHandler
        , HH.slot_ (Proxy :: Proxy "preview") LeftPreviewPosition previewComponent (previewInput LeftPreviewPosition) ] <>
        htmls <>
        [ HH.slot_ (Proxy :: Proxy "preview") RightPreviewPosition previewComponent (previewInput RightPreviewPosition) ]
  renderSyncExprPath local mempty outside inside $
    map wrapCursor $
      renderSyncExpr local outside inside

codepoint =
  { "_": fromJust $ Array.head $ String.toCodePointArray "_"
  , "/": fromJust $ Array.head $ String.toCodePointArray "/"
  }