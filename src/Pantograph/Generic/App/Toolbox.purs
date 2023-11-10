module Pantograph.Generic.App.Toolbox (toolboxComponent) where

import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Display (display)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StringTaggedArray as StringTaggedArray
import Data.Tree (toPath)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, inj, on)
import Debug as Debug
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Pantograph.Generic.GlobalMessageBoard as GMB
import Type.Proxy (Proxy(..))
import Util (fromJust, fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent as MouseEvent

toolboxComponent :: forall sn el ctx env. Dynamics sn el ctx env => H.Component (ToolboxQuery sn el) (ToolboxInput sn el ctx env) (ToolboxOutput sn el) Aff
toolboxComponent = HK.component \{outputToken, queryToken} (ToolboxInput input) -> Debug.trace "[render:toolbox]" \_ -> HK.do
  let queryRefLabel = H.RefLabel "ToolboxInput"
  let getQueryElem = HK.getHTMLElementRef queryRefLabel

  enabled /\ enabledStateId <- HK.useState input.enabled

  ToolboxSelect selectRowIndex selectColIndex /\ selectStateId <- HK.useState $ ToolboxSelect 0 0

  query /\ queryStateId <- HK.useState input.initialQuery

  let
    toEditArray query = 
      let Edits {stringTaggedEdits} = input.edits in
      StringTaggedArray.getPrioritizedItems query stringTaggedEdits <#> snd

    edits = toEditArray query

    getEditsAtSort = do
      query <- HK.get queryStateId
      pure $ toEditArray query

    normalizeSelect (ToolboxSelect rowIx colIx) = do
      edits <- getEditsAtSort
      let editsLength = Array.length edits
      if editsLength == 0 
        then pure $ ToolboxSelect 0 0 
        else do
          let rowIx' = rowIx `mod` editsLength
          let row = fromJust' "normalizeSelect" $ Array.index edits rowIx'
          let colIx' = colIx `mod` NonEmptyArray.length row
          pure $ ToolboxSelect rowIx' colIx'

    getSelectedEdit = do
      HK.get enabledStateId >>= case _ of
        false -> pure Nothing
        true -> do
          ToolboxSelect selectRowIndex' selectColIndex' <- HK.get selectStateId
          edits <- getEditsAtSort
          pure $
            edits #
              (_ Array.!! selectRowIndex') >>>
              map ((_ NonEmptyArray.!! selectColIndex') >>> fromJust' "getSelectedEdit")

    freshenPreview = do
      getSelectedEdit >>= case _ of
        Nothing -> HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "preview edit") Nothing
        Just edit -> HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "preview edit") $ Just edit

    modifyEnabledToolbox f = do
      enabled' <- HK.modify enabledStateId f
      resetQuery
      getQueryElem >>= case _ of
        Nothing -> pure unit
        Just queryElem -> liftEffect $ HTMLElement.focus queryElem

    modifySelect f = do
      select <- HK.get selectStateId
      select' <- normalizeSelect (f select)
      HK.modify_ selectStateId (const select')
      freshenPreview

    resetSelect = do
      modifySelect $ const $ ToolboxSelect 0 0

    submitEdit = do
      getSelectedEdit >>= case _ of
        Nothing -> pure unit
        Just edit -> do
          resetQuery
          HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "submit edit") edit

    modifyQuery f = do
      query <- HK.modify queryStateId f
      getQueryElem >>= case _ of
        Nothing -> pure unit
        Just queryElem -> liftEffect $ HTMLInputElement.setValue query $ fromJust $ HTMLInputElement.fromHTMLElement queryElem
      resetSelect

    resetQuery = do
      modifyQuery (const input.initialQuery)

  HK.useQuery queryToken \(ToolboxQuery q) -> (q # _) $ case_
    # on (Proxy :: Proxy "modify enabled") (\(f /\ a) -> do
        modifyEnabledToolbox f
        pure (Just a)
      )
    # on (Proxy :: Proxy "get enabled") (\k -> do
        pure (Just (k enabled))
      )
    # on (Proxy :: Proxy "modify select") (\(f /\ a) -> do
        modifySelect f
        pure (Just a)
      )
    # on (Proxy :: Proxy "submit edit") (\(_ /\ a) -> do
        submitEdit
        pure (Just a)
      )
    # on (Proxy :: Proxy "modify query") (\(f /\ a) -> do
        modifyQuery f
        pure (Just a)
      )

  HK.pure $
    El.ℓ [El.Classes [El.Toolbox]]
      if not enabled then [] else
      [El.ℓ [El.Classes [El.ToolboxInterior]]
        [ HH.input 
            [ HP.classes [HH.ClassName "ToolboxInput"]
            , HP.ref queryRefLabel
            , HE.onValueInput \_ -> do
                queryElem <- getQueryElem <#> fromJust
                value <- liftEffect $ HTMLInputElement.value $ fromJust $ HTMLInputElement.fromHTMLElement queryElem
                HK.modify_ queryStateId (const value)
                resetSelect
            ]
        , El.ℓ [El.Classes [El.EditRow]] $
            edits # Array.mapWithIndex \rowIndex editRow ->
            El.ℓ [El.Classes $ if rowIndex == selectRowIndex then [El.SelectedEditRow] else [El.EditRow]]
              let 
                colIndex = if rowIndex == selectRowIndex then selectColIndex else 0
                edit = fromJust $ NonEmptyArray.index editRow colIndex
                adjacentIndexedEdits = fromJust $ NonEmptyArray.deleteAt colIndex (editRow # NonEmptyArray.mapWithIndex \colIndex' edit -> {select: ToolboxSelect rowIndex colIndex', edit})
              in
              [El.ℓ
                [ El.Classes [El.Edit]
                , El.OnMouseOver \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    modifySelect $ const $ ToolboxSelect rowIndex colIndex
                , El.OnMouseDown \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    submitEdit
                ]
                (fst $ unwrap $ runM input.ctx input.env $
                  renderEdit {adjacentIndexedEdits, modifySelect, outside: shrinkAnnExprPath input.outside} (shrinkAnnExprPath input.outside) (shrinkAnnExpr input.inside) edit)]
        ]]

type RenderEditLocals sn el =
  { outside :: ExprPath sn el
  , adjacentIndexedEdits :: Array {select :: ToolboxSelect, edit :: Edit sn el}
  , modifySelect :: (ToolboxSelect -> ToolboxSelect) -> HK.HookM Aff Unit
  }

makeToolboxExprProps :: forall sn el er ctx env. Dynamics sn el ctx env => RenderEditLocals sn el -> MakeAnnExprProps sn el er ctx env (HK.HookM Aff Unit)
makeToolboxExprProps {adjacentIndexedEdits, modifySelect, outside: toolboxOutside} outside _inside =
  case adjacentIndexedEdits # Array.find \{edit: Edit edit} -> edit.middle # maybe false \middle -> prefixExprPathSkeleton (toolboxOutside <> toPath middle) (shrinkAnnExprPath outside) of
    Nothing -> 
      pure 
        [ El.Classes [El.ToolboxExpr] ]
    Just {select} ->
      pure
        [ El.Classes [El.AdjacentEditClasp]
        , El.OnMouseOver \mouseEvent -> do
            liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
            modifySelect $ const select  ]

renderEdit :: forall sn el ctx env. Dynamics sn el ctx env =>
  RenderEditLocals sn el ->
  ExprPath sn el ->
  Expr sn el ->
  Edit sn el ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderEdit adjacentIndexedEdits outside inside = case _ of
  Edit edit -> case edit.middle of
    Nothing -> case edit.inside of
      Nothing -> GMB.error $ display"TODO: how to render this kind of Edit?"
      Just inside -> renderAnnExpr outside inside (makeToolboxExprProps adjacentIndexedEdits)
    Just middle -> renderAnnExprPath outside (toPath middle) inside (makeToolboxExprProps adjacentIndexedEdits) case edit.inside of
      Nothing -> pure editHole
      Just inside -> renderAnnExpr outside inside (makeToolboxExprProps adjacentIndexedEdits)

editHole :: forall sn el. Array (BufferHtml sn el)
editHole = [El.ℓ [El.Classes [El.EditHole]] [El.text " "]]
