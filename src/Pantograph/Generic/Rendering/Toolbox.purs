module Pantograph.Generic.Rendering.Toolbox where

import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.SearchableArray as SearchableArray
import Data.Tree (class PrettyTreeNode, toPath)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (case_, inj, on)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Pantograph.Generic.Language.Language (prefixExprPathSkeleton)
import Pantograph.Generic.Rendering.Language (MakeAnnExprProps, renderAnnExpr, renderAnnExprPath)
import Pantograph.Generic.Rendering.Style (className)
import Text.Pretty (pretty)
import Todo (todo)
import Type.Proxy (Proxy(..))
import Util (fromJust, fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent as MouseEvent

toolboxComponent :: forall sn el ctx env. Rendering sn el ctx env => H.Component (ToolboxQuery sn el) (ToolboxInput sn el ctx env) (ToolboxOutput sn el) Aff
toolboxComponent = HK.component \{outputToken, queryToken} (ToolboxInput input) -> HK.do
  let queryRefLabel = H.RefLabel "ToolboxInput"
  let getQueryElem = HK.getHTMLElementRef queryRefLabel

  enabled /\ enabledStateId <- HK.useState input.enabled

  ToolboxSelect selectRowIndex selectColIndex /\ selectStateId <- HK.useState $ ToolboxSelect 0 0

  query /\ queryStateId <- HK.useState input.initialQuery

  let
    toEditArray query = case input.edits of
      SearchableEdits sa -> SearchableArray.prioritizedItems query sa <#> snd
      StringEdits f -> f query

    edits = toEditArray query

    getEdits = do
      query <- HK.get queryStateId
      pure $ toEditArray query

    normalizeSelect (ToolboxSelect rowIx colIx) = do
      edits <- getEdits
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
          edits <- getEdits
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
    HH.div [HP.classes [HH.ClassName "Toolbox"]]
      if not enabled then [] else
      [HH.div [HP.classes [HH.ClassName "ToolboxInterior"]]
        [ HH.input 
            [ HP.classes [HH.ClassName "ToolboxInput"]
            , HP.ref queryRefLabel
            , HE.onValueInput \_ -> do
                queryElem <- getQueryElem <#> fromJust
                value <- liftEffect $ HTMLInputElement.value $ fromJust $ HTMLInputElement.fromHTMLElement queryElem
                HK.modify_ queryStateId (const value)
                resetSelect
            ]
        , HH.div [HP.classes [HH.ClassName "EditRows"]] $
            edits # Array.mapWithIndex \rowIndex editRow ->
            HH.div 
              [HP.classes $ [HH.ClassName "EditRow"] <> if rowIndex == selectRowIndex then [HH.ClassName "SelectedEditRow"] else []]
              let 
                colIndex = if rowIndex == selectRowIndex then selectColIndex else 0
                edit = fromJust $ NonEmptyArray.index editRow colIndex
                adjacentIndexedEdits = fromJust $ NonEmptyArray.deleteAt colIndex (editRow # NonEmptyArray.mapWithIndex \colIndex' edit -> {select: ToolboxSelect rowIndex colIndex', edit})
              in
              [HH.div
                [ HP.classes [HH.ClassName "Edit"]
                , HE.onMouseOver \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    modifySelect $ const $ ToolboxSelect rowIndex colIndex
                , HE.onMouseDown \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    submitEdit
                ]
                (fst $ unwrap $ runM input.ctx input.env $
                  renderEdit {adjacentIndexedEdits, modifySelect} (shrinkAnnExprPath input.outside) (shrinkAnnExpr input.inside) edit)]
        ]]

type RenderEditLocals sn el =
  { adjacentIndexedEdits :: Array {select :: ToolboxSelect, edit :: Edit sn el}
  , modifySelect :: (ToolboxSelect -> ToolboxSelect) -> HK.HookM Aff Unit
  }

makeToolboxExprProps :: forall sn el er ctx env. Rendering sn el ctx env => RenderEditLocals sn el -> MakeAnnExprProps sn el er ctx env
makeToolboxExprProps {adjacentIndexedEdits, modifySelect} outside _inside =
  case adjacentIndexedEdits # Array.find \{edit: Edit edit} -> edit.middle # maybe false \middle -> prefixExprPathSkeleton (toPath middle) (shrinkAnnExprPath outside) of
    Nothing -> 
      pure 
        [ HP.classes [className.expr, className.toolboxExpr] ]
    Just {select, edit} ->
      pure 
        [ HP.classes [className.expr, className.toolboxExpr, className.adjacentEditClasp] 
        , HE.onMouseOver \mouseEvent -> do
            liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
            modifySelect $ const select
        ]

renderEdit :: forall sn el ctx env. Rendering sn el ctx env =>
  RenderEditLocals sn el ->
  ExprPath sn el ->
  Expr sn el ->
  Edit sn el ->
  RenderM sn el ctx env (Array (BufferHtml sn el))
renderEdit adjacentIndexedEdits outside inside = case _ of
  Edit edit -> case edit.middle of
    Nothing -> case edit.inside of
      Nothing -> bug "TODO: how to render this kind of Edit?"
      Just inside -> renderAnnExpr outside inside (makeToolboxExprProps adjacentIndexedEdits)
    Just middle -> renderAnnExprPath outside (toPath middle) inside (makeToolboxExprProps adjacentIndexedEdits) case edit.inside of
      Nothing -> pure editHole
      Just inside -> renderAnnExpr outside inside (makeToolboxExprProps adjacentIndexedEdits)

editHole :: forall sn el. Array (BufferHtml sn el)
editHole = [HH.div [HP.classes [HH.ClassName "EditHole"]] [HH.text " "]]
