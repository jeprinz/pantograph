module Pantograph.Generic.Rendering.Toolbox where

import Pantograph.Generic.Rendering.Common
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Data.Tree (class PrettyTreeNode, toPath)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, inj, on)
import Effect.Aff (Aff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Pantograph.Generic.Language (Edit(..), shrinkAnnExpr, shrinkAnnExprPath)
import Pantograph.Generic.Rendering.Language (MakeAnnExprProps, renderAnnExpr, renderAnnExprPath)
import Pantograph.Generic.Rendering.Style (className)
import Type.Proxy (Proxy(..))
import Util (fromJust, fromJust')
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent as MouseEvent

toolboxComponent :: forall sn el ctx env. Rendering sn el ctx env => H.Component (ToolboxQuery sn el) (ToolboxInput sn el ctx env) (ToolboxOutput sn el) Aff
toolboxComponent = HK.component \{outputToken, queryToken} (ToolboxInput input) -> HK.do
  let inputRefLabel = H.RefLabel "ToolboxInput"
  let getInputElem = HK.getHTMLElementRef inputRefLabel <#> fromMaybe' (\_ -> bug "modifyIsEnabledToolbox: could not find element of inputRefLabel")

  isEnabled /\ isEnabledStateId <- HK.useState input.isEnabled

  ToolboxSelect selectRowIndex selectColIndex /\ selectStateId <- HK.useState $ ToolboxSelect 0 0

  let
    normalizeSelect (ToolboxSelect rowIx colIx) =
      if Array.length input.edits == 0 then ToolboxSelect 0 0 else
      let rowIx' = rowIx `mod` Array.length input.edits in
      let row = fromJust' "normalizeSelect" $ Array.index input.edits rowIx' in
      let colIx' = colIx `mod` NonEmptyArray.length row in
      ToolboxSelect rowIx' colIx'

    getSelectedEdit = do
      HK.get isEnabledStateId >>= case _ of
        false -> pure Nothing
        true -> do
          ToolboxSelect selectRowIndex' selectColIndex' <- HK.get selectStateId
          pure $
            input.edits #
              (_ Array.!! selectRowIndex') >>>
              map ((_ NonEmptyArray.!! selectColIndex') >>> fromJust' "getSelectedEdit")

    freshenPreview = do
      getSelectedEdit >>= case _ of
        Nothing -> HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "preview edit") Nothing
        Just edit -> HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "preview edit") $ Just edit

    modifyIsEnabledToolbox f = do
      isEnabled' <- HK.modify isEnabledStateId f
      freshenPreview
      when isEnabled' do
        inputElem <- getInputElem
        liftEffect $ HTMLElement.focus inputElem

    modifyToolboxSelect f = do
      HK.modify_ selectStateId (f >>> normalizeSelect)
      freshenPreview

    resetSelect = modifyToolboxSelect $ const $ ToolboxSelect 0 0

    submitEdit =
      getSelectedEdit >>= case _ of
        Nothing -> pure unit
        Just edit -> do
          resetSelect
          HK.raise outputToken $ ToolboxOutput $ inj (Proxy :: Proxy "submit edit") edit

  HK.useQuery queryToken \(ToolboxQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "modify isEnabled") (\(f /\ a) -> do
        modifyIsEnabledToolbox f
        pure (Just a)
      )
    # on (Proxy :: Proxy "get isEnabled") (\k -> do
        pure (Just (k isEnabled))
      )
    # on (Proxy :: Proxy "modify select") (\(f /\ a) -> do
        modifyToolboxSelect f
        pure (Just a)
      )
    # on (Proxy :: Proxy "submit edit") (\(_ /\ a) -> do
        submitEdit
        pure (Just a)
      )
    # on (Proxy :: Proxy "modify query") (\(f /\ a) -> do
        inputElem <- getInputElem <#> HTMLInputElement.fromHTMLElement >>> fromJust
        value <- liftEffect $ HTMLInputElement.value inputElem
        liftEffect $ HTMLInputElement.setValue (f value) inputElem
        pure (Just a)
      )

  HK.pure $
    HH.div [HP.classes [HH.ClassName "Toolbox"]]
      if not isEnabled then [] else
      [HH.div [HP.classes [HH.ClassName "ToolboxInterior"]]
        [ HH.input [HP.classes [HH.ClassName "ToolboxInput"], HP.ref inputRefLabel] --, HP.autofocus true]
        , HH.div [HP.classes [HH.ClassName "EditRows"]] $
            input.edits # Array.mapWithIndex \rowIndex itemRow ->
            HH.div 
              [HP.classes $ [HH.ClassName "EditRow"] <> if rowIndex == selectRowIndex then [HH.ClassName "SelectedEditRow"] else []]
              let 
                colIndex = if rowIndex == selectRowIndex then selectColIndex else 0
                item = fromJust' "toolboxComponent.render" $ NonEmptyArray.index itemRow colIndex
              in
              [HH.div
                [ HP.classes [HH.ClassName "Edit"]
                , HE.onMouseOver \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    modifyToolboxSelect $ const $ ToolboxSelect rowIndex colIndex
                , HE.onMouseDown \mouseEvent -> do
                    liftEffect $ Event.stopPropagation $ MouseEvent.toEvent mouseEvent
                    submitEdit
                ]
                (fst $ unwrap $ runM input.ctx input.env $
                  renderEdit (shrinkAnnExprPath input.outside) (shrinkAnnExpr input.inside) item)]
        ]]

makeToolboxExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env
makeToolboxExprProps outside inside = do
  pure 
    [ HP.classes [className.expr, className.toolboxExpr] ]

renderEdit outside inside = case _ of
  Edit edit -> case edit.middle of
    Nothing -> case edit.inside of
      Nothing -> bug "TODO: how to render this kind of Edit?"
      Just inside -> renderAnnExpr outside inside makeToolboxExprProps
    Just middle -> renderAnnExprPath outside (toPath middle) inside makeToolboxExprProps case edit.inside of
      Nothing -> pure editHole
      Just inside -> renderAnnExpr outside inside makeToolboxExprProps

editHole :: forall sn el. Array (BufferHtml sn el)
editHole = [HH.div [HP.classes [HH.ClassName "EditHole"]] [HH.text " "]]
