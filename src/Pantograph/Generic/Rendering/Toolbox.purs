module Pantograph.Generic.Rendering.Toolbox where

import Pantograph.Generic.Rendering.Common
import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tree (class PrettyTreeNode)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Hole (hole)
import Pantograph.Generic.Language (shrinkAnnExpr, shrinkAnnExprPath)
import Pantograph.Generic.Rendering.Language (MakeAnnExprProps, renderAnnExpr, renderAnnExprPath)
import Util (fromJust')

toolboxComponent :: forall sn el ctx env. Show sn => Show el => PrettyTreeNode el => H.Component (ToolboxQuery sn el) (ToolboxInput sn el ctx env) (ToolboxOutput sn el) Aff
toolboxComponent = HK.component \{queryToken} (ToolboxInput input) -> HK.do

  isEnabled /\ isEnabledStateId <- HK.useState input.isEnabled

  ToolboxSelect selectRowIndex selectColIndex /\ selectStateId <- HK.useState $ ToolboxSelect 0 0

  let normalizeToolboxSelect (ToolboxSelect rowIx colIx) =
        if Array.length input.itemRows == 0 then ToolboxSelect 0 0 else
        let rowIx' = rowIx `mod` Array.length input.itemRows in
        let row = fromJust' "normalizeToolboxSelect" $ Array.index input.itemRows rowIx' in
        let colIx' = colIx `mod` NonEmptyArray.length row in
        ToolboxSelect rowIx' colIx'

  HK.useQuery queryToken case _ of
    ModifyIsEnabledToolbox f a -> do
      HK.modify_ isEnabledStateId f
      pure (Just a)
    ModifySelectToolbox f a -> do
      HK.modify_ selectStateId (f >>> normalizeToolboxSelect)
      pure (Just a)
    GetIsEnabledToolbox k -> do
      pure (Just (k isEnabled))

  HK.pure $
    HH.div [HP.classes [HH.ClassName "Toolbox"]]
      if not isEnabled then [] else
      [HH.div [HP.classes [HH.ClassName "ToolboxInterior"]]
        [ HH.input [HP.classes [HH.ClassName "ToolboxInput"], HP.autofocus true]
        , HH.div [HP.classes [HH.ClassName "ToolboxItemRows"]] $
            [HH.div [HP.classes [HH.ClassName "ToolboxItemRow"]]
              [HH.div [HP.classes [HH.ClassName "ToolboxItem"]]
                [HH.text "<toolbox item>"]]]
            <>
            [HH.div [HP.classes [HH.ClassName "ToolboxItemRow", HH.ClassName "SelectedToolboxItemRow"]]
              [HH.div [HP.classes [HH.ClassName "ToolboxItem"]]
                [HH.text "<toolbox item>"]]]
            <>
            (input.itemRows # Array.mapWithIndex \rowIndex itemRow ->
            HH.div 
              [HP.classes $ [HH.ClassName "ToolboxItemRow"] <> if rowIndex == selectRowIndex then [HH.ClassName "SelectedToolboxItemRow"] else []]
              let item = 
                    if rowIndex == selectRowIndex
                      then fromJust' "toolboxComponent.render" $ NonEmptyArray.index itemRow selectColIndex
                      else fromJust' "toolboxComponent.render" $ NonEmptyArray.index itemRow 0
              in
              [HH.div [HP.classes [HH.ClassName "ToolboxItem"]]
                (fst $ unwrap $ runM input.ctx input.env $
                  renderToolboxItem input.renderer (shrinkAnnExprPath input.outside) (shrinkAnnExpr input.inside) item)])
        ]]

makeToolboxExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env
makeToolboxExprProps (Renderer renderer) outside inside = do
  pure 
    [ HP.classes [HH.ClassName "Expr", HH.ClassName "ToolboxExpr"] ]

renderToolboxItem renderer outside inside = case _ of
  ReplaceToolboxItem expr -> renderAnnExpr renderer outside expr makeToolboxExprProps
  InsertToolboxItem path -> renderAnnExprPath renderer outside path inside makeToolboxExprProps (pure toolboxHole)

toolboxHole :: forall sn el. Array (BufferHtml sn el)
toolboxHole = [HH.div [HP.classes [HH.ClassName "ToolboxHole"]] [HH.text "*"]]
