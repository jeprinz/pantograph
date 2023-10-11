module Pantograph.Generic.Rendering.Preview where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tree (class PrettyTreeNode)
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Hole (hole)

previewComponent :: forall sn el ctx env. Show sn => Show el => PrettyTreeNode el => H.Component (PreviewQuery sn el) (PreviewInput sn el ctx env) PreviewOutput Aff
previewComponent = HK.component \{queryToken} (PreviewInput input) -> HK.do

  maybeItem /\ maybeItemStateId <- HK.useState input.maybeItem

  HK.useQuery queryToken case _ of
    ModifyItemPreview f a -> do
      HK.modify_ maybeItemStateId f
      pure (Just a)

  HK.pure $
    case input.position of
      LeftPreviewPosition ->
        HH.div
          [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewLeft"]]
          case maybeItem of
            -- Nothing -> []
            -- Just (ReplaceToolboxItem expr) -> hole "TODO" -- fst $ unwrap $ runM input.ctx input.env $ hole "TODO" -- $ renderExpr input.renderer ?a $ hole "TODO"
            -- Just (InsertToolboxItem path) -> hole "TODO"
            _ -> []
      RightPreviewPosition ->
        HH.div
          [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewRight"]]
          case maybeItem of
            -- Nothing -> []
            -- Just (ReplaceToolboxItem expr) -> []
            -- Just (InsertToolboxItem path) -> hole "TODO"
            _ -> []

