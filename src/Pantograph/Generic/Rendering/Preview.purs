module Pantograph.Generic.Rendering.Preview where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.State (get)
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
import Pantograph.Generic.Language (shrinkAnnExpr, shrinkAnnExprPath)
import Pantograph.Generic.Rendering.Language (MakeAnnExprProps, MakeSyncExprProps, renderAnnExpr, renderAnnExprPath)

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
            Nothing -> []
            Just (ReplaceToolboxItem expr) -> fst $ unwrap $ runM input.ctx input.env $ renderAnnExpr input.renderer (shrinkAnnExprPath input.outside) expr makePreviewExprProps
            Just (InsertToolboxItem path) ->
              -- fst $ unwrap $ runM input.ctx input.env $ renderAnnExprPath input.renderer (shrinkAnnExprPath input.outside) path (shrinkAnnExpr input.inside) makePreviewExprProps (hole "TODO")
              hole "TODO"
      RightPreviewPosition ->
        HH.div
          [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewRight"]]
          case maybeItem of
            Nothing -> []
            Just (ReplaceToolboxItem _) -> []
            -- Just (InsertToolboxItem path) -> hole "TODO"
            _ -> []

makePreviewExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env
makePreviewExprProps (Renderer renderer) outside expr = do
  ctx <- ask
  env <- get
  pure 
    [ HP.classes [HH.ClassName "Expr PreviewExpr"] ]
