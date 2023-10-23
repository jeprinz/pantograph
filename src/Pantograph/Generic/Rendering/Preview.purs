module Pantograph.Generic.Rendering.Preview where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Pantograph.Generic.Rendering.Style (className)
import Type.Proxy (Proxy(..))

previewComponent :: forall sn el ctx env. Rendering sn el ctx env => H.Component (PreviewQuery sn el) (PreviewInput sn el ctx env) PreviewOutput Aff
previewComponent = HK.component \{queryToken} (PreviewInput input) -> HK.do

  maybeEdit /\ maybeEditStateId <- HK.useState input.maybeEdit

  HK.useQuery queryToken \(PreviewQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "modify maybeEdit") \(f /\ a) -> do
        HK.modify_ maybeEditStateId f
        pure (Just a)

  HK.pure $
    case input.position of
      LeftPreviewPosition ->
        HH.div
          [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewLeft"]]
          case maybeEdit of
            Nothing -> []
            Just (PasteEdit (Paste {inside})) -> 
              fst $ unwrap $ runM input.ctx input.env $ 
                renderAnnExpr
                  (shrinkAnnExprPath input.outside)
                  inside
                  makePreviewExprProps
            Just (InsertEdit (Insert {middle})) ->
              fst $ unwrap $ runM input.ctx input.env $
                renderAnnExprPathLeft
                  (shrinkAnnExprPath input.outside :: ExprPath sn el)
                  (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                  (shrinkAnnExpr input.inside :: Expr sn el)
                  makePreviewExprProps
                  (pure [])
      RightPreviewPosition ->
        HH.div
          [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewRight"]]
          case maybeEdit of
            Nothing -> []
            Just (PasteEdit _) -> []
            Just (InsertEdit (Insert {middle})) ->
              fst $ unwrap $ runM input.ctx input.env $
                renderAnnExprPathRight
                  (shrinkAnnExprPath input.outside :: ExprPath sn el)
                  (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                  (shrinkAnnExpr input.inside :: Expr sn el)
                  makePreviewExprProps
                  (pure [])

makePreviewExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env
makePreviewExprProps outside expr = do
  ctx <- ask
  env <- get
  pure 
    [ HP.classes [className.expr, className.previewExpr] ]

