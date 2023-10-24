module Pantograph.Generic.Rendering.Preview where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Data.Maybe (Maybe(..), isJust)
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
          case maybeEdit of
            Nothing -> HH.div [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewLeft"]] []
            Just (Edit edit) ->
              let 
                insideHtml inside = 
                  fst $ unwrap $ runM input.ctx input.env $ 
                    renderAnnExpr
                      (shrinkAnnExprPath input.outside :: ExprPath sn el)
                      inside
                      makePreviewExprProps
                middleHtml middle inside =
                  fst $ unwrap $ runM input.ctx input.env $
                    renderAnnExprPathLeft
                      (shrinkAnnExprPath input.outside :: ExprPath sn el)
                      (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                      (shrinkAnnExpr input.inside :: Expr sn el)
                      makePreviewExprProps
                      (pure inside)
                classes = 
                  [HH.ClassName "Preview", HH.ClassName "PreviewLeft"] <>
                  (if isJust edit.middle then [HH.ClassName "PreviewLeftInsert"] else []) <>
                  (if isJust edit.inside then [HH.ClassName "PreviewLeftPaste"] else [])
              in 
              case edit.middle of
                Nothing -> case edit.inside of
                  Nothing -> bug "TODO: how to preview this kind of Edit?"
                  Just inside -> HH.div [HP.classes classes] (insideHtml inside)
                Just middle -> case edit.inside of
                  Nothing -> HH.div [HP.classes classes] (middleHtml middle [])
                  Just inside -> HH.div [HP.classes classes] (middleHtml middle (insideHtml inside))
      RightPreviewPosition ->
          case maybeEdit of
            Nothing -> HH.div [HP.classes [HH.ClassName "Preview", HH.ClassName "PreviewRight"]] []
            Just (Edit edit) ->
              let
                middleHtml middle =
                  fst $ unwrap $ runM input.ctx input.env $
                    renderAnnExprPathRight
                      (shrinkAnnExprPath input.outside :: ExprPath sn el)
                      (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                      (shrinkAnnExpr input.inside :: Expr sn el)
                      makePreviewExprProps
                      (pure [])

                classes = 
                  [HH.ClassName "Preview", HH.ClassName "PreviewRight"] <>
                  (if isJust edit.middle then [HH.ClassName "PreviewRightInsert"] else []) <>
                  (if isJust edit.inside then [HH.ClassName "PreviewRightPaste"] else [])
              in
              case edit.middle of
                Nothing -> HH.div [HP.classes classes] []
                Just middle -> HH.div [HP.classes classes] (middleHtml middle)

makePreviewExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env
makePreviewExprProps outside expr = do
  ctx <- ask
  env <- get
  pure 
    [ HP.classes [className.expr, className.previewExpr] ]

