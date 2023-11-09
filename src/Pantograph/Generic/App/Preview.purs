module Pantograph.Generic.App.Preview (previewComponent) where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.App.Common
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Bug (bug)
import Data.Display (display)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Data.Variant (case_, on)
import Debug as Debug
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Elements as El
import Halogen.Hooks as HK
import Pantograph.Generic.GlobalMessageBoard as GMB
import Type.Proxy (Proxy(..))

previewComponent :: forall sn el ctx env. Dynamics sn el ctx env => H.Component (PreviewQuery sn el) (PreviewInput sn el ctx env) PreviewOutput Aff
previewComponent = HK.component \{queryToken} (PreviewInput input) -> Debug.trace "[render:preview]" \_ -> HK.do

  maybeEdit /\ maybeEditStateId <- HK.useState input.maybeEdit

  HK.useQuery queryToken \(PreviewQuery query) -> (query # _) $ case_
    # on (Proxy :: Proxy "modify maybeEdit") \(f /\ a) -> do
        HK.modify_ maybeEditStateId f
        pure (Just a)

  HK.pure $
    case input.position of
      LeftPreviewPosition ->
        case maybeEdit of
          Nothing -> El.ℓ [El.Classes [El.PreviewLeft]] []
          Just (Edit edit) ->
            let
              middleHtml middle inside =
                fst $ unwrap $ runM input.ctx input.env $
                  renderAnnExprPathLeft
                    (shrinkAnnExprPath input.outside :: ExprPath sn el)
                    (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                    (shrinkAnnExpr input.inside :: Expr sn el)
                    makePreviewExprProps
                    (pure inside)
              classNames = case edit.middle /\ edit.inside of
                Nothing /\ Nothing -> [El.PreviewLeft]
                Just _ /\ Nothing -> [El.PreviewLeftInsert]
                Nothing /\ Just _ -> [El.PreviewLeftPaste]
                Just _ /\ Just _ -> [El.PreviewLeftInsert, El.PreviewLeftPaste]
            in 
            case edit.middle of
              Nothing -> El.ℓ [El.Classes classNames] []
              Just middle -> El.ℓ [El.Classes classNames] (middleHtml middle [])
      RightPreviewPosition ->
        case maybeEdit of
          Nothing -> El.ℓ [El.Classes [El.PreviewRight]] []
          Just (Edit edit) ->
            let 
              insideHtml inside = 
                fst $ unwrap $ runM input.ctx input.env $ 
                  renderAnnExpr
                    (shrinkAnnExprPath input.outside <> maybe mempty toPath edit.middle)
                    inside
                    makePreviewExprProps
              middleHtml middle inside =
                fst $ unwrap $ runM input.ctx input.env $
                  renderAnnExprPathRight
                    (shrinkAnnExprPath input.outside :: ExprPath sn el)
                    (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
                    (shrinkAnnExpr input.inside :: Expr sn el)
                    makePreviewExprProps
                    (pure inside)
              classNames = case edit.middle /\ edit.inside of
                Nothing /\ Nothing -> [El.PreviewRight]
                Just _ /\ Nothing -> [El.PreviewRightInsert]
                Nothing /\ Just _ -> [El.PreviewRightPaste]
                Just _ /\ Just _ -> [El.PreviewRightInsert, El.PreviewRightPaste]
            in 
            case edit.middle of
              Nothing -> case edit.inside of
                Nothing -> GMB.error $ display"TODO: how to preview this kind of Edit?"
                Just inside -> El.ℓ [El.Classes classNames] (insideHtml inside)
              Just middle -> case edit.inside of
                Nothing -> El.ℓ [El.Classes classNames] (middleHtml middle [])
                Just inside -> El.ℓ [El.Classes classNames] (middleHtml middle (insideHtml inside))
      -- RightPreviewPosition ->
      --     case maybeEdit of
      --       Nothing -> El.ℓ [El.Classes [El.PreviewRight]] []
      --       Just (Edit edit) ->
      --         let
      --           middleHtml middle =
      --             fst $ unwrap $ runM input.ctx input.env $
      --               renderAnnExprPathRight
      --                 (shrinkAnnExprPath input.outside :: ExprPath sn el)
      --                 (shrinkAnnExprPath (toPath middle) :: ExprPath sn el)
      --                 (shrinkAnnExpr input.inside :: Expr sn el)
      --                 makePreviewExprProps
      --                 (pure [])

      --           classNames = case edit.middle /\ edit.inside of
      --             Nothing /\ Nothing -> [El.PreviewRight]
      --             Just _ /\ Nothing -> [El.PreviewRightInsert]
      --             Nothing /\ Just _ -> [El.PreviewRightPaste]
      --             Just _ /\ Just _ -> [El.PreviewRightInsert, El.PreviewRightPaste]
      --         in
      --         case edit.middle of
      --           Nothing -> case edit.inside of
      --             Nothing -> GMB.error $ display"TODO: how to preview this kind of Edit?"
      --             Just inside -> El.ℓ [El.Classes classNames] (insideHtml inside)
      --           Just middle -> case edit.inside of
      --             Nothing -> El.ℓ [El.Classes classNames] (middleHtml middle [])
      --             Just inside -> El.ℓ [El.Classes classNames] (middleHtml middle (insideHtml inside))

makePreviewExprProps :: forall sn el er ctx env. MakeAnnExprProps sn el er ctx env (HK.HookM Aff Unit)
makePreviewExprProps outside expr = do
  pure
    [ El.Classes [El.PreviewExpr] ]

