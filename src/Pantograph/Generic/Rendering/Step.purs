module Pantograph.Generic.Rendering.Step where

import Data.Tuple.Nested
import Pantograph.Generic.Rendering.Common
import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Display (Html, display)
import Data.Foldable (foldMap)
import Data.Tree (tooths)
import Data.Tuple (fst, snd)
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language (StepExpr(..), ExprNode)
import Text.Pretty (pretty)
import Todo (todo)
import Type.Proxy (Proxy)

runRenderStepExpr :: forall sn el ctx env.
  Rendering sn el ctx env =>
  StepExpr sn el ->
  Array Html
runRenderStepExpr e = fst $ snd (runRenderM :: Proxy sn /\ _) $ renderStepExpr e

renderStepExpr :: forall sn el ctx env. 
  Rendering sn el ctx env =>
  StepExpr sn el ->
  RenderM sn el ctx env (Array Html)
renderStepExpr e@(StepExpr node kids) = do
  arrangedKids <- arrangeExpr node
    (kids <#> \kid ->
      renderStepExpr kid <#> (_ /\ getStepExprNode kid))
  let htmls = arrangedKids # foldMap case _ of
        ArrangeKid htmls' -> htmls'
        ArrangeHtml htmls' ->
          -- [ El.ℓ [El.Classes [El.ArrangeHtml]]
          --     htmls' ]
          htmls'
  pure $ [El.ℓ (propsStepExpr e) htmls]
renderStepExpr e@(Boundary (dir /\ ch) kid) = do
  htmls <- renderStepExpr kid
  pure 
    [ El.ℓ [El.Classes [El.StepExprBoundaryInfo]]
        [ El.ℓ [El.Classes [El.StepExprBoundaryDirection]] [El.text $ pretty dir]
        , El.ℓ [El.Classes [El.StepExprBoundaryChange]] [display ch]]
    , El.ℓ (propsStepExpr e) htmls ]
renderStepExpr e@(Marker kid) = do
  htmls <- renderStepExpr kid
  pure $ [El.ℓ (propsStepExpr e) htmls]

propsStepExpr :: forall sn el. StepExpr sn el -> El.Props Unit
propsStepExpr e = 
  [ El.Classes case e of
      StepExpr _ _ -> [El.StepExpr] 
      Boundary _ _ -> [El.StepExprBoundary]
      Marker _ -> [El.StepExprMarker]
  , El.StrictHover (const unit)
  ]

getStepExprNode :: forall sn el. StepExpr sn el -> ExprNode sn el
getStepExprNode (StepExpr node _) = node
getStepExprNode (Boundary _ kid) = getStepExprNode kid
getStepExprNode (Marker kid) = getStepExprNode kid