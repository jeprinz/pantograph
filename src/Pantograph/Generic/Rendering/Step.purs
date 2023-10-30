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
          [ HH.div [HP.classes [HH.ClassName "ArrangeHtml"]]
              htmls' ]
  pure $ [HH.div (propsStepExpr e) htmls]
renderStepExpr e@(Boundary (dir /\ ch) kid) = do
  htmls <- renderStepExpr kid
  pure 
    [ HH.span [HP.classes [HH.ClassName "StepExprBoundaryInfo"]] 
        [ HH.span [HP.classes [HH.ClassName "StepExprBoundaryDirection"]] [HH.text $ pretty dir]
        , HH.span [HP.classes [HH.ClassName "StepExprBoundaryChange"]] [display ch] ]
    , HH.div (propsStepExpr e) htmls ]
renderStepExpr e@(Marker kid) = do
  htmls <- renderStepExpr kid
  pure $ [HH.div (propsStepExpr e) htmls]

propsStepExpr e = 
  [ HP.classes $ Array.fold
      [ [HH.ClassName "Expr"]
      , [HH.ClassName "StepExpr"]
      , case e of
          StepExpr _ _ -> []
          Boundary _ _ -> [HH.ClassName "StepExprBoundary"]
          Marker _ -> [HH.ClassName "StepExprMarker"] ]
  ]

getStepExprNode :: forall sn el. StepExpr sn el -> ExprNode sn el
getStepExprNode (StepExpr node _) = node
getStepExprNode (Boundary _ kid) = getStepExprNode kid
getStepExprNode (Marker kid) = getStepExprNode kid