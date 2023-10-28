module Pantograph.Library.Language.Step where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Bug (bug)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tree (epL)
import Text.Pretty (pretty)
import Todo (todo)

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir

defaultDown :: forall sn el.
  {getSortingRule :: el -> SortingRule sn} ->
  SteppingRule sn el
defaultDown {getSortingRule} = SteppingRule case _ of
  Down /\ ch %.| a@(EN label sigma _ %. kids) -> do
    let SortingRule rule = getSortingRule label
    let sort = getStepExprSort a
    let _ = if epL ch == sort then unit else
          bug $ 
            "defaultDown: boundary's change's left endpoint didn't match sort of internal expr" <> 
            "; ch = " <> pretty ch <>
            "; epL ch = " <> pretty (epL ch) <>
            "; sort " <> pretty sort
    -- chSigma /\ chBackUp <- doOperation ch ?a
    -- ?a
    todo "defaultDown"
      
  _ -> Nothing

-- | Erase the boundary if the boundary's change satisfies a condition.
eraseBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Boolean) -> SteppingRule sn el
eraseBoundary mbDir cond = SteppingRule case _ of
  Boundary (dir /\ ch) kid 
    | matchDirection mbDir dir
    , cond ch 
    -> Just kid
  _ -> Nothing

-- | Interpret a boundary with a change as a transformation over the inner expression.
dischargeBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Maybe (StepExpr sn el -> Maybe (StepExpr sn el))) -> SteppingRule sn el
dischargeBoundary mbDir f = SteppingRule case _ of
  Boundary (dir /\ ch) kid
    | matchDirection mbDir dir
    , Just f' <- f ch
    -> f' kid
  _ -> Nothing
