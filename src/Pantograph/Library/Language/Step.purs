module Pantograph.Library.Language.Step where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tree (epL, epR)
import Data.Tree.Common (injectTreeIntoChange)
import Text.Pretty (pretty)
import Todo (todo)
import Util (findIndexMap, indexDeleteAt, splitAt, splitAtFindMap)

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir

defaultDown :: forall sn el.
  {getChangingRule :: el -> ChangingRule sn} ->
  SteppingRule sn el
defaultDown {getChangingRule} = SteppingRule case _ of
  Down /\ ch %.| a@(EN label exSigma _ %. kids) -> do
    let ChangingRule rule = getChangingRule label
    let sort = getStepExprSort a
    let _ = if epL ch == sort then unit else bug $ "boundary's change's left endpoint didn't match sort of internal expr" <> "; ch = " <> pretty ch <> "; epL ch = " <> pretty (epL ch) <> "; sort " <> pretty sort
    chSigma /\ chBackUp <- doOperation ch rule.parent
    let chSigma' = chSigma <> (injectTreeIntoChange <$> exSigma)
    let kidSorts = applyRuleSortVarSubst chSigma' <$> rule.kids
    let kidsWithBoundaries = Array.zipWith (\ch' kid -> Down /\ ch' %.| kid) kidSorts kids
    Just $ Up /\ chBackUp %.| (EN label (epR <$> chSigma') {} %. kidsWithBoundaries)
  _ -> Nothing

defaultUp :: forall sn el.
  {getChangingRule :: el -> ChangingRule sn} ->
  SteppingRule sn el
defaultUp {getChangingRule} = SteppingRule case _ of
  EN label exSigma _ %. kids -> do
    let ChangingRule rule = getChangingRule label
    leftKidsAndSorts /\ (ch /\ kid /\ ruleCh) /\ rightKidsAndSorts <- splitAtFindMap findUpBoundary $ Array.zip kids rule.kids
    chSigma /\ chBackDown <- doOperation ch ruleCh
    let chSigma' = chSigma <> (injectTreeIntoChange <$> exSigma)
    let wrapKid (kid' /\ ruleSort') = Down /\ applyRuleSortVarSubst chSigma' ruleSort' %.| kid'
    let leftKids = wrapKid <$> leftKidsAndSorts
    let rightKids = wrapKid <$> rightKidsAndSorts
    let parentSigma = applyRuleSortVarSubst chSigma' rule.parent
    let exSigma' = epR <$> chSigma'
    Just $ Up /\ parentSigma %.| (EN label exSigma' {} %. (leftKids <> [Down /\ chBackDown %.| kid] <> rightKids))
  _ -> Nothing
  where
  findUpBoundary = case _ of
    (Up /\ ch %.| kid) /\ sort -> Just (ch /\ kid /\ sort)
    _ /\ _ -> Nothing

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
