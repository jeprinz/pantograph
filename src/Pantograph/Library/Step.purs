module Pantograph.Library.Step where

import Data.Tuple.Nested
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Display (display)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tree (epL, epR)
import Data.Tree.Common (injectTreeIntoChange)
import Halogen.Elements as El
import Pantograph.Generic.Rendering.TerminalItems as TI
import Text.Pretty (pretty)
import Todo (todo)
import Util (debug, debugM, findIndexMap, extractAt, splitAt, splitAtFindMap)

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir

makeDefaultDownSteppingRule :: forall sn el.
  {getChangingRule :: el -> ChangingRule sn} ->
  SteppingRule sn el
makeDefaultDownSteppingRule {getChangingRule} = SteppingRule "defaultDown" case _ of
  Down /\ ch %.| a@(EN label sortSigma _ %. kids) -> do
    let ChangingRule rule = getChangingRule label
    let sort = getStepExprSort a
    let _ = if epL ch == sort then unit else bug $ "boundary's change's left endpoint didn't match sort of internal expr" <> "; ch = " <> pretty ch <> "; epL ch = " <> pretty (epL ch) <> "; sort " <> pretty sort
    changeSigma /\ chBackUp <- doOperation ch rule.parent
    let changeSigma' = changeSigma <> (injectTreeIntoChange <$> sortSigma)
    debugM "makeDefaultDownSteppingRule" {sort: pretty sort, changeSigma: pretty changeSigma', chBackUp: pretty chBackUp, changeSigma': pretty changeSigma'}
    let kidSorts = applyRuleSortVarSubst changeSigma' <$> rule.kids
    let kidsWithBoundaries = Array.zipWith (\ch' kid -> Down /\ ch' %.| kid) kidSorts kids
    TI.debugM (El.τ "defaultDown") 
      { ch: display ch
      , sort: display sort
      , changeSigma: display changeSigma
      , changeSigma': display changeSigma'
      , chBackUp: display chBackUp
      , ruleKidSorts: display $ rule.kids
      , kidSorts: display kidSorts
      , kidsWithBoundaries: display kidsWithBoundaries
      }
    Just $ Up /\ chBackUp %.| (EN label (epR <$> changeSigma') {} %. kidsWithBoundaries)
  _ -> Nothing

makeDefaultUpSteppingRule :: forall sn el.
  {getChangingRule :: el -> ChangingRule sn} ->
  SteppingRule sn el
makeDefaultUpSteppingRule {getChangingRule} = SteppingRule "defaultUp" case _ of
  EN label sortSigma _ %. kids -> do
    let ChangingRule rule = getChangingRule label
    leftKidsAndSorts /\ (ch /\ kid /\ ruleCh) /\ rightKidsAndSorts <- splitAtFindMap findUpBoundary $ Array.zip kids rule.kids
    changeSigma /\ chBackDown <- doOperation ch ruleCh
    let changeSigma' = changeSigma <> (injectTreeIntoChange <$> sortSigma)
    let wrapKid (kid' /\ ruleSort') = Down /\ applyRuleSortVarSubst changeSigma' ruleSort' %.| kid'
    let leftKids = wrapKid <$> leftKidsAndSorts
    let rightKids = wrapKid <$> rightKidsAndSorts
    let parentSigma = applyRuleSortVarSubst changeSigma' rule.parent
    let sortSigma' = epR <$> changeSigma'
    Just $ Up /\ parentSigma %.| (EN label sortSigma' {} %. (leftKids <> [Down /\ chBackDown %.| kid] <> rightKids))
  _ -> Nothing
  where
  findUpBoundary = case _ of
    (Up /\ ch %.| kid) /\ sort -> Just (ch /\ kid /\ sort)
    _ /\ _ -> Nothing

-- | Erase the boundary if the boundary's change satisfies a condition.
eraseBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Boolean) -> SteppingRule sn el
eraseBoundary mbDir cond = SteppingRule "eraseBoundary" case _ of
  Boundary (dir /\ ch) kid 
    | matchDirection mbDir dir
    , cond ch 
    -> Just kid
  _ -> Nothing

-- | Interpret a boundary with a change as a transformation over the inner expression.
dischargeBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Maybe (StepExpr sn el -> Maybe (StepExpr sn el))) -> SteppingRule sn el
dischargeBoundary mbDir f = SteppingRule "dischargeBoundary" case _ of
  Boundary (dir /\ ch) kid
    | matchDirection mbDir dir
    , Just f' <- f ch
    -> f' kid
  _ -> Nothing

-- | Conditionalize a `SteppingRule`.
unless :: forall l r. String -> (StepExpr l r -> Boolean) -> SteppingRule l r -> SteppingRule l r
unless condName cond (SteppingRule name f) = SteppingRule ("unless " <> condName <> " " <> name) \e -> if cond e then Nothing else f e
