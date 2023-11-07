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
import Data.Supertype as Supertype
import Data.Tree (epL, epR, tooths)
import Halogen.Elements as El
import Pantograph.Generic.GlobalMessageBoard as GMB
import Text.Pretty (pretty)
import Todo (todo)
import Util (debug, debugM, extractAt, findIndexMap, fromJust, splitAt, splitAtFindMap)

makeDefaultDownSteppingRule :: forall sn el. SteppingRule sn el
makeDefaultDownSteppingRule = SteppingRule "defaultDown" case _ of
  Down /\ ch %.| (EN label _ er %. kids) -> do
    let SortingRule rule = getSortingRule label
    changeSigma <- unifyChangeWithRuleChange ch (Supertype.inject rule.parent)
    let kidChanges = rule.kids <#> \kid -> applyRuleSortVarSubst changeSigma (Supertype.inject kid :: RuleSortChange sn)
    let kids' = Array.zip kidChanges kids <#> \(kidChange /\ kid) -> Down /\ kidChange %.| kid
    Just $ EN label (changeSigma <#> epR) er %. kids'
  _ -> Nothing

makeDefaultUpSteppingRule :: forall sn el. SteppingRule sn el
makeDefaultUpSteppingRule = SteppingRule "defaultUp" case _ of
  EN label _ er %. kids -> do
    let SortingRule rule = getSortingRule label
    leftKidsAndRuleSorts /\ (kidChange /\ kid /\ kidRuleSort) /\ rightKidsAndRuleSorts <- splitAtFindMap findUpBoundary $ Array.zip kids rule.kids
    changeSigma <- unifyChangeWithRuleChange kidChange (Supertype.inject kidRuleSort :: RuleSortChange sn)
    let makeKidBoundary (kid' /\ kidRuleSort') = Down /\ (changeSigma `applyRuleSortVarSubst` (Supertype.inject kidRuleSort' :: RuleSortChange sn)) %.| kid'
        leftKids = leftKidsAndRuleSorts <#> makeKidBoundary
        rightKids = rightKidsAndRuleSorts <#> makeKidBoundary
        kids' = leftKids <> [kid] <> rightKids
    let parentChange = applyRuleSortVarSubst changeSigma (Supertype.inject rule.parent :: RuleSortChange sn)
    Just $ Up /\ parentChange %.| (EN label (changeSigma <#> epL) er %. kids')  
  _ -> Nothing
  where
  findUpBoundary = case _ of
    (Up /\ ch %.| kid) /\ ruleSort -> Just (ch /\ kid /\ ruleSort)
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

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir
