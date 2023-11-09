module Pantograph.Library.Step where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Display (display)
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Supertype as Supertype
import Data.Tree (epL, epR)
import Data.Tuple.Nested ((/\))
import Halogen.Elements as El
import Pantograph.Generic.Dynamics (Direction(..), StepExpr(..), SteppingRule(..), (%.), (%.|))
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Language (AnnExprNode(..), RuleSortChange, SortChange, SortingRule(..), applyRuleSortVarSubst, getSortingRule, unifyChangeWithRuleChange, unifySortWithRuleSort)
import Pantograph.Generic.Language.Common (unionRuleSortVarSubst)
import Text.Pretty (parens)
import Util (splitAtFindMap)

makeDefaultDownSteppingRule :: forall sn el. SteppingRule sn el
makeDefaultDownSteppingRule = SteppingRule "defaultDown" case _ of
  Down /\ ch %.| e@(EN label sortSigma er %. kids) -> do
    let sortingRule@(SortingRule rule) = getSortingRule label
    GMB.debugRM (display "[makeDefaultDownSteppingRule] try") {sortingRule: display sortingRule, ch: display ch, e: display e, "rule.parent": display rule.parent}
    maybe' (\_ -> GMB.debugM (display "[makeDefaultDownSteppingRule] failed") >>= \_ -> Nothing) Just do
      sigma <- do
        sigma <- unifyChangeWithRuleChange ch (Supertype.inject rule.parent)
        -- RuleSortVars that are not concretized by this substitution are assigned
        -- to the injected sorts from the original `sortSigma`.
        Just $ unionRuleSortVarSubst const sigma (sortSigma <#> Supertype.inject)
      GMB.debugRM (display "makeDefaultDownSteppingRule") {sigma: display sigma}
      let kidChanges = rule.kids <#> \kid -> applyRuleSortVarSubst sigma (Supertype.inject kid :: RuleSortChange sn)
      let kids' = Array.zip kidChanges kids <#> \(kidChange /\ kid) -> Down /\ kidChange %.| kid
      GMB.debugRM (display "makeDefaultDownSteppingRule") {kids': display kids'}
      Just $ EN label (sigma <#> epR) er %. kids'
  _ -> Nothing

makeDefaultUpSteppingRule :: forall sn el. SteppingRule sn el
makeDefaultUpSteppingRule = SteppingRule "defaultUp" case _ of
  e@(EN label sortSigma er %. kids) -> do
    let sortingRule@(SortingRule rule) = getSortingRule label
    leftKidsAndRuleSorts /\ (kidChange /\ kid /\ kidRuleSort) /\ rightKidsAndRuleSorts <- splitAtFindMap findUpBoundary $ Array.zip kids rule.kids
    GMB.debugRM (display "[makeDefaultUpSteppingRule] try") {sortingRule: display sortingRule, e: display e, kidChange: display kidChange, kidRuleSort: display kidRuleSort, kid: display kid}
    maybe' (\_ -> GMB.debugM (display "[makeDefaultUpSteppingRule] failed") >>= \_ -> Nothing) Just do
      sigma <- do
        sigma <- unifyChangeWithRuleChange kidChange (Supertype.inject kidRuleSort :: RuleSortChange sn)
        GMB.debugRM (display "[makeDefaultUpSteppingRule] successfully unified to get sigma") {sigma: display sigma}
        -- RuleSortVars that are not concretized by this substitution are assigned
        -- to the injected sorts from the original `sortSigma`.
        Just $ unionRuleSortVarSubst const sigma (sortSigma <#> Supertype.inject)
      GMB.debugRM (display "makeDefaultUpSteppingRule") {sigma: display sigma}
      let makeKidBoundary (kid' /\ kidRuleSort') = Down /\ (sigma `applyRuleSortVarSubst` (Supertype.inject kidRuleSort' :: RuleSortChange sn)) %.| kid'
          leftKids = leftKidsAndRuleSorts <#> makeKidBoundary
          rightKids = rightKidsAndRuleSorts <#> makeKidBoundary
          kids' = leftKids <> [kid] <> rightKids
      let parentChange = applyRuleSortVarSubst sigma (Supertype.inject rule.parent :: RuleSortChange sn)
      GMB.debugRM (display "makeDefaultUpSteppingRule") {kids': display kids', parentChange: display parentChange}
      Just $ Up /\ parentChange %.| (EN label (sigma <#> epL) er %. kids')  
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
unless condName cond (SteppingRule name f) = SteppingRule ("unless " <> condName <> " " <> parens name) \e -> if cond e then Nothing else f e

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir
