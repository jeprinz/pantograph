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

-- makeDefaultDownSteppingRule :: forall sn el.
--   {getSortChangeThroughExprPath :: ExprPath sn el -> SortChange sn} ->
--   SteppingRule sn el
-- makeDefaultDownSteppingRule {getSortChangeThroughExprPath} = SteppingRule "defaultDown" case _ of
--   Down /\ ch %.| a@(EN label sortSigma _ %. kids) -> do
--     let kidChanges = tooths (?a % ?a) 
--   _ -> Nothing


-- makeDefaultDownSteppingRule :: forall sn el.
--   {getChangingRule :: el -> ChangingRule sn} ->
--   SteppingRule sn el
-- makeDefaultDownSteppingRule {getChangingRule} = SteppingRule "defaultDown" case _ of
--   Down /\ ch %.| a@(EN label sortSigma _ %. kids) -> do
--     let ChangingRule rule = getChangingRule label
--     let sort = getStepExprSort a
--     let _ = if epL ch == sort then unit else
--             GMB.bugR (El.τ "boundary's change's left endpoint didn't match sort of internal expr")
--               { "epL ch == sort": display $ epL ch == sort
--               , ch: display ch
--               , "epL ch": display $ epL ch
--               , "epR ch": display $ epR ch
--               , sort: display $ sort
--               , "show (epL ch)": display <<< show $ epL ch
--               , "show sort": display <<< show $ sort
--               }
--     changeSigma /\ chBackUp <- doOperation ch rule.parent
--     let changeSigma' = changeSigma <> (injectTreeIntoChange <$> sortSigma)

--     GMB.debugRM (El.τ "makeDefaultDownSteppingRule")
--       { sort: display sort
--       , changeSigma: display changeSigma
--       , chBackUp: display chBackUp
--       , changeSigma': display changeSigma'
--       , rule_kids: display rule.kids
--       }

--     -- TODO: bug is here
--     let kidSorts = applyRuleSortVarSubst changeSigma' <$> rule.kids

--     let kidsWithBoundaries = Array.zipWith (\ch' kid -> Down /\ ch' %.| kid) kidSorts kids

--     -- GMB.debugM (El.τ "defaultDown") 
--     --   { ch: display ch
--     --   , sort: display sort
--     --   , changeSigma: display changeSigma
--     --   , changeSigma': display changeSigma'
--     --   , chBackUp: display chBackUp
--     --   , ruleKidSorts: display $ rule.kids
--     --   , kidSorts: display kidSorts
--     --   , kidsWithBoundaries: display kidsWithBoundaries
--     --   }

--     Just $ Up /\ chBackUp %.| (EN label (epR <$> changeSigma') {} %. kidsWithBoundaries)
--   _ -> Nothing

-- makeDefaultUpSteppingRule :: forall sn el.
--   {getChangingRule :: el -> ChangingRule sn} ->
--   SteppingRule sn el
-- makeDefaultUpSteppingRule {getChangingRule} = SteppingRule "defaultUp" case _ of
--   EN label sortSigma _ %. kids -> do
--     let ChangingRule rule = getChangingRule label
--     leftKidsAndSorts /\ (ch /\ kid /\ ruleCh) /\ rightKidsAndSorts <- splitAtFindMap findUpBoundary $ Array.zip kids rule.kids
--     changeSigma /\ chBackDown <- doOperation ch ruleCh
--     let changeSigma' = changeSigma <> (injectTreeIntoChange <$> sortSigma)
--     let wrapKid (kid' /\ ruleSort') = Down /\ applyRuleSortVarSubst changeSigma' ruleSort' %.| kid'
--     let leftKids = wrapKid <$> leftKidsAndSorts
--     let rightKids = wrapKid <$> rightKidsAndSorts
--     let parentSigma = applyRuleSortVarSubst changeSigma' rule.parent
--     let sortSigma' = epR <$> changeSigma'
--     Just $ Up /\ parentSigma %.| (EN label sortSigma' {} %. (leftKids <> [Down /\ chBackDown %.| kid] <> rightKids))
--   _ -> Nothing
--   where
--   findUpBoundary = case _ of
--     (Up /\ ch %.| kid) /\ sort -> Just (ch /\ kid /\ sort)
--     _ /\ _ -> Nothing

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
