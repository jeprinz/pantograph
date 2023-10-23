module Pantograph.Generic.Language.Language where

import Data.Tree
import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT(..), State, execState, get, gets, mapStateT, modify_)
import Data.Array as Array
import Data.List (List(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tree.Common (assertValidToothKids)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Util (asStateT, delete', fromJust, fromJust')

assertValidRuleVarSubst label sigma@(RuleSortVarSubst m) k =
  let SortingRule rule = getSortingRule label in
  if rule.parameters == Map.keys m then k unit else
  bug $ "assertValidRuleVarSubst: For label " <> show label <> ", the substitution " <> pretty sigma <> " is invalid."

-- utilities

makeConstRuleSortNode n = ConstRuleSortNode (SortNode n)

makeConstRuleSort n kids = Tree (makeConstRuleSortNode n) kids

makeVarRuleSort x = Tree (VarRuleSortNode x) []

makeSort sn kids = Tree (SortNode sn) kids

makeExprNode label sigma_ = 
  let sigma = RuleSortVarSubst (Map.fromFoldable (sigma_ <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort))) in
  assertValidRuleVarSubst label sigma \_ ->
    ExprNode {label, sigma}

makeExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeExpr" node \kids ->
    Tree node kids

makeExprTooth label sigma_ i = 
  let node = makeExprNode label sigma_ in
  assertValidToothKids "makeExprTooth" node i \kids ->
    Tooth node i kids

makeStepExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr Nothing node kids

makeNonEmptyExprPath ths = NonEmptyPath $ fromJust' "makeNonEmptyExprPath" $ NonEmptyList.fromFoldable ths

defaultTopExpr =
  getDefaultExpr topSort

getExprNodeSort (ExprNode {label, sigma}) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprSort (Tree el _) =
  getExprNodeSort el

getToothInnerSort (Tooth (ExprNode {label, sigma}) i _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma $ fromJust $ Array.index sortingRule.kids i

getToothOuterSort (Tooth (ExprNode {label, sigma}) _ _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getNonEmptyPathInnerSort = unconsNonEmptyPath >>> _.inner >>> getToothInnerSort
getNonEmptyPathOuterSort = unsnocNonEmptyPath >>> _.outer >>> getToothOuterSort

-- the input sort is the bottom sort
-- The output change goes from the bottom to the top
getPathChange :: forall sn el. Language sn el =>
  ExprPath sn el ->
  Sort sn ->
  SortChange sn
getPathChange path bottomSort = case unconsPath path of
  Nothing -> injectChange bottomSort
  Just {outer, inner: Tooth (ExprNode {label, sigma}) i _} ->
    let ChangingRule rule = getChangingRule label in
    let ruleChange = fromJust $ Array.index rule.kids i in
    let change = applyRuleSortVarSubst sigma ruleChange in
    let restOfPathChange = getPathChange outer (endpoints change).right in
    change <> restOfPathChange

-- build

buildSortingRule :: forall sn. Array String -> (Partial => Array (RuleSort sn) -> {kids :: Array (RuleSort sn), parent :: RuleSort sn}) -> SortingRule sn
buildSortingRule strs k = do
  let parametersArray = MakeRuleSortVar <$> strs
  let parameters = Set.fromFoldable parametersArray
  let parametersVars = makeVarRuleSort <$> parametersArray
  let {kids, parent} = unsafePartial $ k parametersVars
  SortingRule {parameters, kids, parent}

buildChangingRule :: forall sn. Array String -> (Partial => Array (RuleSort sn) -> Array (RuleSortChange sn)) -> ChangingRule sn
buildChangingRule strs k = do
  let parametersArray = MakeRuleSortVar <$> strs
  let parameters = Set.fromFoldable parametersArray
  let parametersVars = makeVarRuleSort <$> parametersArray
  let kids = unsafePartial $ k parametersVars
  ChangingRule {parameters, kids}


