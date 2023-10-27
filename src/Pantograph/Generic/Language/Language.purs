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
import Data.Subtype (inject)
import Data.Traversable (traverse, traverse_)
import Data.Tree.Common (assertValidToothKids)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Record as R
import Record.Builder as RB
import Record.Builder as RecordBuilder
import Text.Pretty (pretty)
import Todo (todo)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Util (class RowKeys, asStateT, buildFromKeys, delete', fromHomogenousRecordToTupleArray, fromJust, fromJust', rowKeys)

-- assert

assertValidRuleVarSubst label sigma@(RuleSortVarSubst m) k =
  let SortingRule rule = getSortingRule label in
  if rule.parameters == Map.keys m then k unit else
  bug $ "assertValidRuleVarSubst: For label " <> show label <> ", the substitution " <> pretty sigma <> " is invalid."

-- build

buildSortingRuleFromStrings :: forall sn. Array String -> (Partial => Array (RuleSort sn) -> Array (RuleSort sn) /\ RuleSort sn) -> SortingRule sn
buildSortingRuleFromStrings strs k = do
  let parametersArray = MakeRuleSortVar <$> strs
  let parameters = Set.fromFoldable parametersArray
  let parametersVars = makeVarRuleSort <$> parametersArray
  let kids /\ parent = unsafePartial $ k parametersVars
  SortingRule {parameters, kids, parent}

buildSortingRule :: forall r sn. RowKeys r => Homogeneous r (RuleSort sn) => Proxy r -> (Record r -> Array (RuleSort sn) /\ RuleSort sn) -> SortingRule sn
buildSortingRule _ k = do
  let parameterNames = rowKeys (Proxy :: Proxy r)
  let parameters = parameterNames # Set.map MakeRuleSortVar
  let kids /\ parent = unsafePartial $ k $ buildFromKeys (makeVarRuleSort <<< MakeRuleSortVar)
  SortingRule {parameters, kids, parent}

buildChangingRule :: forall sn. Array String -> (Partial => Array (RuleSort sn) -> Array (RuleSortChange sn)) -> ChangingRule sn
buildChangingRule strs k = do
  let parametersArray = MakeRuleSortVar <$> strs
  let parameters = Set.fromFoldable parametersArray
  let parametersVars = makeVarRuleSort <$> parametersArray
  let kids = unsafePartial $ k parametersVars
  ChangingRule {parameters, kids}

buildExpr :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => Language sn el => el -> Record r -> Array (Expr sn el) -> Expr sn el
buildExpr label sigma_ = 
  let node = buildExprNode label sigma_ in
  assertValidTreeKids "makeExpr" node \kids ->
    Tree node kids

buildExprNode :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => Language sn el => el -> Record r -> ExprNode sn el
buildExprNode label sigma_ = 
  let sigma = RuleSortVarSubst $ Map.fromFoldable $ map (\(k /\ v) -> (MakeRuleSortVar k /\ v)) $ fromHomogenousRecordToTupleArray sigma_ in
  assertValidRuleVarSubst label sigma \_ ->
    ExprNode label sigma {}

buildStepExpr :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => el -> Record r -> Array (StepExpr sn el) -> StepExpr sn el
buildStepExpr label sigma_ = 
  let node = buildExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr (Nothing /\ node) kids

-- make

makeInjectRuleSortNode n = InjectRuleSortNode (SortNode n)

makeInjectRuleSort n kids = Tree (makeInjectRuleSortNode n) kids

makeExprNode label sigma_ = 
  let sigma = RuleSortVarSubst (Map.fromFoldable (sigma_ <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort))) in
  assertValidRuleVarSubst label sigma \_ ->
    ExprNode label sigma {}

makeExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeExpr" node \kids ->
    Tree node kids

makeExprTooth label sigma_ i = 
  let node = makeExprNode label sigma_ in
  assertValidToothKids "makeExprTooth" node i \kids ->
    Tooth node (i /\ kids)

makeStepExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr (Nothing /\ node) kids

makeExprNonEmptyPath ths = NonEmptyPath $ fromJust' "makeExprNonEmptyPath" $ NonEmptyList.fromFoldable ths

-- get

defaultTopExpr =
  getDefaultExpr topSort

getExprNodeSort (ExprNode label sigma _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprSort :: forall sn el. Language sn el => Expr sn el -> Sort sn
getExprSort (Tree el _) = getExprNodeSort el

getExprToothInnerSort (Tooth (ExprNode label sigma _) (i /\ _)) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma $ fromJust $ Array.index sortingRule.kids i

getExprToothOuterSort (Tooth (ExprNode label sigma _) _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprNonEmptyPathInnerSort = unconsNonEmptyPath >>> _.inner >>> getExprToothInnerSort
getExprNonEmptyPathOuterSort = unsnocNonEmptyPath >>> _.outer >>> getExprToothOuterSort

-- | The SortChange that corresponds to going from the inner sort of the path to
-- | the outer sort of the path.
getExprNonEmptyPathSortChange :: forall sn el. Language sn el => ExprNonEmptyPath sn el -> SortChange sn
getExprNonEmptyPathSortChange = unconsNonEmptyPath >>> case _ of
  {outer: Nothing, inner} -> getExprToothSortChange inner
  {outer: Just outer, inner} -> getExprNonEmptyPathSortChange outer <> getExprToothSortChange inner

-- | The SortChange that corresponds to going from the inner sort of the tooth
-- | to the outer sort of the path.
getExprToothSortChange :: forall sn el. Language sn el => ExprTooth sn el -> SortChange sn
getExprToothSortChange (Tooth (ExprNode label sigma _) (i /\ _)) =
  let ChangingRule rule = getChangingRule label in
  applyRuleSortVarSubst sigma $ fromJust $ Array.index rule.kids i

-- | The input sort is the bottom sort. The output change goes from the bottom
-- | to the top.
getExprPathChange :: forall sn el. Language sn el =>
  ExprPath sn el ->
  Sort sn ->
  SortChange sn
getExprPathChange path bottomSort = case unconsPath path of
  Nothing -> inject bottomSort
  Just {outer, inner: Tooth (ExprNode label sigma _) (i /\ _)} ->
    let ChangingRule rule = getChangingRule label in
    let ruleChange = fromJust $ Array.index rule.kids i in
    let change = applyRuleSortVarSubst sigma ruleChange in
    let restOfPathChange = getExprPathChange outer (endpoints change).right in
    change <> restOfPathChange

-- misc

-- | `p1 == p2`, as path skeletons
eqExprPathSkeleton :: forall sn el. Eq sn => Eq el => ExprPath sn el -> ExprPath sn el -> Boolean
eqExprPathSkeleton p1 p2 = case unconsPath p1 /\ unconsPath p2 of
  Nothing /\ Nothing -> true
  Just {outer: p1', inner: Tooth node1 (i1 /\ _)} /\ Just {outer: p2', inner: Tooth node2 (i2 /\ _)} | node1 == node2 && i1 == i2 -> eqExprPathSkeleton p1' p2'
  _ -> false

-- | `p1` is a prefix of `p2``, as path skeletons
prefixExprPathSkeleton :: forall sn el. Eq sn => Eq el => ExprPath sn el -> ExprPath sn el -> Boolean
prefixExprPathSkeleton p1 p2 = case unconsPath p1 /\ unconsPath p2 of
  Nothing /\ _ -> true
  Just {outer: p1', inner: Tooth node1 (i1 /\ _)} /\ Just {outer: p2', inner: Tooth node2 (i2 /\ _)} | node1 == node2 && i1 == i2 -> prefixExprPathSkeleton p1' p2'
  _ -> false
