module Pantograph.Generic.Language.Language where

import Data.Tree
import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Subtype (inject)
import Data.Traversable (traverse)
import Data.Tree.Common (assertValidToothKids, injectTreeIntoChange)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafePartial)
import Text.Pretty (pretty)
import Todo (todo)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Util (class RowKeys, buildFromKeys, fromHomogenousRecordToTupleArray, fromJust, fromJust', rowKeys)

-- assert

assertValidRuleVarSubst :: forall a sn el. Language sn el => el -> RuleSortVarSubst (Sort sn) -> (Unit -> a) -> a
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

buildChangingRule :: forall sn. Array String -> (Partial => Array (RuleSort sn) -> Array (RuleSortChange sn) /\ RuleSortChange sn) -> ChangingRule sn
buildChangingRule strs k = do
  let parametersArray = MakeRuleSortVar <$> strs
  let parameters = Set.fromFoldable parametersArray
  let parametersVars = makeVarRuleSort <$> parametersArray
  let kids /\ parent = unsafePartial $ k parametersVars
  ChangingRule {parameters, kids, parent}

buildExpr :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => Language sn el => el -> Record r -> Array (Expr sn el) -> Expr sn el
buildExpr label sigma_ = 
  let node = buildExprNode label sigma_ in
  assertValidTreeKids "makeExpr" node \kids ->
    Tree node kids

buildExprNode :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => Language sn el => el -> Record r -> ExprNode sn el
buildExprNode label sigma_ = 
  let sigma = RuleSortVarSubst $ Map.fromFoldable $ map (\(k /\ v) -> (MakeRuleSortVar k /\ v)) $ fromHomogenousRecordToTupleArray sigma_ in
  assertValidRuleVarSubst label sigma \_ ->
    EN label sigma {}

buildStepExpr :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => el -> Record r -> Array (StepExpr sn el) -> StepExpr sn el
buildStepExpr label sigma_ = 
  let node = buildExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr node kids

-- make

makeInjectRuleSortNode :: forall sn. sn -> RuleSortNode sn
makeInjectRuleSortNode n = InjectRuleSortNode (SN n)

makeInjectRuleSort :: forall sn. sn -> Array (Tree (RuleSortNode sn)) -> Tree (RuleSortNode sn)
makeInjectRuleSort n kids = Tree (makeInjectRuleSortNode n) kids

makeExprNode :: forall sn el. Language sn el => el -> Array (String /\ (Tree (SortNode sn))) -> ExprNode sn el
makeExprNode label sigma_ = 
  let sigma = RuleSortVarSubst (Map.fromFoldable (sigma_ <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort))) in
  assertValidRuleVarSubst label sigma \_ ->
    EN label sigma {}

makeExpr :: forall sn el. Language sn el => el -> Array (String /\ (Tree (SortNode sn))) -> Array (Expr sn el) -> Expr sn el
makeExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeExpr" node \kids ->
    Tree node kids

makeExprTooth :: forall sn el. Language sn el => el -> Array (String /\ (Tree (SortNode sn))) -> Int -> Array (Expr sn el) -> ExprTooth sn el
makeExprTooth label sigma_ i = 
  let node = makeExprNode label sigma_ in
  assertValidToothKids "makeExprTooth" node i \kids ->
    Tooth node (i /\ kids)

makeStepExpr :: forall sn el. Language sn el => el -> Array (String /\ (Tree (SortNode sn))) -> Array (StepExpr sn el) -> StepExpr sn el
makeStepExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr node kids

makeExprNonEmptyPath :: forall sn el. Array (ExprTooth sn el) -> ExprNonEmptyPath sn el
makeExprNonEmptyPath ths = NonEmptyPath $ fromJust' "makeExprNonEmptyPath" $ NonEmptyList.fromFoldable ths

-- get

defaultTopExpr :: forall sn el. Language sn el => Maybe (Expr sn el)
defaultTopExpr =
  getDefaultExpr topSort

getExprNodeSort :: forall sn el er. Language sn el => ApplyRuleSortVarSubst (Sort sn) (RuleSort sn) (Sort sn) => AnnExprNode sn el er -> Sort sn
getExprNodeSort (EN label sigma _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprSort :: forall sn el. Language sn el => Expr sn el -> Sort sn
getExprSort (Tree el _) = getExprNodeSort el

getExprToothInnerSort :: forall sn el er. Language sn el => ApplyRuleSortVarSubst (Sort sn) (Tree (RuleSortNode sn)) (Sort sn) => Tooth (AnnExprNode sn el er) -> (Sort sn)
getExprToothInnerSort (Tooth (EN label sigma _) (i /\ _)) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma $ fromJust $ Array.index sortingRule.kids i

getExprToothOuterSort :: forall sn el er. Language sn el => ApplyRuleSortVarSubst (Sort sn) (Tree (RuleSortNode sn)) (Sort sn) => Tooth (AnnExprNode sn el er) -> (Sort sn)
getExprToothOuterSort (Tooth (EN label sigma _) _) =
  let SortingRule sortingRule = getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprNonEmptyPathInnerSort :: forall sn el er. Language sn el => NonEmptyPath (AnnExprNode sn el er) -> Sort sn
getExprNonEmptyPathInnerSort = unconsNonEmptyPath >>> _.inner >>> getExprToothInnerSort

getExprNonEmptyPathOuterSort :: forall sn el er. Language sn el => NonEmptyPath (AnnExprNode sn el er) -> Sort sn
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
getExprToothSortChange (Tooth (EN label sigma _) (i /\ _)) =
  let ChangingRule rule = getChangingRule label in
  applyRuleSortVarSubst sigma $ fromJust $ Array.index rule.kids i

-- | The input sort is the bottom sort. The output change goes from the bottom
-- | to the top.
getExprPathChange :: forall sn el. Language sn el =>
  ExprPath sn el ->
  Sort sn ->
  SortChange sn
getExprPathChange path bottomSort = case unconsPath path of
  Nothing -> injectTreeIntoChange bottomSort
  Just {outer, inner: Tooth (EN label sigma _) (i /\ _)} ->
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

matchRuleSortChangeWithSortChange :: forall sn. 
  Eq sn => Show sn => PrettyTreeNode sn =>
  RuleSortChange sn -> SortChange sn ->
  Maybe (RuleSortVarSubst (SortChange sn))
matchRuleSortChangeWithSortChange (Shift (sh /\ th) ch) (Shift (sh' /\ th') ch') | sh == sh', th == inject th' = matchRuleSortChangeWithSortChange ch ch'
matchRuleSortChangeWithSortChange (Replace t1 t2) (Replace t1' t2') | t1 == inject t1', t2 == inject t2' = Just emptyRuleSortVarSubst
matchRuleSortChangeWithSortChange (InjectChange (InjectRuleSortNode s) kids) (InjectChange s' kids') | s == s' = Array.fold <$> uncurry matchRuleSortChangeWithSortChange `traverse` Array.zip kids kids'
matchRuleSortChangeWithSortChange (InjectChange (VarRuleSortNode x) []) ch' = Just $ singletonRuleSortVarSubst x ch'
matchRuleSortChangeWithSortChange (InjectChange (VarRuleSortNode _) _) _ = bug $ "invalid"
matchRuleSortChangeWithSortChange _ _ = Nothing

-- | I don't have a good name for this operation, but what it does is: input `c1
-- | : Change` and `c2 : RuleChange`, and output `sigma : RuleSortVarSubst` and
-- | `c3 : Change`, such that:
-- | ```
-- |   c1 ∘ c3 = sigma c2
-- | ```
-- | Also, `c3` should be _orthogonal_ to `c1`. If this doesn't exist, it
-- | outputs `Nothing`. (Note that `c2` has metavariables in the change
-- | positions, so its `(Expr (Meta (ChangeLabel l))))`
doOperation :: forall sn.
  Eq sn => Show sn => PrettyTreeNode sn =>
  SortChange sn -> RuleSortChange sn -> 
  Maybe (RuleSortVarSubst (SortChange sn) /\ SortChange sn)
doOperation c1 c2 = do
  sigma <- matchRuleSortChangeWithSortChange c2 c1
  let c2' = applyRuleSortVarSubst sigma c2
  Just $ sigma /\ (invert c1 <> c2')
