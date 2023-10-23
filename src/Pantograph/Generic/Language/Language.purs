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
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Util (asStateT, delete', fromJust, fromJust')

-- utilities

makeConstRuleSortNode n = ConstRuleSortNode (SortNode n)
makeConstRuleSort n kids = Tree (makeConstRuleSortNode n) kids
makeVarRuleSort x = Tree (VarRuleSortNode x) []
makeSort sn kids = Tree (SortNode sn) kids
makeExpr label sigma kids = Tree (AnnExprNode {label, sigma: RuleSortVarSubst (Map.fromFoldable (sigma <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort)))}) kids
makeExprTooth label sigma i kids = Tooth (AnnExprNode {label, sigma: RuleSortVarSubst (Map.fromFoldable (sigma <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort)))}) i kids
makeStepExpr label sigma kids = StepExpr Nothing (AnnExprNode {label, sigma}) kids
makeNonEmptyExprPath ths = NonEmptyPath $ fromJust' "makeNonEmptyExprPath" $ NonEmptyList.fromFoldable ths

defaultTopExpr (Language language) =
  language.getDefaultExpr $ language.topSort

getExprNodeSort (Language language) (AnnExprNode {label, sigma}) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprSort language (Tree el _) =
  getExprNodeSort language el

getToothInteriorSort (Language language) (Tooth (AnnExprNode {label, sigma}) _ _) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getPathInteriorSort language (Path (Cons tooth _)) = getToothInteriorSort language tooth
getPathInteriorSort _ (Path Nil) = bug $ "getPathInteriorSort of empty Path"

-- the input sort is the bottom sort
-- The output change goes from the bottom to the top
getPathChange :: forall sn el. Eq sn => Show sn =>
  Language sn el ->
  ExprPath sn el ->
  Sort sn ->
  SortChange sn
getPathChange (Language language) path bottomSort = case unconsPath path of
  Nothing -> injectChange bottomSort
  Just {outer, inner: Tooth (AnnExprNode {label, sigma}) i _} ->
    let ChangingRule rule = language.getChangingRule label in
    let ruleChange = fromJust $ Array.index rule.kids i in
    let change = applyRuleSortVarSubst sigma ruleChange in
    let restOfPathChange = getPathChange (Language language) outer (endpoints change).right in
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


