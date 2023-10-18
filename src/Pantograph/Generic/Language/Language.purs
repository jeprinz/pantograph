module Pantograph.Generic.Language.Language where

import Data.Tree
import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Data.List (List(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Hole (hole)
import Partial.Unsafe (unsafePartial)
import Util (fromJust')

-- utilities

makeConstRuleSortNode n = ConstRuleSortNode (SortNode n)
makeConstRuleSort n kids = Tree {node: makeConstRuleSortNode n, kids}
makeVarRuleSort x = Tree {node: VarRuleSortNode x, kids: []}
makeSort sn kids = Tree {node: SortNode sn, kids}
makeExpr label sigma kids = Tree {node: AnnExprNode {label, sigma: RuleSortVarSubst (Map.fromFoldable (sigma <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort)))}, kids}
makeExprTooth label sigma i kids = Tooth {node: AnnExprNode {label, sigma: RuleSortVarSubst (Map.fromFoldable (sigma <#> \(str /\ sort) -> (MakeRuleSortVar str /\ sort)))}, i, kids}
makeStepExpr label sigma kids = InjectStepExpr {node: AnnExprNode {label, sigma}, kids, maybeMarker: Nothing}
makeNonEmptyExprPath ths = NonEmptyPath $ fromJust' "makeNonEmptyExprPath" $ NonEmptyList.fromFoldable ths

defaultTopExpr (Language language) =
  language.getDefaultExpr $ language.topSort

getExprNodeSort (Language language) (AnnExprNode {label, sigma}) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getExprSort language (Tree expr) =
  getExprNodeSort language expr.node

getToothInteriorSort (Language language) (Tooth {node: AnnExprNode {label, sigma}}) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma sortingRule.parent

getPathInteriorSort language (Path (Cons tooth _)) = getToothInteriorSort language tooth
getPathInteriorSort _ (Path Nil) = bug $ "getPathInteriorSort of empty Path"

-- builders

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
