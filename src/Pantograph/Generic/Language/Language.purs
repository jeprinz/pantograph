module Pantograph.Generic.Language.Language where

import Data.Tree
import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT(..), execState, get, gets, mapStateT, modify_)
import Data.Array as Array
import Data.List (List(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Match (MatchTree)
import Data.Match as Match
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Hole (hole)
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Util (asStateT, fromJust')

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

-- match

matchExpr :: forall r sn el. Eq el => PrettyTreeNode el =>
  Language sn el -> 
  String -> Tree (AnnExprNode sn el r) ->
  Maybe
    { exprs :: Array (String /\ Tree (AnnExprNode sn el r))
    , sorts :: Array (String /\ Tree (SortNode sn)) }
matchExpr (Language language) = Match.match {emptyMatches, matchConstr}
  where
  emptyMatches = {exprs: [], sorts: []}

  matchConstr expr (Match.Var x) = modify_ $ R.modify (Proxy :: Proxy "exprs") $ Array.cons (x /\ expr)
  matchConstr expr@(Tree {node: AnnExprNode {label, sigma}, kids}) matchTree@(Match.Construction {constr, args}) = do
    let label' = language.parseExprLabel constr
    when (label /= label') $ throwError unit
    case Array.uncons args of
      Nothing -> bug $ "matchConstr: `args` must have at least one element, for `sigma`"
      Just {head: sigmaTree, tail: kidTrees} -> do
        asStateT (\{exprs} sorts -> {exprs, sorts}) (\{sorts} -> sorts) $
          matchConstrRuleSortVarSubst (Language language) sigma sigmaTree
        when (Array.length kids /= Array.length kidTrees) $ bug $ "parse constr " <> show constr <> " should have " <> show (Array.length kids) <> " kids, but instead it has " <> show (Array.length kidTrees) <> "; expr = " <> pretty expr <> "; matchTree = " <> pretty matchTree
        uncurry matchConstr `traverse_` Array.zip kids kidTrees

-- NOTE: when matching, order shouldn't matter, since RuleSortVarSubst uses a Map
matchConstrRuleSortVarSubst :: forall sn el. Language sn el -> RuleSortVarSubst sn -> MatchTree -> StateT (Array (String /\ Sort sn)) Maybe Unit
matchConstrRuleSortVarSubst = hole "TODO"

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
