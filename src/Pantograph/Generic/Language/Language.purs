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
import Hole (hole)
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Util (asStateT, delete', fromJust')

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

-- match

-- type ExprMatches sn el = 
--   { exprs :: Array (String /\ Expr sn el)
--   , sorts :: Array (String /\ Sort sn) }

-- matchExpr :: forall sn el. Eq sn => Show sn => PrettyTreeNode sn => Eq el => PrettyTreeNode el =>
--   MatchingSyntax sn el -> 
--   String -> Expr sn el ->
--   Maybe (ExprMatches sn el)
-- matchExpr (MatchingSyntax syntax) = Match.match {emptyMatches, matchConstr}
--   where
--   emptyMatches = {exprs: [], sorts: []}

--   matchConstr expr (Match.Var x) = modify_ $ R.modify (Proxy :: Proxy "exprs") $ Array.cons (x /\ expr)
--   matchConstr expr@(Tree {node: AnnExprNode {label, sigma}, kids}) matchTree@(Match.Constr {constr, args}) = do
--     let label' = fromJust' ("invalid expr label; matchTree = " <> pretty matchTree) $  syntax.parseExprLabel constr
--     when (label /= label') $ throwError unit
--     case Array.uncons args of
--       Nothing -> bug $ "matchConstr: `args` must have at least one element, for `sigma`"
--       Just {head: sigmaTree, tail: kidTrees} -> do
--         asStateT (\{exprs} sorts -> {exprs, sorts}) (\{sorts} -> sorts) $
--           matchConstrRuleSortVarSubst (MatchingSyntax syntax) sigma sigmaTree
--         when (Array.length kids /= Array.length kidTrees) $ bug $ "matchExpr.matchConstr: parsed constr " <> show constr <> " should have " <> show (Array.length kids) <> " kids, but instead it has " <> show (Array.length kidTrees) <> "; expr = " <> pretty expr <> "; matchTree = " <> pretty matchTree
--         uncurry matchConstr `traverse_` Array.zip kids kidTrees
--   matchConstr _ Match.Wild = pure unit

-- type SortMatches sn = Array (String /\ Sort sn)

-- matchSort :: forall sn el. Eq sn => PrettyTreeNode sn =>
--   MatchingSyntax sn el -> 
--   String -> Sort sn ->
--   Maybe (SortMatches sn)
-- matchSort syntax = Match.match {emptyMatches: [], matchConstr: matchConstrSort syntax}

-- matchConstrSort :: forall sn el. Eq sn => PrettyTreeNode sn =>
--   MatchingSyntax sn el ->
--   Sort sn -> MatchTree -> StateT (SortMatches sn) Maybe Unit
-- matchConstrSort _ sort (Match.Var x) = modify_ $ Array.cons (x /\ sort)
-- matchConstrSort (MatchingSyntax syntax) sort@(Tree {node: SortNode node, kids}) matchTree@(Match.Constr {constr, args}) = do
--   let node' = fromJust' ("invalid sort node; matchTree = " <> pretty matchTree) $ syntax.parseSortNode constr
--   when (node /= node') $ throwError unit
--   when (Array.length kids /= Array.length args) $ bug $ "matchConstrSort: parsed constr " <> show constr <> " should have " <> show (Array.length kids) <> " kids, but instead it has " <> show (Array.length args) <> "; sort = " <> pretty sort <> "; matchTree = " <> pretty matchTree
--   uncurry (matchConstrSort (MatchingSyntax syntax)) `traverse_` Array.zip kids args
-- matchConstrSort _ _ Match.Wild = pure unit

-- -- NOTE: when matching, order shouldn't matter, since RuleSortVarSubst uses a Map
-- matchConstrRuleSortVarSubst :: forall sn el. Eq sn => Show sn => PrettyTreeNode sn => MatchingSyntax sn el -> RuleSortVarSubst sn -> MatchTree -> StateT (Array (String /\ Sort sn)) Maybe Unit
-- matchConstrRuleSortVarSubst _ sigma matchTree@(Match.Var _) = bug $ "can't match a RuleSortVarSubst with a match variable; sigma = " <> show sigma <> "; matchTree = " <> show matchTree
-- matchConstrRuleSortVarSubst (MatchingSyntax syntax) sigma@(RuleSortVarSubst m) matchTree@(Match.Constr {constr, args}) = do
--   when (constr /= "map") $ bug $ "matchConstrRuleSortVarSubst: expected constr " <> show "map" <> "; sigmma = " <> show sigma <> "; matchTree = " <> pretty matchTree
--   -- each arg is the RuleSortVar's label as its constr and the bound Sort as its single arg
--   -- e.g. matchTree = (map (x $x) (y $y))
--   m' <- Array.foldM 
--     (\m' -> case _ of
--       Match.Constr {constr: x, args: [sortTree]} -> case Map.lookup (MakeRuleSortVar x) m of
--         Nothing -> bug $ "matchConstrRuleSortVarSubst: expected rule sort var " <> show x <> " to appear in RuleSortVarSubst " <> show sigma
--         Just sort -> do
--           matchConstrSort (MatchingSyntax syntax) sort sortTree
--           pure $ delete' (MakeRuleSortVar x) m'
--       _ -> bug "TODO"
--     ) 
--     m args
--   when (not (Map.isEmpty m')) $ bug $ "matchConstrRuleSortVarSubst: expected to match all bound RuleSortVars, but there are still some left after matching: " <> show m'
-- matchConstrRuleSortVarSubst _ _ Match.Wild = pure unit