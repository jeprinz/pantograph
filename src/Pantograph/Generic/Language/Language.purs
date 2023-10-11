module Pantograph.Generic.Language.Language where

import Data.Tree
import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Data.List (List(..))
import Hole (hole)

defaultTopExpr (Language language) =
  language.defaultExpr $ language.topSort

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