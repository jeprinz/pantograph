module Pantograph.Generic.Language.Language where

import Prelude
import Pantograph.Generic.Language.Common

defaultTopExpr (Language language) =
  language.defaultExpr $ language.topSort

getExprNodeSort (Language language) (ExprNode {label, sigma, dat}) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma (sortingRule.parent)

getExprSort language (Tree tree) =
  getExprNodeSort language tree.node
