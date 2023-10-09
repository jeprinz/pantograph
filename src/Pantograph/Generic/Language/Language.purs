module Pantograph.Generic.Language.Language where

import Pantograph.Generic.Language.Common
import Prelude
import Data.Tree

defaultTopExpr (Language language) =
  language.defaultExpr $ language.topSort

getExprNodeSort (Language language) (AnnExprNode {label, sigma}) =
  let SortingRule sortingRule = language.getSortingRule label in
  applyRuleSortVarSubst sigma (sortingRule.parent)

getExprSort language (Tree expr) =
  getExprNodeSort language expr.node
