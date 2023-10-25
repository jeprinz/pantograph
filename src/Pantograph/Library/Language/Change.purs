module Pantograph.Library.Language.Change where

import Pantograph.Generic.Language
import Prelude

import Data.Tree.Change (diff)

getDiffChangingRule :: forall el sn. Eq sn => {getSortingRule :: el -> SortingRule sn} -> el -> ChangingRule sn
getDiffChangingRule {getSortingRule} el =
  ChangingRule
    { parameters: sortingRule.parameters
    , kids: sortingRule.kids <#> \kid -> diff kid sortingRule.parent }
  where
  SortingRule sortingRule = getSortingRule el
