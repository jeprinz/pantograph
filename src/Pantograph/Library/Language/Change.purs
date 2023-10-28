module Pantograph.Library.Language.Change where

import Pantograph.Generic.Language
import Prelude

import Data.Tree (injectTreeIntoChange)
import Data.Tree.Change (diff)

getDiffChangingRule :: forall el sn. Eq sn => {getSortingRule :: el -> SortingRule sn} -> el -> ChangingRule sn
getDiffChangingRule {getSortingRule} el =
  ChangingRule
    { parameters: rule.parameters
    , kids: rule.kids <#> \kid -> diff kid rule.parent
    , parent: injectTreeIntoChange rule.parent }
  where
  SortingRule rule = getSortingRule el
