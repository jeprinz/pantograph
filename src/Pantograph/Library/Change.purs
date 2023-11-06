module Pantograph.Library.Change where

import Pantograph.Generic.Language
import Prelude

import Data.Supertype as Supertype
import Data.Tree.Change (diff)

getDiffChangingRule :: forall el sn. Eq sn => {getSortingRule :: el -> SortingRule sn} -> el -> ChangingRule sn
getDiffChangingRule {getSortingRule} el =
  ChangingRule
    { parameters: rule.parameters
    , kids: rule.kids <#> \kid -> diff kid rule.parent
    , parent: Supertype.inject rule.parent }
  where
  SortingRule rule = getSortingRule el
