module Pantograph.Library.Change where

import Prelude

import Data.Display (Html, display)
import Data.Tree.Change (diff)
import Halogen.Elements as El
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Language (class Language, ChangingRule(..), SortingRule(..), getSortingRule)
import Text.Pretty (pretty)

getDiffChangingRule :: forall el sn. Language sn el => el -> ChangingRule sn
getDiffChangingRule el =
  ChangingRule
    { parameters: rule.parameters
    , kids: rule.kids <#> \kid -> diff kid rule.parent
    -- , kids: rule.kids <#> \kid ->
    --     let ch = diff kid rule.parent in
    --     GMB.debugR (El.text "getDiffChangingRule" :: Html) {el: El.τ $ show el, kid: display kid, ch: display ch} \_ -> 
    --     ch 
  }
  where
  SortingRule rule = getSortingRule el
