module Language.Pantograph.Lib.GreyedRules where

import Language.Pantograph.Generic.Grammar as Grammar
import Data.Expr as Expr
import Language.Pantograph.Generic.Smallstep as SmallStep
import Data.Array as Array
import Data.Set as Set
import Util as Util
import Data.TotalMap as TotalMap

createGreyedDownRules :: forall l r. Grammar.IsRuleLabel l r =>
    Grammar.Rule l
    -> Int -- what'th child will be effectively the value of this rule
    -> r -- label for the regular construct
    -> r -- what rule label to use for the greyed construct
    -> Grammar.LanguageChanges l r
    -> Array (SmallStep.StepRule l r)
createGreyedDownRules rule index regularRuleLabel greyRuleLabel languageChanges =
        let Grammar.ChangeRule vars crustyKidChanges = TotalMap.lookup regularRuleLabel languageChanges in
        let crustyKidChange = Util.fromJust' "cgdr1" (Array.index crustyKidChanges index) in
        [
        -- replace greyed with regular rule
        -- insert regular rule
        -- if sub = unify c crustyKidChange down{t}_{c} ~~> (rule sub) % down{t}_{sub kidSort at index in rule}
        -- delete regular rule / replace with greyed if any other children are non-default derivations
        ]

createGreyedConstruct :: forall l r.
    Grammar.Rule l -> Int -> Grammar.Rule l
createGreyedConstruct (Grammar.Rule vars children conclusion) index =
    let x = Expr.freshMetaVar "anything" in
    let xSort = Expr.fromMetaVar x in
    Grammar.Rule
        (Set.insert x vars) -- technically, this maybe should remove any vars that are now unused...
        (Util.fromJust' "cgr" (Array.updateAt index xSort children))
        xSort
