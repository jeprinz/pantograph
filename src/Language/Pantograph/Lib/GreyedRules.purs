module Language.Pantograph.Lib.GreyedRules where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List(..), (:))
import Language.Pantograph.Generic.Grammar as Grammar
import Data.Expr as Expr
import Data.Expr ((%))
import Language.Pantograph.Generic.Smallstep as SmallStep
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Util as Util
import Data.TotalMap as TotalMap
import Data.Either (Either(..))
import Language.Pantograph.Generic.Unification as Unification
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Data.List.Zip as ZipList
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Foldable (foldl)
import Hole (hole)

createGreyedDownRules :: forall l r. Grammar.IsRuleLabel l r =>
    Int -- what'th child will be effectively the value of this rule
    -> r -- label for the regular construct
    -> r -- what rule label to use for the greyed construct
    -> Grammar.LanguageChanges l r
    -> Array (SmallStep.StepRule l r)
createGreyedDownRules index regularRuleLabel greyRuleLabel languageChanges =
        let (Grammar.Rule _vars _children _conclusion) = TotalMap.lookup regularRuleLabel Grammar.language in
        let Grammar.ChangeRule vars crustyKidChanges = TotalMap.lookup regularRuleLabel languageChanges in
        let crustyKidChange = Util.fromJust' "cgdr1" (Array.index crustyKidChanges index) in
        [
        -- replace greyed with regular rule
            --
        -- insert regular rule
            -- if sub = unify c crustyKidChange down{t}_{c} ~~> (rule sub) % down{t}_{sub kidSort at index in rule}
        -- delete regular rule / replace with greyed if any other children are non-default derivations
            --
        ]

createGreyedConstruct :: forall l r.
    Grammar.Rule l -> Int -> Grammar.Rule l
createGreyedConstruct (Grammar.Rule vars children conclusion) index =
    let x = Expr.RuleMetaVar "anything" in
    let xSort = Expr.fromMetaVar x in
    Grammar.Rule
        (Set.insert x vars) -- technically, this maybe should remove any vars that are now unused...
        (Util.fromJust' "cgr" (Array.updateAt index xSort children))
        xSort
------------- Changes ------------------

type ChSub l = Expr.MetaVarSub (Expr.MetaChange l)

chSubPruneToSub :: forall l. Expr.IsExprLabel l => ChSub l -> Unification.Sub l
chSubPruneToSub sub = Map.mapMaybe (ChangeAlgebra.isIdMaybe) sub

subSomeChangeLabel :: forall l. Expr.IsExprLabel l => ChSub l -> Expr.ChangeLabel (Expr.Meta l) -> Expr.ChangeLabel (Expr.Meta l)
subSomeChangeLabel sub =
  let subExpr = Expr.subMetaExprPartially (chSubPruneToSub sub) in
  case _ of
      Expr.Plus (Expr.Tooth dir (ZipList.Path {left, right})) -> Expr.Plus (Expr.Tooth dir (ZipList.Path {left: map subExpr left, right: map subExpr right}))
      Expr.Minus (Expr.Tooth dir (ZipList.Path {left, right})) -> Expr.Minus (Expr.Tooth dir (ZipList.Path {left: map subExpr left, right: map subExpr right}))
      Expr.Inject l -> Expr.Inject l -- NOTE: if l was a metavar, we wouldn't get here because subSomeMetaChange would have dealt with it.
      Expr.Replace e1 e2 -> Expr.Replace (subExpr e1) (subExpr e2)

subSomeMetaChange :: forall l. Expr.IsExprLabel l => ChSub l -> Expr.MetaChange l -> Expr.MetaChange l
subSomeMetaChange sub (Expr.Expr l kids) =
    case l of
        Expr.Inject (Expr.MV x) | Just s <- Map.lookup x sub
            -> s
        _ -> Expr.Expr (subSomeChangeLabel sub l) (map (subSomeMetaChange sub) kids)

-- sub2 after sub1
composeChSub :: forall l. Expr.IsExprLabel l => ChSub l -> ChSub l -> ChSub l
composeChSub sub1 sub2 = Util.union' (map (subSomeMetaChange sub2) sub1) sub2

addToChSub :: forall l. Expr.IsExprLabel l => ChSub l -> Expr.MetaVar -> Expr.MetaChange l -> Maybe (ChSub l)
addToChSub sub x c =
    case Map.lookup x sub of
        Nothing -> pure $ Map.insert x (subSomeMetaChange sub c) (map (subSomeMetaChange (Map.singleton x c)) sub)
        Just c0 -> hole "finish implementing unification"-- ?h

-- any ordering, will unify outputs
--mergeChSubs :: forall l. Expr.IsExprLabel l => ChSub l -> ChSub l -> Maybe (ChSub l)
--mergeChSubs sub1 sub2 =
--    foldl (\acc (k /\ v) -> addToChSub <$> acc <*> k <*> v) (Just sub1) (Map.toUnfoldable sub2 :: List _)

--unifyChanges :: forall l. Expr.IsExprLabel l =>
--    Expr.MetaChange l -> Expr.MetaChange l
--    -> Maybe (ChSub l)
--unifyChanges (Expr.Inject (Expr.Meta (Left x)) % []) c = Just (Map.singleton x c)
--unifyChanges c (Expr.Inject (Expr.Meta (Left x)) % []) = Just (Map.singleton x c)
--unifyChanges (Expr.Inject (Expr.Meta (Right l1)) % kids1) (Expr.Inject (Expr.Meta (Right l2)) % kids2) | l1 == l2 =
--    let a = (foldl (composeC) Map.empty) <$> sequence (Array.zipWith unifyChanges kids1 kids2) in
----    let a = foldl ?h ?h <$> sequence (Array.zipWith unifyChanges kids1 kids2) in
--    ?h
--unifyChanges (Expr.Plus th1 % [kid1]) ((Expr.Plus th2) % [kid2]) = ?h
--unifyChanges (Expr.Minus th1 % [kid1]) ((Expr.Minus th2) % [kid2]) = ?h
--unifyChanges (Expr.Replace a1 b1 % []) (Expr.Replace a2 b2 % []) = do
--    ?h
--unifyChanges _ _ = Nothing




--
