module Language.Pantograph.Lib.GreyedRules where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.List (List(..), (:))
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Grammar
import Data.Expr
import Data.Expr as Expr
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Smallstep
import Language.Pantograph.Generic.Smallstep as Smallstep
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
import Bug as Bug
import Language.Pantograph.Generic.Rendering.Base as Base

greyRuleSigmaLabel :: String
greyRuleSigmaLabel = "grey-anything"

--type SplitChangeType l = SortChange l -> {downChange :: SortChange l, upChange :: SortChange l, cursorSort :: Sort l}

createGreyedDownRules :: forall l r. Grammar.IsRuleLabel l r =>
    Int -- what'th child will be effectively the value of this rule
    -> r -- label for the regular construct
    -> Maybe r -- what rule label to use for the greyed construct
    -> Base.EditorSpec l r
    -> Array (Smallstep.StepRule l r)
createGreyedDownRules index regularRuleLabel maybeGreyRuleLabel spec =
        let (Grammar.Rule _vars children _conclusion) = TotalMap.lookup regularRuleLabel Grammar.language in
        let Grammar.ChangeRule vars kidChanges = TotalMap.lookup regularRuleLabel spec.languageChanges in
        let kidChange = Util.fromJust' "cgdr1" (Array.index kidChanges index) in
        let otherKidChanges = map Smallstep.metaInject $ Util.fromJust (Array.deleteAt index children) in
--        let otherKidSorts = Util.fromJust (Array.deleteAt index children) in
        let {downChange, upChange, cursorSort: _} = spec.splitChange kidChange in
        let metadDownChange = map MInj downChange in
        let metadUpChange = map MInj upChange in
        [
        -- delete regular rule / replace with greyed if any other children are non-default derivations
            case _ of
                ((Smallstep.Boundary Smallstep.Down c) % [
                    (SSInj (Grammar.DerivLabel l sub)) % kids
                ]) | l == regularRuleLabel -> do
                    chSub /\ chBackUp <- doOperation c metadUpChange --
                    let subFull = map (map Expr.CInj) sub
                    let sub' = Map.union chSub subFull -- NOTE: Map.union uses first argument on duplicates, so we only use subFull for metavars not changed
                    let kid = Util.fromJust $ (Array.index kids index)
                    let wrapGrey = case maybeGreyRuleLabel of -- If there is a greyed version of the rule, then wrap it around the result
                            Just greyRuleLabel ->
                                let kidGSorts' = map (Expr.subMetaExpr sub') otherKidChanges in
                                let kidsWithBoundaries = Array.zipWith (\ch' kid -> wrapBoundary Down ch' kid) kidGSorts' kids in
                                let x = Expr.RuleMetaVar greyRuleSigmaLabel in
                                let xSort = Expr.fromMetaVar x in
                                let sigma' = Map.insert x xSort (map rEndpoint sub') in
                                \kid' -> SSInj (DerivLabel greyRuleLabel sigma') % Util.fromJust (Array.insertAt index kid' kidsWithBoundaries)
                            _ -> \x -> x
                    pure $ wrapBoundary Up chBackUp (wrapGrey (wrapBoundary Down (subMetaExpr sub' metadDownChange) kid))
                _ -> Nothing
        ]
        <> case maybeGreyRuleLabel of -- These two rules only exist if we have a greyed constructor
            Just greyRuleLabel -> [
            -- replace greyed with regular rule
            -- removed greyed rule if all other children are default
    --                    let otherKidsAreDefault = Array.all (\dterm ->
    --                            case Grammar.defaultDerivTerm dterm of
    --                                Just default | not (termToSSTerm default == dterm) -> true
    --                                Nothing -> false) otherKids
            ]
            Nothing -> []
        <> [
--            -- insert regular rule
--            case _ of
--                ((Smallstep.Boundary Smallstep.Down c) % [
--                    asdfasdf
--                ]) -> do
--                    chSub /\ chBackUp <- doOperation (invert c) metadUpChange --
--                    -- Need to 1) refactor tooth part out of newPathFromRule 2) call that part here 3) that gives me sub?
--
--                    let subFull = map (map Expr.CInj) ?sub
--                    let sub' = Map.union chSub subFull -- NOTE: Map.union uses first argument on duplicates, so we only use subFull for metavars not changed
--                    ?h
--                _ -> Nothing
        ]


createGreyedConstruct :: forall l r.
    Grammar.Rule l -> Int -> Grammar.Rule l
createGreyedConstruct (Grammar.Rule vars children conclusion) index =
    let x = Expr.RuleMetaVar greyRuleSigmaLabel in
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
      Expr.CInj l -> Expr.CInj l -- NOTE: if l was a metavar, we wouldn't get here because subSomeMetaChange would have dealt with it.
      Expr.Replace e1 e2 -> Expr.Replace (subExpr e1) (subExpr e2)

subSomeMetaChange :: forall l. Expr.IsExprLabel l => ChSub l -> Expr.MetaChange l -> Expr.MetaChange l
subSomeMetaChange sub (Expr.Expr l kids) =
    case l of
        Expr.CInj (Expr.MV x) | Just s <- Map.lookup x sub
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
--unifyChanges (Expr.CInj (Expr.Meta (Left x)) % []) c = Just (Map.singleton x c)
--unifyChanges c (Expr.CInj (Expr.Meta (Left x)) % []) = Just (Map.singleton x c)
--unifyChanges (Expr.CInj (Expr.Meta (Right l1)) % kids1) (Expr.CInj (Expr.Meta (Right l2)) % kids2) | l1 == l2 =
--    let a = (foldl (composeC) Map.empty) <$> sequence (Array.zipWith unifyChanges kids1 kids2) in
----    let a = foldl ?h ?h <$> sequence (Array.zipWith unifyChanges kids1 kids2) in
--    ?h
--unifyChanges (Expr.Plus th1 % [kid1]) ((Expr.Plus th2) % [kid2]) = ?h
--unifyChanges (Expr.Minus th1 % [kid1]) ((Expr.Minus th2) % [kid2]) = ?h
--unifyChanges (Expr.Replace a1 b1 % []) (Expr.Replace a2 b2 % []) = do
--    ?h
--unifyChanges _ _ = Nothing




--
