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
import Language.Pantograph.Generic.Edit as Edit
import Debug (trace, traceM)
import Text.Pretty (pretty)
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.Expr as Expr
import Data.List.Rev as RevList
import Control.Apply (lift2)
import Data.Tuple (fst)

greyRuleSigmaLabel :: String
greyRuleSigmaLabel = "grey-anything"

--type SplitChangeType l = SortChange l -> {downChange :: SortChange l, upChange :: SortChange l, cursorSort :: Sort l}

createGreyedRules :: forall l r. Grammar.IsRuleLabel l r =>
    Int -- what'th child will be effectively the value of this rule
    -> r -- label for the regular construct
    -> Maybe r -- what rule label to use for the greyed construct
    -> Base.SplitChangeType l
    -> (DerivLabel l r -> Maybe (DerivLabel l r))
    -> LanguageChanges l r -- TODO: when I delete this argument also delete this type definition in Base.purs
    -> Array (Smallstep.StepRule l r)
createGreyedRules index regularRuleLabel maybeGreyRuleLabel splitChange forgetSorts languageChanges =
        let (Grammar.Rule ruleVars ruleChildren _conclusion) = TotalMap.lookup regularRuleLabel Grammar.language in
        let Grammar.ChangeRule vars kidChanges = TotalMap.lookup regularRuleLabel languageChanges in
        let kidChange = Util.fromJust' "cgdr1" (Array.index kidChanges index) in
        let otherRuleChildren = Util.fromJust (Array.deleteAt index ruleChildren) in
--        let otherKidChanges = map Smallstep.metaInject $ otherRuleChildren in
--        let otherKidSorts = Util.fromJust (Array.deleteAt index ruleChildren) in
        let {downChange, upChange, cursorSort: _} = splitChange kidChange in
--        let metadDownChange = map MInj downChange in
--        let metadUpChange = map MInj upChange in
        [
        -- delete regular rule / replace with greyed if any other children are non-default derivations
            \_parentTooth -> case _ of
                ((Smallstep.Boundary Smallstep.Down c) % [
                    dterm@((SSInj label@(Grammar.DerivLabel l sub)) % kids)
                ]) | l == regularRuleLabel -> do
--                    let (Tooth _label (ZipList.Path {left, right})) /\ _ = Util.fromJust $ Expr.tooth index dterm
--                    dleft <- sequence (map ssTermToTerm left)
--                    dright <- sequence (map ssTermToTerm right)
--                    let realChange = Smallstep.getPathChange2
--                            (Path (List.singleton (Tooth label (ZipList.Path {left: dleft, right: dright})))) forgetSorts
--                    let {downChange: realDownChange, upChange: realUpChange, cursorSort: _} = splitChange realChange
----                    _ <- if realUpChange == c then Just unit else Nothing
--                    traceM ("here in grey down, l is: " <> pretty l <> " and c is: " <> pretty c <> " and upChange is: " <> pretty upChange)
                    _sortSub /\ chSub1 <- getChangeMatches c (invert upChange)
--                    traceM "got here"
                    chSub <- MultiMap.toMap chSub1
                    let subFull = map (map Expr.CInj) sub
                    let sub' = Map.union chSub subFull -- NOTE: Map.union uses first argument on duplicates, so we only use subFull for metavars not changed
                    let kid = Util.fromJust $ (Array.index kids index)
                    let wrapGrey = case maybeGreyRuleLabel of -- If there is a greyed version of the rule, then wrap it around the result
                            Just greyRuleLabel ->
                                let kidGSorts' = map (subSomeMetaChange sub' <<< inject) otherRuleChildren in
                                let kidsWithBoundaries = Array.zipWith (\ch' kid -> wrapBoundary Down ch' kid) kidGSorts' kids in
                                let x = Expr.RuleMetaVar greyRuleSigmaLabel in
                                let xSort = Expr.fromMetaVar x in
                                let sigma' = Map.insert x xSort (map rEndpoint sub') in
                                \kid' -> SSInj (DerivLabel greyRuleLabel sigma') % Util.fromJust (Array.insertAt index kid' kidsWithBoundaries)
                            _ -> \x -> x
                    pure $ wrapGrey (wrapBoundary Down (subSomeMetaChange sub' downChange) kid)
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
            -- insert regular rule
            \ _parentTooth sterm -> case sterm of
                ((Smallstep.Boundary Smallstep.Down c) % [
                    kid
                ]) -> do
--                    traceM ("here in grey down, kid is: " <> pretty kid <> " and c is: " <> pretty c <> " and upChange is: " <> pretty upChange)
                    sortSub1 /\ chSub1 <- getChangeMatches c upChange
--                    traceM "got here["
                    let (Tooth dl _) /\ _ = Edit.newToothFromRule regularRuleLabel index
                    let freshSub = Util.fromJust (derivLabelSub dl)
                    chSub <- MultiMap.toMap chSub1
                    sortSub <- MultiMap.toMap sortSub1
                    let sub = Map.union (Util.union' sortSub (map rEndpoint chSub)) freshSub -- Any vars that aren't determined get set to fresh ones
                    let subFull = map (map Expr.CInj) sub
                    let chSubFull = Map.union chSub subFull -- NOTE: Map.union uses first argument on duplicates, so we only use subFull for metavars not changed
--
                    let otherChildren = (termToSSTerm <<< Util.fromJust <<< defaultDerivTerm <<< subMetaExprPartially sub)
                            <$> otherRuleChildren
--                    let tooth = \kid -> Array.insertAt index kid otherChildren
                    let kidDownChange = (subSomeMetaChange chSubFull (invert downChange))
--                    traceM ("insert lambda here returings: downChange: " <> pretty downChange <> " chSubFull: " <> pretty chSubFull <> " kidDownChange: " <> pretty kidDownChange)
                    pure $ (SSInj (DerivLabel regularRuleLabel sub))
                        % (Util.fromJust (Array.insertAt index (wrapBoundary Down kidDownChange kid) otherChildren))
                _ -> Nothing
        ]


-- NOTE: this function doesn't work right, because the child ends up as ?x instead of (Term ?gamma ?x)
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


-- Really what I want is more of a one-sided unification.


--

---- Find substitution of variables in left argument to values in right argument
--getChangeMatches :: forall l. Expr.IsExprLabel l => MetaChange l -> Expr.MetaChange l
--    -> Maybe (MultiMap MetaVar (MetaExpr l) /\ MultiMap MetaVar (Expr.MetaChange l))
--getChangeMatches c1 c2 =
----    case l1 of
----        Expr.MV x -> Just $ MultiMap.insert x e2 (MultiMap.empty)
----        Expr.MInj l | l == l2 -> ?h -- foldl (lift2 MultiMap.union) (Just MultiMap.empty) (Array.zipWith getMatches kids1 kids2)
----        _ ->
----            Nothing
--    case c1 /\ c2 of
--        (Expr (Plus (Tooth l1 (ZipList.Path {left, right}))) [c1])
--            /\ (Expr (Plus (Tooth l2 (ZipList.Path {left: left2, right: right2}))) [c2]) | l1 == l2 -> do
--            subs1 /\ csubs1 <- getChangeMatches c1 c2
--            ?h
--        (Expr (Minus l1) [c1]) /\ (Expr (Minus l2) [c2]) -> ?h
--        (Expr (CInj l1) kids1) /\ (Expr (CInj l2) kids2) -> ?h
--        (Replace a1 b1 % [] /\ Replace a2 b2 % []) -> ?h
--        _ -> Nothing

-- These will return a substitution of variables in the second argument for corresponding
-- values in the first argument.
getSortMatches :: forall l. IsExprLabel l => MetaExpr l -> MetaExpr l -> Maybe (MultiMap MetaVar (MetaExpr l))
getSortMatches (l1 % kids1) (l2 % kids2) | l1 == l2 =
    MultiMap.unions <$> sequence (Array.zipWith getSortMatches kids1 kids2)
getSortMatches e2 (MV mv % []) = Just (MultiMap.insert mv e2 MultiMap.empty)
getSortMatches _ _ = Nothing

-- helper function for getChangeMatches
getToothMatches :: forall l. IsExprLabel l =>
    Tooth (Meta l) -> Tooth (Meta l)
    -> Maybe (MultiMap MetaVar (MetaExpr l))
getToothMatches (Tooth l1 (ZipList.Path {left: left1, right: right1}))
           (Tooth l2 (ZipList.Path {left: left2, right: right2})) =
    if not (l1 == l2 && List.length right1 == List.length right2) then Nothing else do
    leftMatches <- sequence $ List.zipWith getSortMatches (RevList.unreverse left1) (RevList.unreverse left2)
    rightMatches <- sequence $ List.zipWith getSortMatches right1 right2
    pure $ MultiMap.union (MultiMap.unions leftMatches) (MultiMap.unions rightMatches)

getChangeMatches :: forall l. IsExprLabel l =>
    -- Two kinds of slots: those in change positions, and those in expression postions
    MetaChange l -> MetaChange l
    -- Two kinds out outputs: expressions and changes
    -> Maybe ((MultiMap MetaVar (MetaExpr l)) /\ (MultiMap MetaVar (MetaChange l)))
getChangeMatches c (CInj (MV mv) % []) = Just (MultiMap.empty /\ MultiMap.insert mv c MultiMap.empty)
getChangeMatches (l1 % kids1) (l2 % kids2) | l1 == l2 =
    foldl (\(a/\b) (c/\d) -> MultiMap.union a c /\ MultiMap.union b d) (MultiMap.empty /\ MultiMap.empty)
        <$> sequence (Array.zipWith getChangeMatches kids1 kids2)
getChangeMatches (Plus th1 % [kid1]) (Plus th2 % [kid2]) = do
    toothMatches <- getToothMatches th1 th2
    es /\ cs <- getChangeMatches kid1 kid2
    pure $ (MultiMap.union toothMatches es) /\ cs
getChangeMatches (Minus th1 % [kid1]) (Minus th2 % [kid2]) = do
    toothMatches <- getToothMatches th1 th2
    es /\ cs <- getChangeMatches kid1 kid2
    pure $ (MultiMap.union toothMatches es) /\ cs
getChangeMatches (Replace a1 b1 % []) (Replace a2 b2 % []) = do
    matches1 <- getSortMatches a1 a2
    matches2 <- getSortMatches b1 b2
    pure $ (MultiMap.union matches1 matches2) /\ MultiMap.empty
getChangeMatches _ _ = Nothing