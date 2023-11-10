module Pantograph.Generic.Language.Unification where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Language
import Prelude
import Util

import Control.Alternative (guard)
import Data.Array as Array
import Data.Display (display)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Supertype as Supertype
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Halogen.Elements as El
import Pantograph.Generic.GlobalMessageBoard as GMB
import Text.Pretty (pretty)
import Todo (todo)

-- | Try to unify `a₁ : Sort` and `a₂ : Sort`. If they can unify, then yield the
-- | unified `a : Sort` and the unifying `σ : SortVarSubst` such that `a = σ a₁
-- | = σ a₂`.
unifySort :: forall sn el. Eq sn => Show sn => PrettyTreeNode sn => Sort sn -> Sort sn -> Maybe (Sort sn /\ SortVarSubst sn)
unifySort (Tree (SN sn1) ss1) (Tree (SN sn2) ss2) 
  | sn1 == sn2 = do
    ss /\ sigmas <- Array.unzip <$> uncurry unifySort `traverse` Array.zip ss1 ss2
    let sigma = Array.fold sigmas
    Just $ Tree (SN sn1) ss /\ sigma
unifySort (Tree (VarSN x1) _) s@(Tree (VarSN x2) _) | x1 == x2 = Just (s /\ mempty)
unifySort (Tree (VarSN x) _) s = Just (s /\ (SortVarSubst $ Map.singleton x s))
unifySort s (Tree (VarSN x) _) = Just (s /\ (SortVarSubst $ Map.singleton x s))
unifySort _ _ = Nothing

unifySort3 :: forall sn el. Language sn el => Sort sn -> Sort sn -> Sort sn -> Maybe (Sort sn /\ SortVarSubst sn)
unifySort3 s1 s2 s3 = do
  s12 /\ sigma <- unifySort s1 s2
  s123 /\ sigma' <- unifySort s12 (applySortVarSubst sigma s3)
  let sigma'' = sigma' <> sigma
  pure (s123 /\ sigma'')

-- unifySortVarSubst :: forall sn el. Language sn el => SortVarSubst sn -> SortVarSubst sn -> Maybe (SortVarSubst sn)
-- unifySortVarSubst sigma1@(SortVarSubst m1) sigma2@(SortVarSubst m2) = do
--   -- apply sigma1 to each value of m2, and apply sigma2 to each value of m1
--   -- unify the values of each overlapping key of m1 and m2
--   let m1' = m1 <#> applySortVarSubst sigma2
--   let m2' = m2 <#> applySortVarSubst sigma1
--   -- let ks = Set.intersection (Map.keys m1) (Map.keys m2)
--   let es = Map.intersectionWith Tuple m1' m2'
--   let m = List.foldl ?a (Set.toUnfoldable ks :: List _) 
--   -- map SortVarSubst $ sequence $ Map.unionWith ?a m1' m2'
--   ?a

{-
TODO: this can result in a `SortVarSubst` as well as the `RuleSortVarSubst`. For
example:
```
  s  = ?γ ⊢ ?α rs = ($x : $α), $γ ⊢ $α
```
We actually need to first substitute `?γ` for `(?x : ?α), ?γ' ⊢ ?β` in a
`SortVarSubst`. These are fresh `SortVar`s, and other appearances of `?γ` need
to know to be substituted as well.
-}

unifySortWithRuleSort :: forall sn el. Language sn el => Sort sn -> RuleSort sn -> Maybe (RuleSortVarSubst (Sort sn))
-- unifySortWithRuleSort s rs = GMB.debugR (display "unifySortWithRuleSort") {s: display s, rs: display rs} \_ -> unifySortWithRuleSort' s rs
unifySortWithRuleSort s rs = do
  GMB.debugRM (display "unifySortWithRuleSort") {s: display s, rs: display rs}
  r <- unifySortWithRuleSort' s rs
  GMB.debugRM (display "unifySortWithRuleSort") {s: display s, rs: display rs, result: display r}
  pure r

unifySortWithRuleSort' :: forall sn el. Language sn el => Sort sn -> RuleSort sn -> Maybe (RuleSortVarSubst (Sort sn))
unifySortWithRuleSort' (Tree sn1 ss1) (Tree (InjectRuleSortNode sn2) ss2) = do
    guard $ sn1 == sn2
    -- Array.foldM composeRuleSortVarSubstSort emptyRuleSortVarSubst =<< traverse (uncurry unifySortWithRuleSort) (Array.zip ss1 ss2)
    GMB.debugRM (display "unifySortWithRuleSort'") {ss1: display ss1, ss2: display ss2}
    sigmas <- traverse (uncurry unifySortWithRuleSort) (Array.zip ss1 ss2)
    GMB.debugRM (display "unifySortWithRuleSort'") {sigmas: display sigmas}
    r <- Array.foldM composeRuleSortVarSubstSort emptyRuleSortVarSubst sigmas
    GMB.debugRM (display "unifySortWithRuleSort'") {sigmas: display sigmas, result: display r}
    Just r
unifySortWithRuleSort' s (VarRuleSortNode x % []) = Just $ singletonRuleSortVarSubst x s
unifySortWithRuleSort' _ _ = Nothing

unifyChangeWithRuleChange :: forall sn el. Language sn el => SortChange sn -> RuleSortChange sn -> Maybe (RuleSortVarSubst (SortChange sn))
unifyChangeWithRuleChange ch (VarRuleSortNode x %! []) = Just $ singletonRuleSortVarSubst x ch
unifyChangeWithRuleChange ch1@(sign1 /\ (sn1 %- i1 /\ kids1) %!/ kid1) ch2@(sign2 /\ (InjectRuleSortNode sn2 %- i2 /\ kids2) %!/ kid2) = do
  guard $ sign1 == sign2
  guard $ i1 == i2
  guard $ sn1 == sn2
  if true then do
    GMB.debugRM (display "unifyChangeWithRuleChange / Shift") {ch1: display ch1, ch2: display ch2}
    sigma1 <- map Array.fold $ uncurry (\s rs -> Supertype.inject <$$> unifySortWithRuleSort s rs) `traverse` Array.zip kids1 kids2
    GMB.debugRM (display "unifyChangeWithRuleChange / Shift") {sigma1: display sigma1}
    sigma2 <- unifyChangeWithRuleChange kid1 kid2
    GMB.debugRM (display "unifyChangeWithRuleChange / Shift") {sigma12: display sigma2}
    Just $ sigma1 <> sigma2
  else do
    sigma1 <- map Array.fold $ uncurry (\s rs -> Supertype.inject <$$> unifySortWithRuleSort s rs) `traverse` Array.zip kids1 kids2
    sigma2 <- unifyChangeWithRuleChange kid1 kid2
    Just $ sigma1 <> sigma2
unifyChangeWithRuleChange ch1@(s1 %!~> s1') ch2@(s2 %!~> s2') = do
  if true then do
    GMB.debugRM (display "unifyChangeWithRuleChange / Replace") {ch1: display ch1, ch2: display ch2}
    sigma1 <- unifySortWithRuleSort s1 s2
    GMB.debugRM (display "unifyChangeWithRuleChange / Replace") {sigma1: display sigma1}
    sigma2 <- unifySortWithRuleSort s1' s2'
    GMB.debugRM (display "unifyChangeWithRuleChange / Replace") {sigma12: display sigma2}
    -- Just $ (Supertype.inject <$> sigma1) <> (Supertype.inject <$> sigma2)
    let sigma1' = Supertype.inject <$> sigma1
    let sigma2' = Supertype.inject <$> sigma2
    GMB.debugRM (display "unifyChangeWithRuleChange / Replace") {sigma1': display sigma1', sigma2': display sigma2'}
    Just $ sigma1' <> sigma2'
  else do
    sigma1 <- unifySortWithRuleSort s1 s2
    sigma2 <- unifySortWithRuleSort s1' s2'
    Just $ (Supertype.inject <$> sigma1) <> (Supertype.inject <$> sigma2)
unifyChangeWithRuleChange ch1@(sn1 %! kids1) ch2@(InjectRuleSortNode sn2 %! kids2) = do
  guard $ sn1 == sn2
  if true then do
    GMB.debugRM (display "unifyChangeWithRuleChange / Inject") {ch1: display ch1, ch2: display ch2}
    sigmas <- uncurry unifyChangeWithRuleChange `traverse` Array.zip kids1 kids2
    GMB.debugRM (display "unifyChangeWithRuleChange / Inject") {sigmas: display sigmas}
    Just $ Array.fold sigmas
  else do
    map Array.fold $ uncurry unifyChangeWithRuleChange `traverse` Array.zip kids1 kids2
unifyChangeWithRuleChange _ _ = Nothing

-- | You can only compose `RuleSortVarSubst (Sort sn)` if each `RuleSortVar`
-- | maps to a `Sort` that is unifiable with all other `Sort`s that the
-- | `RuleSortVar` is mapped to.
composeRuleSortVarSubstSort' :: forall sn el. Language sn el => RuleSortVarSubst (Sort sn) -> RuleSortVarSubst (Sort sn) -> Maybe (RuleSortVarSubst (Sort sn) /\ SortVarSubst sn)
composeRuleSortVarSubstSort' (RuleSortVarSubst m1) (RuleSortVarSubst m2) = do
  m <- sequence $ Map.unionWith (\s1_ s2_ -> s1_ >>= \(s1 /\ _) -> s2_ >>= \(s2 /\ _) -> unifySort s1 s2) (m1 <#> \s -> Just (s /\ mempty)) (m2 <#> \s -> Just (s /\ mempty))
  let m' = m <#> fst
  let sigma = List.fold $ Map.values $ map snd $ m
  Just $ RuleSortVarSubst m' /\ sigma

-- | For all `RuleSortVar`s that are not substituted by `rsigma`, assign it to a
-- | fresh `SortVar`.
padRuleSortVarSubstWithFreshVarSorts :: forall sn el. Language sn el => el -> RuleSortVarSubst (Sort sn) -> RuleSortVarSubst (Sort sn)
padRuleSortVarSubstWithFreshVarSorts el (RuleSortVarSubst m) =
  let SortingRule rule = getSortingRule el in
  RuleSortVarSubst $ Map.fromFoldable $ (Set.toUnfoldable rule.parameters :: List _) <#> \x -> x /\ case Map.lookup x m of
    Nothing -> freshVarSortFromRuleSortVar x
    Just s -> s

generalizeExpr :: forall sn el. Language sn el => Expr sn el -> Expr sn el
generalizeExpr (EN el _ _ % kids) = EN el rsigma'' {} % kids''
  where
  kids' = generalizeExpr <$> kids
  sorts = getExprSort <$> kids'
  SortingRule rule = getSortingRule el
  rsigmas = map (fromJust' "generalizeExpr/rsigmas") $ map (uncurry unifySortWithRuleSort) $ Array.zip sorts rule.kids
  f (rsigma1 /\ sigma) rsigma2_ =
    let rsigma2 = map (applySortVarSubst sigma) rsigma2_ in
    fromJust' "generalizeExpr/f" $ composeRuleSortVarSubstSort' rsigma1 rsigma2
  rsigma' /\ sigma = Array.foldl f (emptyRuleSortVarSubst /\ (mempty :: SortVarSubst sn)) rsigmas
  rsigma'' = padRuleSortVarSubstWithFreshVarSorts el rsigma'
  kids'' = kids' <##> \(EN el' rsigma' _) -> EN el' (rsigma' <#> applySortVarSubst sigma) {}

generalizeExprPath :: forall sn el. Language sn el => ExprPath sn el -> ExprPath sn el
generalizeExprPath path0 = go path0 (freshVarSort "inner") mempty
  where
  go :: ExprPath sn el -> Sort sn -> ExprPath sn el -> ExprPath sn el
  go outer innerSort inner = case unconsPath outer of
    Nothing -> inner
    Just {outer: outer', inner: EN el _ _ %- i /\ kids} ->
      let 
        SortingRule rule = getSortingRule el
        kids' = generalizeExpr <$> kids
        sorts = fromJust' "generalizeExprPath/sorts" $ Array.insertAt i innerSort $ getExprSort <$> kids' 
      in
      GMB.debugR (display "generalizeExprPath/rsigmas") {sorts: display sorts, "rule.kids": display rule.kids} \_ ->
      let
        -- rsigmas = map (fromJust' "generalizeExprPath/rsigmas") $ map (uncurry unifySortWithRuleSort) $ Array.zip sorts rule.kids
        rsigmas = map (\(s /\ rs) -> GMB.debugR (display "generalizeExprPath/rsigmas") {s: display s, rs: display rs} \_ -> fromJust $ unifySortWithRuleSort s rs) $ Array.zip sorts rule.kids
        f (rsigma1 /\ sigma) rsigma2_ =
          let rsigma2 = map (applySortVarSubst sigma) rsigma2_ in
          fromJust' "generalizeExprPath/f" $ composeRuleSortVarSubstSort' rsigma1 rsigma2
        rsigma' /\ sigma = Array.foldl f (emptyRuleSortVarSubst /\ (mempty :: SortVarSubst sn)) rsigmas
        rsigma'' = padRuleSortVarSubstWithFreshVarSorts el rsigma'
        kids'' = kids' <##> \(EN el' rsigma' _) -> EN el' (rsigma' <#> applySortVarSubst sigma) {}
        th' = EN el rsigma'' {} %- i /\ kids''
        sort = getExprToothOuterSort th'
      in
      go outer' sort (inner `consPath` th')

generalizeExprNonEmptyPath :: forall sn el. Language sn el => ExprNonEmptyPath sn el -> ExprNonEmptyPath sn el
generalizeExprNonEmptyPath = fromPath "generalizeExprNonEmptyPath" <<< generalizeExprPath <<< toPath

generalizeClipboard :: forall sn el. Language sn el => Clipboard sn el -> Clipboard sn el
generalizeClipboard = case _ of
  ExprClipboard expr -> ExprClipboard $ generalizeExpr expr
  ExprNonEmptyPathClipboard path -> ExprNonEmptyPathClipboard $ generalizeExprNonEmptyPath path

