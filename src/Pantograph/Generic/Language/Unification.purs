module Pantograph.Generic.Language.Unification where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language.Common
import Prelude
import Util

import Control.Alternative (guard)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Supertype as Supertype
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Text.Pretty (pretty)
import Todo (todo)

-- | Try to unify `a₁ : Sort` and `a₂ : Sort`. If they can unify, then yield the
-- | unified `a : Sort` and the unifying `σ : SortVarSubst` such that `a = σ a₁
-- | = σ a₂`.
unifySort :: forall sn el. Language sn el => Sort sn -> Sort sn -> Maybe (Sort sn /\ SortVarSubst sn)
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

unifySortWithRuleSort :: forall sn el. Language sn el => Sort sn -> RuleSort sn -> Maybe (RuleSortVarSubst (Sort sn))
unifySortWithRuleSort (Tree sn1 ss1) (Tree (InjectRuleSortNode sn2) ss2) = do
    guard $ sn1 == sn2
    Array.foldM composeRuleSortVarSubstSort emptyRuleSortVarSubst =<< traverse (uncurry unifySortWithRuleSort) (Array.zip ss1 ss2)
unifySortWithRuleSort s (VarRuleSortNode x % []) = Just $ singletonRuleSortVarSubst x s
unifySortWithRuleSort _ _ = Nothing

unifyChangeWithRuleChange :: forall sn el. Language sn el => SortChange sn -> RuleSortChange sn -> Maybe (RuleSortVarSubst (SortChange sn))
unifyChangeWithRuleChange ch (VarRuleSortNode x %! []) = Just $ singletonRuleSortVarSubst x ch
unifyChangeWithRuleChange (sign1 /\ (sn1 %- i1 /\ kids1) %!/ kid1) (sign2 /\ (InjectRuleSortNode sn2 %- i2 /\ kids2) %!/ kid2) = do
  guard $ sign1 == sign2
  guard $ i1 == i2
  guard $ sn1 == sn2
  sigma1 <- map Array.fold $ uncurry (\s rs -> Supertype.inject <$$> unifySortWithRuleSort s rs) `traverse` Array.zip kids1 kids2
  sigma2 <- unifyChangeWithRuleChange kid1 kid2
  Just $ sigma1 <> sigma2
unifyChangeWithRuleChange (s1 %!~> s1') (s2 %!~> s2') = do
  sigma1 <- unifySortWithRuleSort s1 s2
  sigma2 <- unifySortWithRuleSort s1' s2'
  Just $ (Supertype.inject <$> sigma1) <> (Supertype.inject <$> sigma2)
unifyChangeWithRuleChange (sn1 %! kids1) (InjectRuleSortNode sn2 %! kids2) = do
  guard $ sn1 == sn2
  map Array.fold $ uncurry unifyChangeWithRuleChange `traverse` Array.zip kids1 kids2
unifyChangeWithRuleChange _ _ = Nothing
