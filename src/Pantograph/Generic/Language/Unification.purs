module Pantograph.Generic.Language.Unification where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Language.Common
import Prelude

import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (sequence, traverse)
import Data.Tree (Tree(..))
import Data.Tuple (Tuple(..), uncurry)
import Todo (todo)

-- | Try to unify `a₁ : Sort` and `a₂ : Sort`. If they can unify, then yield the
-- | unified `a : Sort` and the unifying `σ : SortVarSubst` such that `a = σ a₁
-- | = σ a₂`.
unifySort :: forall sn el. Language sn el => Sort sn -> Sort sn -> Maybe (Sort sn /\ SortVarSubst sn)
unifySort (Tree (SN sn1) ss1) (Tree (SN sn2) ss2) 
  | sn1 == sn2 = do
    ss /\ sigmas <- Array.unzip <$> uncurry unifySort `traverse` Array.zip ss1 ss2
    let sigma = Array.foldr append mempty sigmas
    Just $ Tree (SN sn1) ss /\ sigma
unifySort (Tree (VarSN x1) _) s2@(Tree (VarSN x2) _) 
  | x1 == x2 = Just (s2 /\ mempty)
unifySort (Tree (VarSN x1) _) s2 = 
  Just (s2 /\ (SortVarSubst $ Map.singleton x1 s2))
unifySort s1 s2@(Tree (VarSN _) _) = unifySort s2 s1
unifySort _ _ = Nothing

unifySort3 :: forall sn el. Language sn el => Sort sn -> Sort sn -> Sort sn -> Maybe (Sort sn /\ SortVarSubst sn)
unifySort3 s1 s2 s3 = do
  s12 /\ sigma <- unifySort s1 s2
  unifySort s12 (applySortVarSubst sigma s3)

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
--   -- todo ""
