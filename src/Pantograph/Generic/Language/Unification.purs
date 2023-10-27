module Pantograph.Generic.Language.Unification where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Language.Common
import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tree (Tree(..))
import Data.Tuple (uncurry)

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
