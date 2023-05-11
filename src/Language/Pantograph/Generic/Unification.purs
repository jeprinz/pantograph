module Language.Pantograph.Generic.Unification where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Language.Pantograph.Generic.Grammar
import Data.Maybe (Maybe)
import Data.List(List(..), (:))
import Data.List as List
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.UUID (UUID)
import Data.Gram (Gram(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unify (Meta, MetaVar)
import Data.Foldable (foldl)
import Data.Unify (freshMetaVar)
import Data.Gram as Gram
import Data.Unify (Meta(..))
import Util (lookup', union')
import Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)
import Control.Apply (lift2)

{-

Here is roughly how unify should work:

unify :: forall l. (l -> l -> Maybe Sub) -> Expr l -> Expr l -> Maybe Sub

unify (Minus A B1) (Minus A B2) -> unify B1 B2
unify (Minus A1 B1) (Minus A2 B2) -> fail if A1 =/= A2

we need to be able to:
- unify an Expr l with a Change l, getting a substitution of metavars for changes
-

In order to be able to do
unify (+A -> B) (+A' -> B'), we would need to unify the EXPRESSIONS A and A'. But those are in labels in B and B'!

Its not clear to me though if we'll need to be able to do that.
We might only need to do unification of changes with expressions, or changes with changes where the metavariables are
only in the change parts, not the expression parts.

The reasoning is that when we perform the typechanging algorithm, the change going in doesn't have metavariables -
its really only being unified with an expression that has metavariables.

If I have a Change = Expr (ChangeLabel l), and then I turn it into something with metavariables for unification
then I have Expr (ChangeLabel (Meta l)), which still doesn't have metavariables in the teeth things.

It seems wierd to have an change with both "change metavariables" and "expression metavariables" in it anyway.

-}

type Ren = Map MetaVar MetaVar

genFreshener :: Set MetaVar -> Ren
genFreshener vars = foldl
    (\acc x -> Map.insert x (freshMetaVar unit) acc)
    Map.empty vars

class Freshenable t where
    freshen :: Ren -> t -> t

instance Freshenable (Gram.MetaExpr l) where
    freshen sub (Gram.Gram ((Meta (Left x)) /\ [])) = Gram.Gram (Meta (Left (lookup' x sub)) /\ [])
    freshen sub (Gram.Gram ((Meta (Left x)) /\ _)) = unsafeCrashWith "wrong number of children on metavar label"
    freshen sub (Gram.Gram ((Meta (Right l)) /\ kids)) = Gram.Gram ((Meta (Right l)) /\ (map (freshen sub) kids))

type Sub l = Map MetaVar (MetaExpr l)

subMetaExpr :: forall l. Sub l -> Gram.MetaExpr l -> Gram.MetaExpr l
subMetaExpr sub (Gram.Gram ((Meta (Left x)) /\ [])) = lookup' x sub
subMetaExpr sub (Gram.Gram ((Meta (Left x)) /\ _)) = unsafeCrashWith "wrong number of children on metavar label"
subMetaExpr sub (Gram.Gram ((Meta (Right l)) /\ kids)) = Gram.Gram ((Meta (Right l)) /\ (map (subMetaExpr sub) kids))

assertNoMetavars :: forall l. Gram.MetaExpr l -> Gram.Expr l
assertNoMetavars (Gram.Gram ((Meta (Left x)) /\ _)) = unsafeCrashWith "has a metavar"
assertNoMetavars (Gram.Gram ((Meta (Right l)) /\ kids)) = Gram.Gram (l /\ (map assertNoMetavars kids))

fullySubMetaExpr :: forall l. Map MetaVar (Gram.Expr l) -> Gram.MetaExpr l -> Gram.Expr l
fullySubMetaExpr sub (Gram.Gram ((Meta (Left x)) /\ [])) = lookup' x sub
fullySubMetaExpr sub (Gram.Gram ((Meta (Left x)) /\ _)) = unsafeCrashWith "wrong number of children on metavar label"
fullySubMetaExpr sub (Gram.Gram ((Meta (Right l)) /\ kids)) = Gram.Gram (l /\ (map (fullySubMetaExpr sub) kids))

------------- Unification ------------------------------------------------------

-- we may need a more general notion of unification later, but this is ok for now
unify :: forall l. Eq l => Gram.MetaExpr l -> Gram.MetaExpr l -> Maybe (Gram.MetaExpr l /\ Sub l)
unify e1@(Gram.Gram (Meta l1 /\ kids1)) e2@(Gram.Gram (Meta l2 /\ kids2)) =
    case l1 /\ l2 of
        Left x /\ _ -> Just (e2 /\ Map.insert x e2 Map.empty)
        _ /\ Left x -> unify e2 e1
        Right l /\ Right l' | l == l' -> do
            kids' /\ sub <- unifyLists (List.fromFoldable kids1) (List.fromFoldable kids2)
            pure (Gram.Gram (Meta (Right l) /\ Array.fromFoldable kids') /\ sub)
        _ /\ _ -> Nothing

unifyLists :: forall l. Eq l => List (Gram.MetaExpr l) -> List (Gram.MetaExpr l) -> Maybe (List (Gram.MetaExpr l) /\ Sub l)
unifyLists Nil Nil = Just (Nil /\ Map.empty)
unifyLists (e1 : es1) (e2 : es2) = do
    e /\ sub <- unify e1 e2
    let es1' = map (subMetaExpr sub) es1
    let es2' = map (subMetaExpr sub) es2
    es /\ sub2 <- unifyLists es1' es2'
    pure $ (e : es) /\ union' sub sub2
unifyLists _ _ = unsafeCrashWith "shouldn't happen"

------------- Another operation I need for typechanges stuff ------------------

getMatches :: forall l. Eq l => Ord l => Gram.MetaExpr l -> Gram.Expr l -> Maybe (MultiMap MetaVar (Gram.Expr l))
getMatches (Gram.Gram (Meta l1 /\ kids1)) e2@(Gram.Gram (l2 /\ kids2)) =
    case l1 of
        Left x -> Just $ MultiMap.insert x e2 (MultiMap.empty)
        Right l | l == l2 -> foldl (lift2 MultiMap.union) (Just MultiMap.empty) (getMatches <$> kids1 <*> kids2)
        _ -> Nothing






