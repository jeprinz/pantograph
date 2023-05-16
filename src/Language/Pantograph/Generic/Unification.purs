module Language.Pantograph.Generic.Unification where

import Prelude

import Bug as Bug
import Bug.Assertion (Assertion(..), assert, assertInput)
import Control.Apply (lift2)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Pretty (pretty, quotes)
import Util (lookup', union')

{-

Here is roughly how unify should work:

unify :: forall l. (l -> l -> Maybe Sub) -> Expr l -> Expr l -> Maybe Sub

unify (Minus A B1) (Minus A B2) -> unify B1 B2
unify (Minus A1 B1) (Minus A2 B2) -> fail if A1 =/= A2

we need to be able to:
- unify an Expr l with a Change l, getting a substitution of metavars for changes
-

In order to be able to do
unify (+A -> B) (+A' -> B'), we would need to unify the EXPRESSIONS A and A'. But those are in ExprLabels in B and B'!

Its not clear to me though if we'll need to be able to do that.
We might only need to do unification of changes with expressions, or changes with changes where the metavariables are
only in the change parts, not the expression parts.

The reasoning is that when we perform the typechanging algorithm, the change going in doesn't have metavariables -
its really only being unified with an expression that has metavariables.

If I have a Change = Expr (ChangeLabel l), and then I turn it into something with metavariables for unification
then I have Expr (ChangeLabel (Meta l)), which still doesn't have metavariables in the teeth things.

It seems wierd to have an change with both "change metavariables" and "expression metavariables" in it anyway.

-}

type Ren = Map Expr.MetaVar Expr.MetaVar

genFreshener :: Set Expr.MetaVar -> Ren
genFreshener vars = foldl
    (\acc x -> Map.insert x (Expr.freshMetaVar' unit) acc)
    Map.empty vars

class Freshenable t where
    freshen :: Ren -> t -> t

instance Freshenable (Expr.Meta l) where
    freshen ren (Expr.Meta (Left x)) = Expr.Meta (Left (lookup' x ren))
    freshen ren (Expr.Meta (Right l)) = Expr.Meta (Right l)

-- TODO: I really want this definition, but then I get an overlapping instances issue
--instance (Functor f, Freshenable l) => Freshenable (f l) where
--    freshen ren x = map (freshen sub) x

-- TODO: so instead, I have it written manually in some specific cases

instance Freshenable l => Freshenable (Expr.Expr l) where
    freshen sub expr = map (freshen sub) expr

instance Freshenable l => Freshenable (Expr.ChangeLabel l) where
    freshen sub l = map (freshen sub) l

type Sub l = Map Expr.MetaVar (Expr.MetaExpr l)

noMetaVars :: forall l. Expr.IsExprLabel l => String -> Expr.MetaExpr l -> Assertion (Expr.Expr l)
noMetaVars source mexpr0 = Assertion
    { name: "noMetaVars", source
    , result: do
        let go = assertInput (Expr.wellformedExpr "noMetaVars") \mexpr -> case mexpr of
                Expr.Meta (Left x) % [] -> throwError $ "Found MetaVar " <> quotes (pretty mexpr)
                Expr.Meta (Right l) % kids -> (l % _) <$> go `traverse` kids
        go mexpr0
    }

------------- Unification ------------------------------------------------------

-- we may need a more general notion of unification later, but this is ok for now
-- NOTE: should prefer substituting variables from the left if possible
unify :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> Maybe (Expr.MetaExpr l /\ Sub l)
unify e1@(Expr.Expr (Expr.Meta l1) kids1) e2@(Expr.Expr (Expr.Meta l2) kids2) =
    case l1 /\ l2 of
        Left x /\ _ -> Just (e2 /\ Map.insert x e2 Map.empty)
        _ /\ Left x -> unify e2 e1
        Right l /\ Right l' | l == l' -> do
            kids' /\ sub <- unifyLists (List.fromFoldable kids1) (List.fromFoldable kids2)
            pure (Expr.Expr (Expr.Meta (Right l)) (Array.fromFoldable kids') /\ sub)
        _ /\ _ -> Nothing

unifyLists :: forall l. Expr.IsExprLabel l => List (Expr.MetaExpr l) -> List (Expr.MetaExpr l) -> Maybe (List (Expr.MetaExpr l) /\ Sub l)
unifyLists Nil Nil = Just (Nil /\ Map.empty)
unifyLists (e1 : es1) (e2 : es2) = do
    e /\ sub <- unify e1 e2
    let es1' = map (Expr.subSomeMetaVars sub) es1
    let es2' = map (Expr.subSomeMetaVars sub) es2
    es /\ sub2 <- unifyLists es1' es2'
    pure $ (e : es) /\ union' sub sub2
unifyLists _ _ = Bug.bug "[unifyLists] shouldn't happen"

------------- Another operation I need for typechanges stuff ------------------

getMatches :: forall l. Eq l => Ord l => Expr.MetaExpr l -> Expr.Expr l -> Maybe (MultiMap Expr.MetaVar (Expr.Expr l))
getMatches (Expr.Expr (Expr.Meta l1) kids1) e2@(Expr.Expr l2 kids2) =
    case l1 of
        Left x -> Just $ MultiMap.insert x e2 (MultiMap.empty)
        Right l | l == l2 -> foldl (lift2 MultiMap.union) (Just MultiMap.empty) (getMatches <$> kids1 <*> kids2)
        _ -> Nothing






