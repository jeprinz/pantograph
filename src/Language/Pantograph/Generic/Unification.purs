module Language.Pantograph.Generic.Unification where

import Prelude

import Bug as Bug
import Bug.Assertion (Assertion(..), assert, assertInput_)
import Control.Apply (lift2)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either (Either(..))
import Data.Expr (subMetaExprPartially, (%))
import Data.Expr as Expr
import Data.Foldable (foldl)
import Data.Foldable as Foldable
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Pretty (pretty, quotes)
import Util (lookup', union')
import Debug (trace)
import Debug (traceM)
import Data.Traversable (sequence)
import Data.List.Zip as ZipList

--QUESTION from Jacob: what is Freshenable for? Do we use any of that?
--we already have an implementation of substitution elsewhere, which is
--much simpler and more general

type Ren = Expr.MetaVarSub Expr.MetaVar

genFreshener :: Set Expr.MetaVar -> Ren
genFreshener vars = foldl
    (\acc x -> Map.insert x (Expr.freshenMetaVar x) acc)
    Map.empty vars

class Freshenable t where
    freshen :: Ren -> t -> t

instance Freshenable (Expr.Meta l) where
    freshen ren (Expr.MV x) = Expr.MV (lookup' x ren)
    freshen ren (Expr.MInj l) = Expr.MInj l

-- TODO: I really want this definition, but then I get an overlapping instances issue
--instance (Functor f, Freshenable l) => Freshenable (f l) where
--    freshen ren x = map (freshen sub) x

-- TODO: so instead, I have it written manually in some specific cases

instance Freshenable l => Freshenable (Expr.Expr l) where
    freshen sub expr = map (freshen sub) expr

instance Freshenable l => Freshenable (Expr.ChangeLabel l) where
    freshen sub l = map (freshen sub) l

--------------------------------------------------------------------------------
-- !HENRY here's a way to get around this, but you still need to write instances
-- for deeply-nested type-class-instance-inferences

newtype AsFreshenable f (a :: Type) = AsFreshenable (f a)

derive instance Newtype (AsFreshenable f a) _
derive instance Functor f => Functor (AsFreshenable f)

instance (Functor f, Freshenable l) => Freshenable (AsFreshenable f l) where
    freshen rho = map (freshen rho) -- beautiful

freshen' :: forall f l. Functor f => Freshenable l => Ren -> f l -> f l
freshen' rho = AsFreshenable >>> freshen rho >>> Newtype.unwrap

--------------------------------------------------------------------------------

type Sub l = Expr.MetaVarSub (Expr.MetaExpr l)

-- sub2 after sub1
composeSub :: forall l. Expr.IsExprLabel l => Sub l -> Sub l -> Sub l
composeSub sub1 sub2 = union' (map (Expr.subMetaExprPartially sub2) sub1) sub2

composeSubs :: forall l f. Foldable.Foldable f => Expr.IsExprLabel l => f (Sub l) -> Sub l
composeSubs subs = foldl composeSub Map.empty subs

noMetaVars :: forall l. Expr.IsExprLabel l => String -> Expr.MetaExpr l -> Assertion (Expr.Expr l)
noMetaVars source mexpr0 = Assertion
    { name: "noMetaVars", source
    , result: do
        let go = assertInput_ (Expr.wellformedExpr "noMetaVars") \mexpr -> case mexpr of
                Expr.MV _ % [] -> throwError $ "Found MetaVar " <> quotes (pretty mexpr)
                Expr.MInj l % kids -> (l % _) <$> go `traverse` kids
        go mexpr0
    }

------------- Unification ------------------------------------------------------

-- we may need a more general notion of unification later, but this is ok for now
-- NOTE: should prefer substituting variables from the left if possible
unify :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> Maybe (Expr.MetaExpr l /\ Sub l)
unify e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) =
    case l1 /\ l2 of
        Expr.MV x1 /\ Expr.MV x2 | x1 == x2 -> Just (e1 /\ Map.empty)
        Expr.MV x /\ _ -> Just (e2 /\ Map.insert x e2 Map.empty)
        _ /\ Expr.MV x -> unify e2 e1
        Expr.MInj l /\ Expr.MInj l' | l == l' -> do
            kids' /\ sub <- unifyLists (List.fromFoldable kids1) (List.fromFoldable kids2)
            pure (Expr.Expr (Expr.MInj l) (Array.fromFoldable kids') /\ sub)
        _ /\ _ -> Nothing

unifyLists :: forall l. Expr.IsExprLabel l => List (Expr.MetaExpr l) -> List (Expr.MetaExpr l) -> Maybe (List (Expr.MetaExpr l) /\ Sub l)
unifyLists Nil Nil = Just (Nil /\ Map.empty)
unifyLists (e1 : es1) (e2 : es2) = do
    e /\ sub <- unify e1 e2
    let es1' = map (Expr.subMetaExprPartially sub) es1
    let es2' = map (Expr.subMetaExprPartially sub) es2
    es /\ sub2 <- unifyLists es1' es2'
    pure $ (e : es) /\ composeSub sub sub2
unifyLists _ _ = Bug.bug "[unifyLists] shouldn't happen"


------------- Another operation I need for typechanges stuff ------------------

getMatches :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.Expr l -> Maybe (MultiMap Expr.MetaVar (Expr.Expr l))
getMatches _e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) =
    case l1 of
        Expr.MV x -> Just $ MultiMap.insert x e2 (MultiMap.empty)
        Expr.MInj l | l == l2 -> foldl (lift2 MultiMap.union) (Just MultiMap.empty) (Array.zipWith getMatches kids1 kids2)
        _ ->
            Nothing

--getToothMatches :: forall l. Expr.IsExprLabel l => Expr.Tooth (Expr.Meta l) -> Expr.Tooth l
--    -> Maybe (MultiMap Expr.MetaVar (Expr.Expr l))
--getToothMatches t1 t2 = ?h
