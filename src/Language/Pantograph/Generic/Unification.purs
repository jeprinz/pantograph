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
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Pretty (pretty, quotes)
import Util (lookup', union')
import Debug (trace)
import Debug (traceM)
import Data.Traversable (sequence)
import Data.List.Zip as ZipList
import Hole (hole)
import Control.Monad.State (State)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Tuple (fst, snd)

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

occurs :: forall l. Expr.IsExprLabel l => Expr.MetaVar -> Expr.MetaExpr l -> Boolean
occurs x e =
    case e of
        Expr.MV x' % [] -> x' == x
        _ % kids -> Array.any (occurs x) kids

-- we may need a more general notion of unification later, but this is ok for now
-- NOTE: should prefer substituting variables from the left if possible
unify :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> Maybe (Expr.MetaExpr l /\ Sub l)
unify e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) =
    case l1 /\ l2 of
        Expr.MV x1 /\ Expr.MV x2 | x1 == x2 -> Just (e1 /\ Map.empty)
        Expr.MV x /\ _ | not (occurs x e2) -> Just (e2 /\ Map.insert x e2 Map.empty)
        _ /\ Expr.MV x -> unify e2 e1
        Expr.MInj l /\ Expr.MInj l' | l == l' -> do
            kids' /\ sub <- unifyLists (List.fromFoldable kids1) (List.fromFoldable kids2)
            pure (Expr.Expr (Expr.MInj l) (Array.fromFoldable kids') /\ sub)
        _ /\ _ -> Nothing

-- TODO: A really simple optimization is to unifyp the rest of the list first and only apply the subs to e1 and e2
unifyLists :: forall l. Expr.IsExprLabel l => List (Expr.MetaExpr l) -> List (Expr.MetaExpr l) -> Maybe (List (Expr.MetaExpr l) /\ Sub l)
unifyLists Nil Nil = Just (Nil /\ Map.empty)
unifyLists (e1 : es1) (e2 : es2) = do
--    e /\ sub <- unify e1 e2
--    let es1' = map (Expr.subMetaExprPartially sub) es1
--    let es2' = map (Expr.subMetaExprPartially sub) es2
--    es /\ sub2 <- unifyLists es1' es2'
--    pure $ (e : es) /\ composeSub sub sub2
    es /\ sub <- unifyLists es1 es2
    let e1' = Expr.subMetaExprPartially sub e1
    let e2' = Expr.subMetaExprPartially sub e2
    e /\ sub2 <- unify e1' e2'
    pure $ (e : es) /\ composeSub sub sub2
unifyLists _ _ = Bug.bug "[unifyLists] shouldn't happen"

------------- Fast Unification ------------------------------------------------------

{-
This version of unification is faster than the naive algorithm avoe.
It turns out that the fastest algorithms are very complex, but this is at least faster.
It also has the advantage that it can be composed conveniently in a monadic style.
-}

flattenHelperInsertVar :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaVar -> State (Sub l) (Expr.MetaExpr l)
flattenHelperInsertVar original x = do
    sub <- State.get
    case Map.lookup x sub of
        Just t -> pure t -- If var is already in the new sub, do nothing
        Nothing -> do -- Otherwise, if its in the original sub at all, then add it the output sub
            value <- case Map.lookup x original of
                    Nothing -> pure $ Expr.MV x % []
                    Just t -> do
                        value <- flattenHelper original t
                        _ <- State.modify (Map.insert x value)
                        pure value
            pure value

flattenHelper :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaExpr l -> State (Sub l) (Expr.MetaExpr l)
flattenHelper original {-x-} (Expr.MV x % _) = flattenHelperInsertVar original x
flattenHelper original {-x-} (l % kids) = do
    kids' <- sequence $ map (flattenHelper original) kids
    pure $ l % kids'

-- The input is a non-idempotent substitution, and the output in the State is an idempotent subsitution
flattenSubImpl :: forall l. Expr.IsExprLabel l => Sub l -> State (Sub l) Unit
flattenSubImpl original =  do
--    traceM ("in flattenSubImpl, original is: " <> pretty original)
    _ <- sequence (map (\(key /\ _) -> flattenHelperInsertVar original key) (Map.toUnfoldable original :: List _))
--    result <- State.get
--    traceM ("in flattenSubImpl, result is: " <> pretty result)
    pure unit

-- makes a substitution idempotent.
flattenSub :: forall l. Expr.IsExprLabel l => Sub l -> Sub l
flattenSub sub = snd $ State.runState (flattenSubImpl sub) Map.empty

-- occurs check with a non-idempotent sub as an enviroment
recursiveOccurs :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaVar -> Expr.MetaExpr l -> Boolean
recursiveOccurs sub x e =
    case e of
        Expr.MV y % [] | Just e' <- Map.lookup y sub -> recursiveOccurs sub x e'
        Expr.MV x' % [] -> x' == x
        _ % kids -> Array.any (occurs x) kids

-- NOTE: it may be confusing that the State in unifyFImpl is a completely different thing to the State in flatten*
-- The (Sub l) in the State is the non-idempotent substitution being built up
unifyFImpl :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> ExceptT Unit (State (Sub l)) (Expr.MetaExpr l)
unifyFImpl e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) = do
    sub <- State.get
    case l1 /\ l2 of
        Expr.MV x /\ _ | Just e1' <- Map.lookup x sub -> unifyFImpl e1' e2
        _ /\ Expr.MV x | Just e2' <- Map.lookup x sub -> unifyFImpl e1 e2'
        Expr.MV x1 /\ Expr.MV x2 | x1 == x2 -> pure e1
        Expr.MV x /\ _ | not (recursiveOccurs sub x e2) -> do
            _ <- lift $ State.modify (Map.insert x e2)
            pure e2
        _ /\ Expr.MV _ -> unifyFImpl e2 e1
        Expr.MInj l /\ Expr.MInj l' | l == l' -> do
            kids' <- sequence $ Array.zipWith unifyFImpl kids1 kids2
            pure ((Expr.MInj l) % kids')
        _ /\ _ -> throwError unit

unifyF :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> Maybe (Expr.MetaExpr l /\ Sub l)
unifyF e1 e2 =
    let maybeExpr /\ sub = State.runState (runExceptT (unifyFImpl e1 e2)) Map.empty in
    case maybeExpr of
        Left _ -> Nothing
        Right expr ->
            let flatSub = flattenSub sub in
            Just (Expr.subMetaExprPartially flatSub expr /\ flatSub)

runUnifyMonad :: forall l b. Expr.IsExprLabel l => ExceptT Unit (State (Sub l)) b -> Maybe (Sub l /\ b)
runUnifyMonad m =
    let maybeExpr /\ sub = State.runState (runExceptT m) Map.empty in
    case maybeExpr of
        Left _ -> Nothing
        Right x -> Just (flattenSub sub /\ x)

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
