module Language.Pantograph.GenericExpr.Unification where

import Prelude
import Language.Pantograph.GenericExpr.Expression
import Data.Map (Map)
import Data.Map as Map
import Data.List (List(..), (:))
import Data.List as List
import Data.UUID (UUID)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Exception.Unsafe (unsafeThrow)
import Util (union')


type Sub ExprLabel = Map UUID (ExprWM ExprLabel)


subExpr :: forall l . Sub l -> ExprWM l -> ExprWM l
subExpr s ex@(Expr (EMetaVar x) Nil) = case Map.lookup x s of
    Nothing -> ex
    Just ex' -> ex'
subExpr s (Expr ExprLabel kids) = Expr ExprLabel ((subExpr s) <$> kids)

-- Its too annoying to do wrapping and unwrapping so its not explicitly a functor
mapSub :: forall ExprLabel1 ExprLabel2 . (ExprLabel1 -> ExprLabel2) -> Sub ExprLabel1 -> Sub ExprLabel2
mapSub f s = map (map (map f)) s -- truly one of the lines of code of all time

--data Expr ExprLabel = ExprWM ExprLabel (Array (Expr ExprLabel)) | EMetaVar UUID
unify :: forall l . Eq l => ExprWM l -> ExprWM l -> Maybe (ExprWM l /\ Sub l)
unify (Expr (EMetaVar x) Nil) e = Just $ e /\ Map.insert x e Map.empty
unify e1 e2@(Expr (EMetaVar x) Nil) = unify e2 e1
unify (Expr ExprLabel1 kids1) (Expr ExprLabel2 kids2) =
    if not (ExprLabel1 == ExprLabel2) then Nothing else do
    kids /\ sub <- unifyExprs kids1 kids2
    pure $ Expr ExprLabel1 kids /\ sub

unifyExprs :: forall l . Eq l => List (ExprWM l) -> List (ExprWM l) -> Maybe (List (ExprWM l) /\ Sub l)
unifyExprs Nil Nil = Just $ Nil /\ Map.empty
unifyExprs (e1 : es1) (e2 : es2) = do
    e' /\ sub1 <- unify e1 e2
    let es1' = map (subExpr sub1) es1
    let es2' = map (subExpr sub1) es2
    es' /\ sub2 <- unifyExprs es1' es2'
    pure $ ((subExpr sub2 e') : es') /\ (union' sub1 sub2)
unifyExprs _ _ = unsafeThrow "kids had different lengths even though ExprLabel was the same in unifyExprs"
