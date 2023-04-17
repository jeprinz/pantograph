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


type Sub label = Map UUID (Expr label)


subExpr :: forall l . Sub l -> Expr l -> Expr l
subExpr s ex@(EMetaVar x) = case Map.lookup x s of
    Nothing -> ex
    Just ex' -> ex'
subExpr s (Expr label kids) = Expr label ((subExpr s) <$> kids)

-- Its too annoying to do wrapping and unwrapping so its not explicitly a functor
mapSub :: forall label1 label2 . (label1 -> label2) -> Sub label1 -> Sub label2
mapSub f s = map (map f) s

--data Expr label = ExprWM label (Array (Expr label)) | EMetaVar UUID
unifyExpr :: forall l . Ord l => Expr l -> Expr l -> Maybe (Expr l /\ Sub l)
unifyExpr (EMetaVar x) e = Just $ e /\ Map.insert x e Map.empty
unifyExpr e1 e2@(EMetaVar x) = unifyExpr e2 e1
unifyExpr (Expr label1 kids1) (Expr label2 kids2) =
    if not (label1 == label2) then Nothing else do
    kids /\ sub <- unifyExprs kids1 kids2
    pure $ Expr label1 kids /\ sub

unifyExprs :: forall l . Ord l => List (Expr l) -> List (Expr l) -> Maybe (List (Expr l) /\ Sub l)
unifyExprs Nil Nil = Just $ Nil /\ Map.empty
unifyExprs (e1 : es1) (e2 : es2) = do
    e' /\ sub1 <- unifyExpr e1 e2
    let es1' = map (subExpr sub1) es1
    let es2' = map (subExpr sub1) es2
    es' /\ sub2 <- unifyExprs es1' es2'
    pure $ (e' : es') /\ (union' sub1 sub2)
unifyExprs _ _ = unsafeThrow "kids had different lengths even though label was the same in unifyExprs"
