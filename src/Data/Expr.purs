module Data.Expr
  ( BindInfo
  , class Expr
  , exprBindInfo
  , varExpr
  , mapExpr
  , joinExpr
  , substExpr
  ) where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Partial.Unsafe (unsafeCrashWith)

data BindInfo x
  = BindInfo
    { binds :: Set.Set x
    , refs :: Set.Set x
    }

emptyBindInfo :: forall x. BindInfo x
emptyBindInfo = BindInfo { binds: Set.empty, refs: Set.empty }

class Expr e where
  exprBindInfo :: forall x. Ord x => e x -> BindInfo x
  varExpr :: forall x. x -> e x
  mapExpr :: forall x y. Ord x => (Map.Map x y -> x -> y) -> e x -> e y
  joinExpr :: forall x. e (e x) -> e x

substExpr :: forall e x y. Expr e => Ord x => (Map.Map x (e y) -> x -> e y) -> e x -> e y
substExpr f = joinExpr <<< mapExpr f

-- | Example instance of `Expr`
data Term x
  = Var x
  | Lam x (Term x)
  | App (Term x) (Term x)

instance exprTerm :: Expr Term where
  exprBindInfo (Var x) = BindInfo { binds: Set.empty, refs: Set.fromFoldable [ x ] }
  exprBindInfo (Lam x t) = BindInfo { binds: Set.fromFoldable [ x ], refs: Set.empty }
  exprBindInfo (App _ _) = emptyBindInfo
  varExpr = Var
  mapExpr = unsafeCrashWith "mapExpr"
  joinExpr = unsafeCrashWith "joinExpr"
