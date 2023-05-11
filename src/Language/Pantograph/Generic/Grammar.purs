module Language.Pantograph.Generic.Grammar where

import Data.Eq
import Data.Ord
import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Expr (MetaVar(..))
import Data.Expr as Expr
import Data.Map (Map)
import Data.Set (Set)
import Text.Pretty (class Pretty, pretty)
import Type.Direction as Dir

--data Label

type Expr l = Expr.Expr l
type Change l = Expr.Change l
type MetaExpr l = Expr.MetaExpr l
type MetaChange l = Expr.MetaChange l

--data RuleName = Lam | App | Z | S

data DerivLabel l r = DerivLabel r (MetaExpr l)
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

-- instance (ExprLabel l, Pretty r) => ExprLabel (DerivLabel l r) where
--   -- This implementation ignores the rule and metaexpression, but maybe we want
--   -- to print those at some point for debugging?
--   prettyNodeG (DerivLabel _r (Expr (l /\ _metaExpr)) /\ kids) = prettyNodeG (l /\ kids)

{-
--DerivTerm needs built-in hole?
--DerivTerm and DerivPath need boundaries?

-}
type DerivTerm l r = Expr.Expr (DerivLabel l r)
type DerivPath l r = Expr.Path Dir.Up (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)

data Rule l r = Rule (Set MetaVar) (Array (MetaExpr l)) (MetaExpr l)

type Language l r = Map r (Rule l r)

--derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
--derive instance (Ord l, Ord r) => Ord (DerivLabel l r)
----derive instance Functor j => Functor (Expr j)
