module Language.Pantograph.Generic.Grammar where

import Data.Eq
import Data.Ord
import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Enum (class Enum)
import Data.Expr (class ExprLabel, expectedKidsCount)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Text.Pretty (class Pretty, pretty)
import Type.Direction as Dir

--data Label

--data RuleName = Lam | App | Z | S

class (Enum r, Bounded r, Show r) <= DerivRuleName r

data DerivLabel l r = DerivLabel r (Expr.MetaExpr l)
derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance (ExprLabel l, Eq r, Ord r, Show r) => ExprLabel (DerivLabel l r) where
  -- This implementation ignores the rule and metaexpression, but maybe we want
  -- to print those at some point for debugging?
  prettyExprF'_unsafe (DerivLabel _r (Expr.Expr l _metaExpr) /\ kids) = Expr.prettyExprF (l /\ kids)

  expectedKidsCount (DerivLabel _r (Expr.Expr l _)) = expectedKidsCount l

{-
--DerivTerm needs built-in hole?
--DerivTerm and DerivPath need boundaries?
-}
type DerivTerm l r = Expr.Expr (DerivLabel l r)
type DerivExpr l r = Expr.Expr (DerivLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipper' l r = Expr.Zipper' (DerivLabel l r)

data Rule l r = Rule (Set Expr.MetaVar) (Array (Expr.MetaExpr l)) (Expr.MetaExpr l)

type Language l r = Map r (Rule l r)

--derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
--derive instance (Ord l, Ord r) => Ord (DerivLabel l r)
----derive instance Functor j => Functor (Expr j)
