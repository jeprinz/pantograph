module Language.Pantograph.Generic.Grammar where

import Data.Eq
import Data.Ord
import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Enum (class Enum)
import Data.Expr (class IsExprExprLabel, expectedKidsCount)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Text.Pretty (class Pretty, pretty)
import Type.Direction as Dir
import Language.Pantograph.Generic.ChangeAlgebra

--data ExprLabel

--data RuleLabel = Lam | App | Z | S

class (Enum r, Bounded r, Show r) <= IsDerivRuleLabel r

data DerivExprLabel l r = DerivExprLabel r (Expr.MetaExpr l)
derive instance Generic (DerivExprLabel l r) _
instance (Show l, Show r) => Show (DerivExprLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivExprLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivExprLabel l r)

instance (IsExprExprLabel l, IsDerivRuleLabel r) => IsExprExprLabel (DerivExprLabel l r) where
  -- This implementation ignores the rule and metaexpression, but maybe we want
  -- to print those at some point for debugging?
  prettyExprF'_unsafe (DerivExprLabel _r (Expr.Expr l _metaExpr) /\ kids) = 
    Expr.prettyExprF (l /\ kids)

  expectedKidsCount (DerivExprLabel _r (Expr.Expr l _)) = expectedKidsCount l

{-
--DerivTerm needs built-in hole?
--DerivTerm and DerivPath need boundaries?
-}
type DerivTerm l r = Expr.Expr (DerivExprLabel l r)
-- !HENRY: personally, I think it's better to keep the name `Expr` consistent, 
-- rather than conflate generic "terms" and "terms" in a particular language 
-- (e.g. as in "terms and their types")
type DerivExpr l r = Expr.Expr (DerivExprLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivExprLabel l r)
type DerivZipper l r = Expr.Zipper (DerivExprLabel l r)
type DerivZipper' l r = Expr.Zipper' (DerivExprLabel l r)

data Rule l = Rule (Set Expr.MetaVar) (Array (Expr.MetaExpr l)) (Expr.MetaExpr l)

type Language l r = Map r (Rule l)

data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (Expr.MetaChange l)) -- changes go from parent to kid
type LanguageChanges l r = Map r (ChangeRule l)

autoDeriveLanguageChanges :: forall l r. Eq l => Language l r -> LanguageChanges l r
autoDeriveLanguageChanges = map (\(Rule vars kids parent) -> ChangeRule vars (map (diff parent) kids))

--derive instance (Eq l, Eq r) => Eq (DerivExprLabel l r)
--derive instance (Ord l, Ord r) => Ord (DerivExprLabel l r)
----derive instance Functor j => Functor (Expr j)
