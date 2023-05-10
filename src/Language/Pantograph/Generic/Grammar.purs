module Language.Pantograph.Generic.Grammar where

import Data.Eq
import Data.Ord
import Data.Tuple
import Data.Tuple.Nested
import Data.Unify
import Prelude

import Data.Gram (class GramLabel, Gram(..), prettyExpr, prettyNodeG)
import Data.Gram as Gram
import Data.Map (Map)
import Data.Set (Set)
import Data.Unify (MetaVar(..))
import Text.Pretty (class Pretty, pretty)
import Type.Direction as Dir

--import Data.Gram

--data Label

type Expr l = Gram.Expr l
type Change l = Gram.Change l
type MetaExpr l = Gram.MetaExpr l
type MetaChange l = Gram.MetaChange l

--data RuleName = Lam | App | Z | S

data DerivLabel l r = DerivLabel r (MetaExpr l)
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

-- instance (GramLabel l, Pretty r) => GramLabel (DerivLabel l r) where
--   -- This implementation ignores the rule and metaexpression, but maybe we want
--   -- to print those at some point for debugging?
--   prettyNodeG (DerivLabel _r (Gram (l /\ _metaExpr)) /\ kids) = prettyNodeG (l /\ kids)

{-
--DerivTerm needs built-in hole?
--DerivTerm and DerivPath need boundaries?

-}
type DerivTerm l r = Gram.Expr (DerivLabel l r)
type DerivPath l r = Gram.Path Dir.Up (DerivLabel l r)
type DerivZipper l r = Gram.Zipper (DerivLabel l r)

data Rule l r = Rule (Set MetaVar) (Array (MetaExpr l)) (MetaExpr l)

type Language l r = Map r (Rule l r)

--derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
--derive instance (Ord l, Ord r) => Ord (DerivLabel l r)
----derive instance Functor j => Functor (Gram j)
