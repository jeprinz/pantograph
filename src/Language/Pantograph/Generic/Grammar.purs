module Language.Pantograph.Generic.Grammar where

import Data.Gram as Gram
import Data.Set (Set)
import Type.Direction as Dir
import Data.Map (Map)
import Data.Ord
import Data.Eq

--import Data.Gram

--data Label

type Expr l = Gram.Expr l
type Change l = Gram.Change l
type MetaExpr l = Gram.MetaExpr l

--data RuleName = Lam | App | Z | S

data DerivLabel l r = DerivLabel r (MetaExpr l)

type DerivTerm l r = Gram.Expr (DerivLabel l r)
type DerivPath l r = Gram.Path Dir.Up (DerivLabel l r)

data Rule l r = Rule (Set Gram.MetaVar) (Array (MetaExpr l)) (MetaExpr l)

type Language l r = Map r (Rule l r)

derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)
----derive instance Functor j => Functor (Gram j)

data Test a = Bla

derive instance Eq a => Eq (Test a)
derive instance Ord a => Ord (Test a)
