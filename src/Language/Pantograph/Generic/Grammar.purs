module Language.Pantograph.Generic.Grammar where

import Data.Gram as Gram
import Data.Set (Set)
import Type.Direction as Dir

--import Data.Gram

--data Label

type Expr l = Gram.Expr l
type Change l = Gram.Change l
type MetaExpr l = Gram.MetaExpr l

--data RuleName = Lam | App | Z | S

data DerivLabel l r = DerivLabel r (MetaExpr l)

type DerivTerm l r = Gram.Expr (DerivLabel l r)
type DerivPath l r = Gram.Path Dir.Up (DerivLabel l r)

data Rule l r = Rule r (Set Gram.MetaVar) (Array (MetaExpr l)) (MetaExpr l)