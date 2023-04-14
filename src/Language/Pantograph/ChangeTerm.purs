module Language.Pantograph.ChangeTerm where

import Prelude
import Language.Pantograph.Grammar

data Context
data Change

type Expr2Expr = forall f . Expr f -> Expr f

chTerm :: Context -> Change -> Expr ~> Expr
chTerm = ?h
