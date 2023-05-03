module Language.Pantograph.STLC.Grammar where

import Language.Pantograph.Generic.Grammar as G

data Label

type Expr = G.Expr Label
type MetaExpr = G.MetaExpr Label

data RuleName = Lam | App | Z | S

type DerivLabel = G.DerivLabel Label RuleName

type DerivTerm = G.DerivTerm Label RuleName
type DerivPath = G.DerivPath Label RuleName

type Rule = G.Rule Label RuleName

rules :: Array Rule
rules = [

{-

Var G A
---------------------- suc
Var (G , T) A

----------------------- zero
Var (G , T) T

-}

]