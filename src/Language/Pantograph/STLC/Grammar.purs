module Language.Pantograph.STLC.Grammar where

import Prelude
import Language.Pantograph.Generic.Grammar as G
import Data.Set (Set)
import Data.Set as Set
--import Data.Gram

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

]