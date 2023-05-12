module Language.Pantograph.ULC.Grammar where

import Data.Tuple.Nested ((/\))
import Prelude
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, (%))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, defaultLanguageChanges, makeRule)
import Language.Pantograph.Generic.Grammar as Grammar

--------------------------------------------------------------------------------
-- ExprLabel
--------------------------------------------------------------------------------

data ExprLabel
  = VarSort
  | TermSort
  | HoleInteriorSort

derive instance Generic ExprLabel _
instance Show ExprLabel where show x = genericShow x
instance Eq ExprLabel where eq x = genericEq x
instance Ord ExprLabel where compare x y = genericCompare x y

instance IsExprLabel ExprLabel where
  prettyExprF'_unsafe (VarSort /\ []) = "VarSort"
  prettyExprF'_unsafe (TermSort /\ []) = "TermSort"
  prettyExprF'_unsafe (HoleInteriorSort /\ []) = "HoleInteriorSort"

  expectedKidsCount VarSort = 0
  expectedKidsCount TermSort = 0
  expectedKidsCount HoleInteriorSort = 0

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

type Expr = Expr.Expr ExprLabel
type MetaExpr = Expr.MetaExpr ExprLabel
type Zipper = Expr.Zipper ExprLabel
type Tooth = Expr.Tooth ExprLabel

varSortE :: Expr
varSortE = VarSort % []
termSortE :: Expr
termSortE = TermSort % []
holeInteriorSortE :: Expr
holeInteriorSortE = HoleInteriorSort % []

varSortME = pure VarSort % [] :: MetaExpr
termSortME = pure TermSort % [] :: MetaExpr
holeInteriorSortME = pure HoleInteriorSort % [] :: MetaExpr

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

-- | Naming convention: <title>_<output sort>
data RuleLabel
  = Zero
  | Suc
  | Lam
  | App
  | Ref
  | Hole
  | HoleInterior

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where show x = genericShow x
instance Enum RuleLabel where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded RuleLabel where
  bottom = genericBottom
  top = genericTop
instance IsRuleLabel RuleLabel

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language ExprLabel RuleLabel
type Rule = Grammar.Rule ExprLabel

language :: Language
language = TotalMap.makeTotalMap case _ of
  Zero -> makeRule [] \[] ->
    [ ]
    /\ --------
    ( varSortME )
  Suc -> makeRule [] \[] ->
    [ varSortME ]
    /\ --------
    ( varSortME )
  Lam -> makeRule [] \[] ->
    [ varSortME
    , termSortME ]
    /\ --------
    ( termSortME )
  App -> makeRule [] \[] ->
    [ termSortME
    , termSortME ]
    /\ --------
    ( termSortME )
  Ref -> makeRule [] \[] ->
    [ varSortME ]
    /\ --------
    ( termSortME )
  Hole -> makeRule ["sort"] \[sort] ->
    [ holeInteriorSortME ]
    /\ --------
    ( sort )
  HoleInterior -> makeRule [] \[] ->
    [ ]
    /\ --------
    ( holeInteriorSortME )

--------------------------------------------------------------------------------
-- DerivExpr (and friends)
--------------------------------------------------------------------------------

type DerivExpr = Grammar.DerivExpr ExprLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir ExprLabel RuleLabel
type DerivZipper = Grammar.DerivZipper ExprLabel RuleLabel
type DerivZipper' = Grammar.DerivZipper' ExprLabel RuleLabel

-- var
zeroDE :: DerivExpr
zeroDE = Grammar.DerivLabel Zero varSortME % []
sucDE :: DerivExpr -> DerivExpr
sucDE var = Grammar.DerivLabel Suc varSortME % [var]
-- term
refDE :: DerivExpr -> DerivExpr
refDE var = Grammar.DerivLabel Ref termSortME % [var]
lamDE :: DerivExpr -> DerivExpr -> DerivExpr
lamDE var bod = Grammar.DerivLabel Lam termSortME % [var, bod]
appDE :: DerivExpr -> DerivExpr -> DerivExpr
appDE apl arg = Grammar.DerivLabel App termSortME % [apl, arg]
-- hole
holeDE :: DerivExpr -> MetaExpr -> DerivExpr
holeDE interior sort = Grammar.DerivLabel Hole sort % [interior]
-- hole interior
holeInteriorDE :: DerivExpr
holeInteriorDE = Grammar.DerivLabel HoleInterior holeInteriorSortME % []

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges ExprLabel RuleLabel
type ChangeRule = Grammar.ChangeRule ExprLabel

-- !TODO special cases
languageChanges :: LanguageChanges
languageChanges = defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  Zero -> identity
  Suc -> identity
  Lam -> identity
  App -> identity
  Ref -> identity
  Hole -> identity
  HoleInterior -> identity
