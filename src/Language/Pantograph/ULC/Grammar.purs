module Language.Pantograph.ULC.Grammar where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, prettyExprF'_unsafe, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
import Hole as Hole
import Language.Pantograph.Generic.Grammar ((|-))
import Language.Pantograph.Generic.Grammar as Grammar
import Text.Pretty (class Pretty, (<+>))
import Text.Pretty as P

--------------------------------------------------------------------------------
-- ExprLabel
--------------------------------------------------------------------------------

data ExprLabel -- TODO: rename to SortLabel
  = VarSort
  | TermSort
  -- | HoleInteriorSort

derive instance Generic ExprLabel _
instance Show ExprLabel where show x = genericShow x
instance Eq ExprLabel where eq x = genericEq x
instance Ord ExprLabel where compare x y = genericCompare x y

instance Pretty ExprLabel where
  pretty VarSort = "var-sort"
  pretty TermSort = "term-sort"
  -- pretty HoleInteriorSort = "hole-interior-sort"


instance IsExprLabel ExprLabel where
  prettyExprF'_unsafe (VarSort /\ _) = "Var"
  prettyExprF'_unsafe (TermSort /\ _) = "Term"
  -- prettyExprF'_unsafe (HoleInteriorSort /\ _) = "HoleInterior"

  expectedKidsCount VarSort = 0
  expectedKidsCount TermSort = 0
  -- expectedKidsCount HoleInteriorSort = 0

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

type Expr = Expr.Expr ExprLabel
type MetaExpr = Expr.MetaExpr ExprLabel
type Zipper = Expr.Zipper ExprLabel
type Tooth = Expr.Tooth ExprLabel

-- varSortE :: Expr
-- varSortE = VarSort % []
-- termSortE :: Expr
-- termSortE = TermSort % []
-- holeInteriorSortE :: Expr
-- holeInteriorSortE = HoleInteriorSort % []

-- varSortME :: MetaExpr
-- varSortME = pure VarSort % []
-- termSortME :: MetaExpr
-- termSortME = pure TermSort % []
-- holeInteriorSortME :: MetaExpr
-- holeInteriorSortME = pure HoleInteriorSort % []

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
  -- | Hole
  -- | HoleInterior

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

instance Pretty RuleLabel where
  pretty Zero = "z"
  pretty Suc = "s"
  pretty Lam = "lam"
  pretty App = "app"
  pretty Ref = "ref"
  -- pretty Hole = "hole"
  -- pretty HoleInterior = "hole-interior"

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language ExprLabel RuleLabel
type Rule = Grammar.Rule ExprLabel

instance Grammar.IsRuleLabel ExprLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = "Z"
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = "S" <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, b]) = P.parens $ "λ" <+> x <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  -- prettyExprF'_unsafe_RuleLabel (Hole /\ [hi]) = "Hole[" <> hi <> "]"
  -- prettyExprF'_unsafe_RuleLabel (HoleInterior /\ []) = "?"

  language = language

language :: Language
language = TotalMap.makeTotalMap case _ of
  Zero -> Grammar.makeRule [] \[] ->
    [ ]
    /\ --------
    ( VarSort %* [] )
  Suc -> Grammar.makeRule [] \[] ->
    [ VarSort %* [] ]
    /\ --------
    ( VarSort %* [] )
  Lam -> Grammar.makeRule [] \[] ->
    [ VarSort %* [] -- instead, s
    , TermSort %* [] ]
    /\ --------
    ( TermSort %* [] )
  App -> Grammar.makeRule [] \[] ->
    [ TermSort %* []
    , TermSort %* [] ]
    /\ --------
    ( TermSort %* [] )
  Ref -> Grammar.makeRule [] \[] ->
    [ VarSort %* [] ]
    /\ --------
    ( TermSort %* [] )
  -- Hole -> Grammar.makeRule ["sort"] \[sort] ->
  --   [ Hole.hole "holeInteriorSortME" ]
  --   /\ --------
  --   ( sort )
  -- HoleInterior -> Grammar.makeRule [] \[] ->
  --   [ ]
  --   /\ --------
  --   ( Hole.hole "holeInteriorSortME" )

--------------------------------------------------------------------------------
-- DerivTerm (and friends)
--------------------------------------------------------------------------------

type DerivTerm = Grammar.DerivTerm ExprLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir ExprLabel RuleLabel
type DerivZipper = Grammar.DerivZipper ExprLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp ExprLabel RuleLabel

-- -- var
-- zeroDE :: DerivTerm
-- zeroDE = Zero |- varSortME % []
-- sucDE :: DerivTerm -> DerivTerm
-- sucDE var = Suc |- varSortME % [var]
-- -- term
-- refDE :: DerivTerm -> DerivTerm
-- refDE var = Ref |- termSortME % [var]
-- lamDE :: DerivTerm -> DerivTerm -> DerivTerm
-- lamDE var bod = Lam |- termSortME % [var, bod]
-- appDE :: DerivTerm -> DerivTerm -> DerivTerm
-- appDE apl arg = App |- termSortME % [apl, arg]
-- -- hole
-- holeDE :: DerivTerm -> MetaExpr -> DerivTerm
-- holeDE interior sort = Hole |- sort % [interior]
-- -- hole interior
-- holeInteriorDE :: DerivTerm
-- holeInteriorDE = HoleInterior |- holeInteriorSortME % []

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges ExprLabel RuleLabel
type ChangeRule = Grammar.ChangeRule ExprLabel

-- !TODO special cases
languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  Zero -> identity
  Suc -> identity
  Lam -> identity
  App -> identity
  Ref -> identity
  -- Hole -> identity
  -- HoleInterior -> identity

