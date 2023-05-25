module Language.Pantograph.ULC.Grammar where

import Prelude
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
import Language.Pantograph.Generic.Grammar ((%|-))
import Language.Pantograph.Generic.Grammar as Grammar
import Text.Pretty (class Pretty, (<+>))
import Text.Pretty as P

--------------------------------------------------------------------------------
-- SortLabel
--------------------------------------------------------------------------------

data SortLabel
  = VarSort
  | TermSort

derive instance Generic SortLabel _
instance Show SortLabel where show x = genericShow x
instance Eq SortLabel where eq x = genericEq x
instance Ord SortLabel where compare x y = genericCompare x y

instance Pretty SortLabel where
  pretty VarSort = "Var"
  pretty TermSort = "Term"


instance IsExprLabel SortLabel where
  prettyExprF'_unsafe (VarSort /\ _) = "Var"
  prettyExprF'_unsafe (TermSort /\ _) = "Term"

  expectedKidsCount VarSort = 0
  expectedKidsCount TermSort = 0

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

type Expr = Expr.Expr SortLabel
type MetaExpr = Expr.MetaExpr SortLabel
type Zipper = Expr.Zipper SortLabel
type Tooth = Expr.Tooth SortLabel
type Sort = Grammar.Sort SortLabel

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
  pretty Hole = "?"

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language SortLabel RuleLabel
type Rule = Grammar.Rule SortLabel

instance Grammar.IsRuleLabel SortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = "Z"
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = "S" <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [b]) = P.parens $ "λ" <+> b
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (Hole /\ []) = "?"

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    Hole -> true
    _ -> false

  defaultDerivTerm' sort = pure $ (Hole %|- sort) % []

language :: Language
language = TotalMap.makeTotalMap case _ of
  Zero -> Grammar.makeRule [] \[] ->
    [ ]
    /\ --------
    ( Grammar.InjectSortLabel VarSort %* [] )
  Suc -> Grammar.makeRule [] \[] ->
    [ Grammar.InjectSortLabel VarSort %* [] ]
    /\ --------
    ( Grammar.InjectSortLabel VarSort %* [] )
  Lam -> Grammar.makeRule [] \[] ->
    [ Grammar.InjectSortLabel TermSort %* [] ]
    /\ --------
    ( Grammar.InjectSortLabel TermSort %* [] )
  App -> Grammar.makeRule [] \[] ->
    [ Grammar.InjectSortLabel TermSort %* []
    , Grammar.InjectSortLabel TermSort %* [] ]
    /\ --------
    ( Grammar.InjectSortLabel TermSort %* [] )
  Ref -> Grammar.makeRule [] \[] ->
    [ Grammar.InjectSortLabel VarSort %* [] ]
    /\ --------
    ( Grammar.InjectSortLabel TermSort %* [] )
  Hole -> Grammar.makeRule ["sort"] \[sort] ->
    [ ]
    /\ --------
    ( sort )

--------------------------------------------------------------------------------
-- DerivTerm (and friends)
--------------------------------------------------------------------------------

type DerivTerm = Grammar.DerivTerm SortLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir SortLabel RuleLabel
type DerivZipper = Grammar.DerivZipper SortLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp SortLabel RuleLabel

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges SortLabel RuleLabel
type ChangeRule = Grammar.ChangeRule SortLabel

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

