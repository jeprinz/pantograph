module Language.Pantograph.SULC.Grammar where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
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
import Hole (hole)
import Language.Pantograph.Generic.Grammar ((%|-), (%|-*))
import Language.Pantograph.Generic.Grammar as Grammar
import Text.Pretty (class Pretty, (<+>))
import Text.Pretty as P

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

data PreSortLabel
  = VarSort {-Ctx-} {-String-}
  | TermSort {-Ctx-}
  | CtxConsSort {-String-} {-Ctx-}
  | CtxNilSort

derive instance Generic PreSortLabel _
instance Show PreSortLabel where show x = genericShow x
instance Eq PreSortLabel where eq x = genericEq x
instance Ord PreSortLabel where compare x y = genericCompare x y

instance Pretty PreSortLabel where
  pretty VarSort = "Var"
  pretty TermSort = "Term"
  pretty CtxConsSort = "CtxCons"
  pretty CtxNilSort = "CtxNil"

instance IsExprLabel PreSortLabel where
  prettyExprF'_unsafe (VarSort /\ [gamma, x]) = "Var(" <> x <> ")"
  prettyExprF'_unsafe (TermSort /\ [gamma]) = "Term"
  prettyExprF'_unsafe (CtxConsSort /\ [x, gamma]) = x <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"

  expectedKidsCount VarSort = 2
  expectedKidsCount TermSort = 1
  expectedKidsCount CtxConsSort = 2
  expectedKidsCount CtxNilSort = 0

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

type Expr = Expr.Expr PreSortLabel
type MetaExpr = Expr.MetaExpr PreSortLabel
type Zipper = Expr.Zipper PreSortLabel
type Tooth = Expr.Tooth PreSortLabel
type Sort = Grammar.Sort PreSortLabel

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
  | TermHole

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
  pretty TermHole = "?"

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language PreSortLabel RuleLabel
type Rule = Grammar.Rule PreSortLabel

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = "Z"
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = "S" <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [b]) = P.parens $ "λ" <+> b
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ []) = "?"

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    _ -> false

  defaultDerivTerm' ctx@(Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % [gamma]) = (TermHole %|- ctx) % []
  defaultDerivTerm' (Expr.Meta (Right Grammar.NameSortLabel) % []) = Grammar.DerivString "" % []

ctxCons x gamma = CtxConsSort %|-* [x, gamma]
infixl 7 ctxCons as %:

language :: Language
language = TotalMap.makeTotalMap case _ of

  Zero -> Grammar.makeRule ["gamma", "x"] \[gamma, x] ->
    []
    /\ --------
    ( VarSort %|-* [x %: gamma, x] )

  Suc -> Grammar.makeRule ["gamma", "x", "y"] \[gamma, x, y] ->
    [ VarSort %|-* [gamma, x] ]
    /\ --------
    ( VarSort %|-* [(y %: gamma), x] )

  Lam -> Grammar.makeRule ["gamma", "x"] \[gamma, x] ->
    [ Grammar.NameSortLabel %* [x]
    , TermSort %|-* [x %: gamma] ]
    /\ --------
    ( TermSort %|-* [gamma])

  App -> Grammar.makeRule ["gamma"] \[gamma] ->
    [ TermSort %|-* [gamma]
    , TermSort %|-* [gamma] ]
    /\ --------
    ( TermSort %|-* [gamma] )

  Ref -> Grammar.makeRule ["gamma", "x"] \[gamma, x] -> 
    [ VarSort %|-* [gamma, x] ]
    /\ --------
    ( TermSort %|-* [gamma] )

  TermHole -> Grammar.makeRule ["gamm"] \[gamma] ->
    [ ]
    /\ --------
    ( TermSort %|-* [gamma] )

--------------------------------------------------------------------------------
-- DerivTerm (and friends)
--------------------------------------------------------------------------------

type DerivTerm = Grammar.DerivTerm PreSortLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir PreSortLabel RuleLabel
type DerivZipper = Grammar.DerivZipper PreSortLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp PreSortLabel RuleLabel

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges PreSortLabel RuleLabel
type ChangeRule = Grammar.ChangeRule PreSortLabel

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

