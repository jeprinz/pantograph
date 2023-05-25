module Language.Pantograph.STLC.Grammar where

import Data.Tuple.Nested
import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, Meta(..), MetaVar(..), freshMetaVar', prettyExprF'_unsafe)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Unit (unit)
import Hole (hole)
import Hole as Hole
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, defaultDerivTerm, isHoleRuleTotalMap)
import Language.Pantograph.Generic.Grammar as G
import Text.Pretty (class Pretty, (<+>))
import Text.Pretty as P

data ExprLabel =
    -- sorts
    STerm {-context-} {-type-} | SVar {-context-} {-type-} | SType {-type-}
    -- TODO: add a sort for names, and place an element of SName as a child of let and lambda, and make a Name rule
    -- contexts
    | CCons {-context-} {-name-} {-type-} | CNil
    -- types
    | TBase | TArrow {-type-} {-type-} | TBool
    -- metadata
    | Name String

derive instance Generic ExprLabel _
instance Eq ExprLabel where eq x y = genericEq x y
instance Ord ExprLabel where compare x y = genericCompare x y
instance Show ExprLabel where show x = genericShow x

instance Pretty ExprLabel where
    pretty STerm = "term-sort"
    pretty SVar = "var-sort"
    pretty SType = "type-sort"
    pretty CCons = "ctx-cons"
    pretty CNil = "ctx-nil"
    pretty TBase = "base"
    pretty TArrow = "arrow"
    pretty TBool = "bool"
    pretty (Name str) = str

instance Expr.IsExprLabel ExprLabel where
    -- prettyExprF'_unsafe :: Partial => ExprF ExprLabel String -> String
    prettyExprF'_unsafe _ = Hole.hole "STLC prettyExprF'_unsafe"
    -- expectedKidsCount :: ExprLabel -> Int
    expectedKidsCount _ = Hole.hole "STLC expectedKidsCount"

type Expr = Expr.Expr ExprLabel
type MetaExpr = Expr.MetaExpr ExprLabel

data RuleLabel = Lam | App | Z | S | Var | Let | Base | Arrow | If | Newline -- | TermBind

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where show x = genericShow x

instance Pretty RuleLabel where
    pretty Lam = "lam"
    pretty App = "app"
    pretty Z = "z"
    pretty S = "s"
    pretty Var = "var"
    pretty Let = "let"
    pretty Base = "base"
    pretty Arrow = "->"
    pretty If = "if"
    pretty Newline = "newline"

instance Enum RuleLabel where
    succ x = genericSucc x
    pred x = genericPred x

instance Bounded RuleLabel where
    bottom = genericBottom
    top = genericTop

instance IsRuleLabel ExprLabel RuleLabel where
    prettyExprF'_unsafe_RuleLabel (Lam /\ [x, alpha, b]) = P.parens $ "λ" <+> x <+> ":" <+> alpha <+> "↦" <+> b
    prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
    prettyExprF'_unsafe_RuleLabel (Z /\ []) = "Z"
    prettyExprF'_unsafe_RuleLabel (S /\ [x]) = "S" <> x
    prettyExprF'_unsafe_RuleLabel (Var /\ [x]) = "@" <> x
    prettyExprF'_unsafe_RuleLabel (Let /\ [x, alpha, a, b]) = P.parens $ "let" <+> x <+> ":" <+> alpha <+> "=" <+> a <+> "in" <+> b
    prettyExprF'_unsafe_RuleLabel (Base /\ []) = "Base"

    language = TotalMap.makeTotalMap case _ of
        -- Terms
        App -> G.makeRule ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
            [ gamma ⊢ exp TArrow [alpha, beta]
            , gamma ⊢ alpha ]
            /\ --------
            ( gamma ⊢ beta )
        Lam -> G.makeRule ["gamma", "alpha", "name", "beta"] \[gamma, alpha, name, beta] ->
            [ exp SType [alpha]
            , exp CCons [gamma, name, alpha] ⊢ beta ]
            /\ --------
            ( gamma ⊢ exp TArrow [alpha, beta] )
        Var -> G.makeRule ["gamma", "x"] \[gamma, alpha] ->
            [ gamma ⊢⊢ alpha ]
            /\ --------
            ( gamma ⊢ alpha )
        Let -> G.makeRule ["gamma", "name", "alpha", "beta"] \[gamma, name, alpha, beta] ->
            [ exp SType [alpha]
            , gamma ⊢ alpha
            , exp CCons [gamma, name, alpha] ⊢ beta ]
            /\ --------
            ( gamma ⊢ beta )
        If -> G.makeRule ["a", "g"] \[a, g] ->
            []
            /\ --------
            (g ⊢  exp TArrow [exp TBool [], exp TArrow [a, exp TArrow [a, a]]])
        -- Types
        Arrow -> G.makeRule ["a", "b"] \[a, b] ->
            [exp SType [a], exp SType [b]]
            /\ --------
            (exp SType [exp TArrow [a, b]])
        Base -> G.makeRule [] \[] ->
            []
            /\ --------
            ( exp SType [exp TBase []] )
        -- Variables
        Z -> G.makeRule ["gamma", "name", "alpha"] \[gamma, name, alpha] ->
            [ ]
            /\ --------
            ( exp CCons [gamma, name, alpha] ⊢⊢ alpha )
        S -> G.makeRule ["gamma", "alpha", "name", "beta"] \[gamma, alpha, name, beta] ->
            [ gamma ⊢⊢ alpha ]
            /\ --------
            ( exp CCons [gamma, name, beta] ⊢⊢ alpha )
        -- Miscellaneous
        Newline -> G.makeRule ["s"] \[s] ->
            [s]
            /\ --------
            s

    isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
        _ -> hole "TODO: @jacob add hole rule(s) to STLC"

    defaultDerivTerm' = case _ of
        _ -> hole "TODO: @jacob add hole rule(s) to STLC"

type DerivLabel = G.DerivLabel ExprLabel RuleLabel

type DerivTerm = G.DerivTerm ExprLabel RuleLabel
type DerivPath dir = G.DerivPath dir ExprLabel RuleLabel

type Rule = G.Rule ExprLabel

exp :: forall l. l -> Array (Expr.MetaExpr l) -> Expr.MetaExpr l
exp l kids = Expr.Expr (Meta (Right l)) kids

var :: forall l. MetaVar -> Expr.MetaExpr l
var x = Expr.Expr (Meta (Left x)) []

judgeTerm gamma alpha = exp STerm [gamma, alpha]
infix 8 judgeTerm as ⊢

judgeVar gamma alpha = exp SVar [gamma, alpha]
infix 8 judgeVar as ⊢⊢
