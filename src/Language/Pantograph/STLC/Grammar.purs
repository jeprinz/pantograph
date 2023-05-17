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
import Hole as Hole
import Language.Pantograph.Generic.Grammar (class IsRuleLabel)
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

data RuleLabel = Lam | App | Z | S | Var | Let | Base -- | TermBind

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
        Z -> G.makeRule ["gamma", "name", "alpha"] \[gamma, name, alpha] ->
            [ ]
            /\ --------
            ( exp CCons [gamma, name, alpha] ⊢@ alpha )
        S -> G.makeRule ["gamma", "alpha", "name", "beta"] \[gamma, alpha, name, beta] ->
            [ gamma ⊢@ alpha ]
            /\ --------
            ( exp CCons [gamma, name, beta] ⊢@ alpha )
        Var -> G.makeRule ["gamma", "x"] \[gamma, alpha] ->
            [ gamma ⊢@ alpha ]
            /\ --------
            ( gamma ⊢ alpha )
        Let -> G.makeRule ["gamma", "name", "alpha", "impl", "beta", "body"] \[gamma, name, alpha, impl, beta, body] ->
            [ exp SType [alpha]
            , gamma ⊢ alpha
            , exp CCons [gamma, name, alpha] ⊢ beta ] 
            /\ --------
            ( gamma ⊢ beta )
        Base -> G.makeRule [] \[] ->
            []
            /\ --------
            ( exp SType [exp TBase []] )

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
infix 8 judgeVar as ⊢@

--data Rule l r = Rule (Set MetaVar) (Array (MetaExpr l)) (MetaExpr l)
rules :: Array Rule
rules = [
    --------------------------- Terms ----------------------------------------------
    -- app
    let g = freshMetaVar' unit in
    let a = freshMetaVar' unit in
    let b = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [exp STerm [var g, exp TArrow [var a, var b]],   exp STerm [var g, var a]]
        (exp STerm [var g, var b])
    ,
    -- lambda
    let g = freshMetaVar' unit in
    let a = freshMetaVar' unit in
    let b = freshMetaVar' unit in
    let n = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [
            exp SType [var a], -- the type annotation (comment this line if we don't want annotations on lambdas)
            exp STerm [exp CCons [var g, var n, var a], var b] -- the body
        ]
        (exp STerm [var g, exp TArrow [var a, var b]])
    ,
    -- var
    let g = freshMetaVar' unit in
    let t = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, t])
        [exp SVar [var g, var t]]
        (exp STerm [var g, var t])
    ,
    -- let
    let g = freshMetaVar' unit in
    let defTy = freshMetaVar' unit in
    let bodyTy = freshMetaVar' unit in
    let n = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, defTy, bodyTy])
        [
            exp SType [var defTy], -- type
            exp STerm [exp CCons [var g, var n, var defTy], var defTy], -- definition
            exp STerm [exp CCons [var g, var n, var defTy], var bodyTy]] --body
        (exp STerm [var g, var bodyTy])

    ,
    -- if
    let a = freshMetaVar' unit in
    let g = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [a])
        []
        -- Bool -> a -> a -> a
        (exp STerm [var g, exp TArrow [exp TBool [], exp TArrow [var a, exp TArrow [var a, var a]]]])


    --------------------------- Types ----------------------------------------------
    ,
    -- base
    G.Rule (Set.fromFoldable [])
        []
        (exp SType [exp TBase []])
    ,
    -- arrow
    let a = freshMetaVar' unit in
    let b = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [])
        [exp SType [var a], exp SType [var b]]
        (exp SType [exp TArrow [var a, var b]])

    --------------------------- Debruin Indices ------------------------------------
    ,
    -- zero
    let g = freshMetaVar' unit in
    let t = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, t])
        []
        (exp SVar [exp CCons [var g,{-TODO: name-} var t], var t])
    ,
    -- suc
    let g = freshMetaVar' unit in
    let a = freshMetaVar' unit in
    let b = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [exp SVar [var g, var a]]
        (exp SVar [exp CCons [var g, {-TODO-} var b], var a])

    --------------------------- Other ----------------------------------------------
    ,
    -- hole
    let s = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [s])
        []
        (var s)
    ,
    -- newline
    let s = freshMetaVar' unit in
    G.Rule (Set.fromFoldable [s])
        [var s]
        (var s)

{-

Var G A
---------------------- suc
Var (G , T) A

----------------------- zero
Var (G , T) T

-}

]

