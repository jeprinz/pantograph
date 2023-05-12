module Language.Pantograph.STLC.Grammar where

import Data.Tuple.Nested
import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (Meta(..), MetaVar(..), freshMetaVar')
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Unit (unit)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, expectedHypothesesCount)
import Language.Pantograph.Generic.Grammar as G

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

type Expr = Expr.Expr ExprLabel
type MetaExpr = Expr.MetaExpr ExprLabel

data RuleLabel = Lam | App | Z | S | Var | Let | Base -- | TermBind

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where show x = genericShow x
instance Enum RuleLabel where
    succ x = genericSucc x
    pred x = genericPred x
instance Bounded RuleLabel where
    bottom = genericBottom
    top = genericTop

instance IsRuleLabel RuleLabel where
    expectedHypothesesCount Lam = 3
    expectedHypothesesCount App = 2
    expectedHypothesesCount Z = 0
    expectedHypothesesCount S = 1
    expectedHypothesesCount Var = 1
    expectedHypothesesCount Let = 4
    expectedHypothesesCount Base = 0

type DerivLabel = G.DerivLabel ExprLabel RuleLabel

type DerivTerm = G.DerivTerm ExprLabel RuleLabel
type DerivPath dir = G.DerivPath dir ExprLabel RuleLabel

type Rule = G.Rule ExprLabel

exp :: forall l. l -> Array (Expr.MetaExpr l) -> Expr.MetaExpr l
exp l kids = Expr.Expr (Meta (Right l)) kids

var :: forall l. MetaVar -> Expr.MetaExpr l
var x = Expr.Expr (Meta (Left x)) []

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

