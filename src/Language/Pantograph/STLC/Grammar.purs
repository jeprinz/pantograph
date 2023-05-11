module Language.Pantograph.STLC.Grammar where

import Data.Tuple.Nested

import Data.Either (Either(..))
import Data.Expr (Meta(..), MetaVar(..), freshMetaVar)
import Data.Expr as Expr
import Data.Set as Set
import Data.Unit (unit)
import Language.Pantograph.Generic.Grammar as G

data Label =
    -- sorts
    STerm {-context-} {-type-} | SVar {-context-} {-type-} | SType {-type-}
    -- TODO: add a sort for names, and place an element of SName as a child of let and lambda, and make a Name rule
    -- contexts
    | CCons {-context-} {-name-} {-type-} | CNil
    -- types
    | TBase | TArrow {-type-} {-type-} | TBool
    -- metadata
    | Name String

type Expr = Expr.Expr Label
type MetaExpr = Expr.MetaExpr Label

data RuleName = Lam | App | Z | S | Var | Let | Base -- | TermBind

type DerivLabel = G.DerivLabel Label RuleName

type DerivTerm = G.DerivTerm Label RuleName
type DerivPath = G.DerivPath Label RuleName

type Rule = G.Rule Label RuleName

exp :: forall l. l -> Array (Expr.MetaExpr l) -> Expr.MetaExpr l
exp l kids = Expr.Expr (Meta (Right l)) kids

var :: forall l. MetaVar -> Expr.MetaExpr l
var x = Expr.Expr (Meta (Left x)) []

--data Rule l r = Rule (Set MetaVar) (Array (MetaExpr l)) (MetaExpr l)
rules :: Array Rule
rules = [
    --------------------------- Terms ----------------------------------------------
    -- app
    let g = freshMetaVar unit in
    let a = freshMetaVar unit in
    let b = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [exp STerm [var g, exp TArrow [var a, var b]],   exp STerm [var g, var a]]
        (exp STerm [var g, var b])
    ,
    -- lambda
    let g = freshMetaVar unit in
    let a = freshMetaVar unit in
    let b = freshMetaVar unit in
    let n = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [
            exp SType [var a], -- the type annotation (comment this line if we don't want annotations on lambdas)
            exp STerm [exp CCons [var g, var n, var a], var b] -- the body
        ]
        (exp STerm [var g, exp TArrow [var a, var b]])
    ,
    -- var
    let g = freshMetaVar unit in
    let t = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, t])
        [exp SVar [var g, var t]]
        (exp STerm [var g, var t])
    ,
    -- let
    let g = freshMetaVar unit in
    let defTy = freshMetaVar unit in
    let bodyTy = freshMetaVar unit in
    let n = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, defTy, bodyTy])
        [
            exp SType [var defTy], -- type
            exp STerm [exp CCons [var g, var n, var defTy], var defTy], -- definition
            exp STerm [exp CCons [var g, var n, var defTy], var bodyTy]] --body
        (exp STerm [var g, var bodyTy])

    ,
    -- if
    let a = freshMetaVar unit in
    let g = freshMetaVar unit in
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
    let a = freshMetaVar unit in
    let b = freshMetaVar unit in
    G.Rule (Set.fromFoldable [])
        [exp SType [var a], exp SType [var b]]
        (exp SType [exp TArrow [var a, var b]])

    --------------------------- Debruin Indices ------------------------------------
    ,
    -- zero
    let g = freshMetaVar unit in
    let t = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, t])
        []
        (exp SVar [exp CCons [var g,{-TODO: name-} var t], var t])
    ,
    -- suc
    let g = freshMetaVar unit in
    let a = freshMetaVar unit in
    let b = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [exp SVar [var g, var a]]
        (exp SVar [exp CCons [var g, {-TODO-} var b], var a])

    --------------------------- Other ----------------------------------------------
    ,
    -- hole
    let s = freshMetaVar unit in
    G.Rule (Set.fromFoldable [s])
        []
        (var s)
    ,
    -- newline
    let s = freshMetaVar unit in
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

