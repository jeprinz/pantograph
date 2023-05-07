module Language.Pantograph.STLC.Grammar where

import Language.Pantograph.Generic.Grammar as G
import Data.Unify (freshMetaVar)
import Data.Unit (unit)
import Data.Set as Set
import Data.Gram as Gram
import Data.Tuple.Nested
import Data.Unify (MetaVar, Meta(..))
import Data.Either (Either(..))

data Label =
    -- sorts
    STerm {-context-} {-type-} | SVar {-context-} {-type-} | SType {-type-}
    -- contexts
    | CCons {-context-} {-type-} | CNil
    -- types
    | TBase | TArrow {-type-} {-type-}

type Expr = G.Expr Label
type MetaExpr = G.MetaExpr Label

data RuleName = Lam | App | Z | S | Var | Let

type DerivLabel = G.DerivLabel Label RuleName

type DerivTerm = G.DerivTerm Label RuleName
type DerivPath = G.DerivPath Label RuleName

type Rule = G.Rule Label RuleName

exp :: forall l. l -> Array (Gram.MetaExpr l) -> Gram.MetaExpr l
exp l kids = Gram.Gram (Meta (Right l) /\ kids)

var :: forall l. MetaVar -> Gram.MetaExpr l
var x = Gram.Gram (Meta (Left x) /\ [])

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
    G.Rule (Set.fromFoldable [g, a, b])
        [
            exp SType [var a], -- the type annotation (comment this line if we don't want annotations on lambdas)
            exp STerm [exp CCons [var g, var a], var b] -- the body
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
    G.Rule (Set.fromFoldable [g, defTy, bodyTy])
        [
            exp SType [var defTy], -- type
            exp STerm [exp CCons [var g, var defTy], var defTy], -- definition
            exp STerm [exp CCons [var g, var defTy], var bodyTy]] --body
        (exp STerm [var g, var bodyTy])

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
        (exp SVar [exp CCons [var g, var t], var t])
    ,
    -- suc
    let g = freshMetaVar unit in
    let a = freshMetaVar unit in
    let b = freshMetaVar unit in
    G.Rule (Set.fromFoldable [g, a, b])
        [exp SVar [var g, var a]]
        (exp SVar [exp CCons [var g, var b], var a])

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

