module Language.Pantograph.GenericExpr.TypeCheck where

import Prelude
import Language.Pantograph.GenericExpr.Expression

typecheck :: forall l. Language l
    -> Expr l {-the sort-}
    -> Expr l {-the expression-}
    -> Boolean
typecheck lang sort (Expr label kids) =
    let (TypingRule parentSort kidSorts) = lang label in
    ?h
