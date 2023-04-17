module Language.Pantograph.GenericExpr.TypeCheck where

import Prelude
import Language.Pantograph.GenericExpr.Expression

typecheck :: forall l. Language l -> Expr l -> Boolean
typecheck lang e =
    ?h
