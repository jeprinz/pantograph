module Language.Pantograph.Generic.Unification where

import Prelude

{-

Here is roughly how unify should work:

unify :: forall l. (l -> l -> Maybe Sub) -> Expr l -> Expr l -> Maybe Sub

unify (Minus A B1) (Minus A B2) -> unify B1 B2
unify (Minus A1 B1) (Minus A2 B2) -> fail if A1 =/= A2


-}