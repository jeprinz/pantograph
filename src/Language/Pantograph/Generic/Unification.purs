module Language.Pantograph.Generic.Unification where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Language.Pantograph.Generic.Grammar
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.UUID (UUID)
import Data.Gram (Meta)
import Data.Gram (Gram(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Gram (MetaVar)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

{-

Here is roughly how unify should work:

unify :: forall l. (l -> l -> Maybe Sub) -> Expr l -> Expr l -> Maybe Sub

unify (Minus A B1) (Minus A B2) -> unify B1 B2
unify (Minus A1 B1) (Minus A2 B2) -> fail if A1 =/= A2

we need to be able to:
- unify an Expr l with a Change l, getting a substitution of metavars for changes
-

In order to be able to do
unify (+A -> B) (+A' -> B'), we would need to unify the EXPRESSIONS A and A'. But those are in labels in B and B'!

Its not clear to me though if we'll need to be able to do that.
We might only need to do unification of changes with expressions, or changes with changes where the metavariables are
only in the change parts, not the expression parts.

The reasoning is that when we perform the typechanging algorithm, the change going in doesn't have metavariables -
its really only being unified with an expression that has metavariables.

If I have a Change = Expr (ChangeLabel l), and then I turn it into something with metavariables for unification
then I have Expr (ChangeLabel (Meta l)), which still doesn't have metavariables in the teeth things.

It seems wierd to have an change with both "change metavariables" and "expression metavariables" in it anyway.

-}


class Subbable a where
    sub :: (MetaVar -> a) -> a -> a

--class SubType a b where
--    inject :: a -> b
--    goback :: b -> Maybe a
--
--instance SubType a a where
--    inject x = x
--    goback = Just
--
--instance SubType MetaVar l => Subbable (Expr l) where
--    sub s (Gram (l /\ kids)) = case goback l of
--        Just mv -> s mv
--        Nothing -> Gram (l /\ map (sub s) kids)

type Sub outer label = Map UUID (Expr (outer (Meta label)))

applySub :: forall outer l. Functor outer => Sub outer l -> Expr (outer (Meta l)) -> Expr (outer (Meta l))
applySub sub (Gram (l /\ kids)) =

    unsafeCrashWith "unimplemented"

-- just so I can reference this in other files for now
unifyTemp :: forall outer l. Functor outer => Expr (outer (Meta l)) -> Expr (outer (Meta l)) -> Maybe (Sub outer l)
unifyTemp = unsafeCrashWith "unimplemented"

type Sub2 l = Map UUID (Expr (Meta l))

unifyTemp2 :: forall l. Expr (Meta l) -> Expr (Meta l) -> Maybe (Sub2 l)
unifyTemp2 = unsafeCrashWith "unimplemented"

applySub2 :: forall l . Sub2 l -> MetaExpr l -> MetaExpr l
applySub2 = unsafeCrashWith "unimplemented"
