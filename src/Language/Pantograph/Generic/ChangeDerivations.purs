module Language.Pantograph.Generic.ChangeDerivations where

import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Unification
import Prelude

import Data.Array (unzip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr (Meta(..))
import Data.Expr as Expr
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List as List
import Data.List.Zip (Path(..))
import Data.List.Zip as ListZip
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import Type.Direction as Dir
import Util (lookup', fromJust', fromRight)
import Util as Util

{-
Issue:
You need to deal with up rules in terms, and also down rules in paths.

Its not so straightforwards to do the pattern matching.

Maybe instead of relying on purescript pattern matching, we could implement our own pattern matching with metavariables over
the derivations/derivation paths. But we could still use purescript pattern matching for the changes, because it seems like
that should still work?
-}

{-
This notion of change rules can be used to derive either smallstep or chDeriv and chPath directly!

The idea is that each smallstep rule has exactly one boundary on it's left side. If that boundary is an upboundary, then it
is encoded as an UpRule. If a downwards, boundary, then its encoded as a DownRule.

For example, the rule:
↓{lam x : A . e}_(C1 -> C2)  ~~>  lam x : ↓{A}C1 . ↓{e}C2
Is encoded as the DownRule:
\(C1 -> C2) (lam x : A . e) down = Just $ (lam x : down C1 A . down C2 B)

↑{e1}c e2  ~~>  	↑{e1 ↓{e2}c}c

[] e2


OPTION 1: ALL UNIFICATION
Rule = VeryMetaDeriv x VeryMetaDeriv
>>>> Doesn't work because can't handle inverses and composition

OPTION 1.5: MOSTLY UNIFICATION
Rule = VeryMetaDeriv x (Map UUID (Either Deriv Path Change) -> Deriv)

OPTION 2: SOME UNIFICATION
DownRule = MetaDeriv x (Change -> Map UUID Deriv -> Deriv x Change)
UpRule = MetaPath x (Change -> Map UUID Deriv -> Path x Change)

OPTION 3: Pretend that paths are terms
step 1: convert paths to terms
step 2: do smallstep with (Deriv -> Maybe Deriv)
step 3: if it was a path, convert back to a term

OPTION 4: forget cursor position until later
step 1: the program is a term paths don't exist
step 2: do smallstep with (Deriv -> Maybe Deriv)
step 3: figure out where the cursor was?

PROBLEM: what if the "cursor was here" node got in the middle of a rule working?



Thus, we can use purescript pattern matching to implement the rules! No need to implement our own pattern matching and
unification over boundaries, changes, and terms.

-}
type DownRule l r = Expr.Change l -> DerivTermWithBoundaries l r
    -> (Expr.Change l -> DerivTermWithBoundaries l r -> DerivTermWithBoundaries l r)
    -> Maybe (DerivTermWithBoundaries l r /\ Expr.Change l)
type UpRule l r = Expr.Change l -> DerivPath l r
    -> (Expr.Change l -> DerivTermWithBoundaries l r -> DerivTermWithBoundaries l r) -> (Expr.Change l -> DerivPathWithBoundaries l r -> DerivPathWithBoundaries l r)
    -> Maybe (DerivPath l r /\ Expr.Change l)

type ChangeAlgorithm l r = List (DownRule l r) /\ List (UpRule l r)

--getApplicableRule :: forall l r. ChangeAlgorithm l r -> DerivTerm l r -> Maybe (DownRule)


data DerivWithBoundariesLabel l r = DerivWithBoundariesLabel r (Expr.MetaExpr l) | DownBoundary (Expr.Change (Meta l)) | UpBoundary (Expr.Change (Meta l))

type DerivTermWithBoundaries l r = Expr.Expr (DerivWithBoundariesLabel l r)
type DerivPathWithBoundaries l r = Expr.Path Dir.Up (DerivWithBoundariesLabel l r)

downBoundary :: forall l r . Expr.Change l -> DerivTermWithBoundaries l r -> DerivTermWithBoundaries l r
downBoundary ch term = Expr.Expr (DownBoundary (map (map (Meta <<< Right)) ch)) [term]

upBoundary :: forall l r . Expr.Change l -> DerivTermWithBoundaries l r -> DerivTermWithBoundaries l r
upBoundary ch term = Expr.Expr (UpBoundary (map (map (Meta <<< Right)) ch)) [term]

--updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)

-- returns nothing if there are no more boundaries
--smallStepTerm :: forall l r. ChangeAlgorithm l r -> DerivTermWithBoundaries l r -> Maybe (DerivTermWithBoundaries l r)
--smallStepTerm algorithm@(downRules /\ upRules) (Expr.Gram (l /\ kids)) =
--    case l of
--        DerivWithBoundariesLabel rule sort ->
--            case Util.findWithIndex (smallStepTerm algorithm) kids of
--                Nothing -> Nothing
--                Just (kid /\ i) -> Just $ Expr.Gram (l /\
--                    fromJust' "smallStepTerm" (Array.updateAt i kid kids))
--        DownBoundary ch ->
--            let kid = Util.assertSingleton kids in
--            case Util.findWithIndex (\downRule -> downRule ch kid downBoundary) downRules of
--                Just ((kid' /\ chUp) /\ _) -> Just if isId chUp then kid' else upBoundary chUp kid'
--                Nothing -> do
--                    kid' <- smallStepTerm algorithm kid
--                    pure $ Expr.Gram (l /\ [kid'])
--        UpBoundary ch -> unsafeCrashWith "TODO" --?h

{-

--{-
--This function implements generic typechanging on derivations, using the rules from a type system!
--It doesn't yet have the ability to input special cases implemented.
---}
--chDeriv :: forall l r . Ord l => Ord r => Language l r -> Change l -> DerivTerm l r -> DerivTerm l r
--chDeriv lang ch (Gram ((DerivLabel l _) /\ kids)) =
----data Rule l r = Rule (Set Expr.MetaVar) (Array (MetaExpr l)) (MetaExpr l)
--    let (Rule boundMetaVars kidSorts parentSort) = lookup' l lang in
--    -- TODO: freshen the metavars in kidSorts and parentSort
--    case unifyTemp2 (map Right ch) (map (map Expr) parentSort) of
--        Just sub ->
--            let perKid kid@(Gram ((DerivLabel _ actualSort) /\ _)) kidSort =
--                    let ci = applySub2 sub (map (map Expr) kidSort) in
--                    -- TODO: these lines of code are correct, its just annoying to get all the types to work out. Also a question of how general we want unify to be.
----                    let cLeft = fst (endpoints ci) in
----                    let sub' = fromJust' "chDeriv" $ unifyTemp2 cLeft actualSort in
----                    let kidSort' = (map (Expr <<< fromRight) kidSort) in
------                    let change = applySub sub' ?h in
----                    chDeriv lang ?h kid
--                    ?h
--            in
----            let perKid = ?h in
--            Gram ((DerivLabel l (snd (endpoints ?h))) /\ (perKid <$> kids <*> kidSorts))
--        Nothing -> ?h
---}