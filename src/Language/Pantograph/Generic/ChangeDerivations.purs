module Language.Pantograph.Generic.ChangeDerivations where

import Prelude

import Data.Array (unzip)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List.Zip (Path(..))
import Data.List.Zip as ListZip
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import Language.Pantograph.Generic.Grammar
import Data.Gram (Gram(..))
import Util (lookup', fromJust', fromRight)
import Data.Gram (ChangeLabel(..))
import Data.Either (Either(..))
import Data.Tuple (fst, snd)
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Unification
import Data.Maybe (Maybe(..))

{-
This function implements generic typechanging on derivations, using the rules from a type system!
It doesn't yet have the ability to input special cases implemented.
-}

{-

chDeriv :: forall l r . Ord l => Ord r => Language l r -> Change l -> DerivTerm l r -> DerivTerm l r
chDeriv lang ch (Gram ((DerivLabel l _) /\ kids)) =
--data Rule l r = Rule (Set Gram.MetaVar) (Array (MetaExpr l)) (MetaExpr l)
    let (Rule boundMetaVars kidSorts parentSort) = lookup' l lang in
    -- TODO: freshen the metavars in kidSorts and parentSort
    case unifyTemp2 (map Right ch) (map (map Expr) parentSort) of
        Just sub ->
            let perKid kid@(Gram ((DerivLabel _ actualSort) /\ _)) kidSort =
                    let ci = applySub2 sub (map (map Expr) kidSort) in
                    -- TODO: these lines of code are correct, its just annoying to get all the types to work out. Also a question of how general we want unify to be.
--                    let cLeft = fst (endpoints ci) in
--                    let sub' = fromJust' "chDeriv" $ unifyTemp2 cLeft actualSort in
--                    let kidSort' = (map (Expr <<< fromRight) kidSort) in
----                    let change = applySub sub' ?h in
--                    chDeriv lang ?h kid
                    ?h
            in
--            let perKid = ?h in
            Gram ((DerivLabel l (snd (endpoints ?h))) /\ (perKid <$> kids <*> kidSorts))
        Nothing -> ?h
-}