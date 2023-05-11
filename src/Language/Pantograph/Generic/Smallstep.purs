module Language.Pantograph.Generic.Smallstep where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Type.Direction as Dir
import Data.List.Zip as ZipList
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Data.List.Rev as Rev
import Data.List as List
import Data.List (List(..), (:))
import Data.Foldable
import Language.Pantograph.Generic.Grammar as Grammar

import Data.Gram as Gram
import Partial.Unsafe (unsafeCrashWith)
import Data.Either (Either(..))
import Util (lookup')
import Language.Pantograph.Generic.Unification
import Data.Tuple (snd, fst)
import Language.Pantograph.Generic.ChangeAlgebra (endpoints)
import Language.Pantograph.Generic.Grammar (MetaChange)
import Language.Pantograph.Generic.ChangeAlgebra (mEndpoints)
import Data.Unify (Meta(..))
import Language.Pantograph.Generic.ChangeAlgebra

data Direction = Up | Down


data StepLabel l r = Inject (Grammar.DerivLabel l r) | Cursor | Boundary Direction (MetaChange l)
type Term l r = Gram.Expr (StepLabel l r)

type Rule l r = Term l r -> Maybe (Term l r)

-- later Henry can tell me how his definition of path works
type SanePath l = List (Gram.Tooth l)

---------- Code for converting zippers to terms and back ------------------------------------------

addToothToTerm :: forall l r. Gram.Tooth (Grammar.DerivLabel l r) -> Term l r -> Term l r
addToothToTerm (l /\ ZipList.Path {left, right}) t =
    Gram.Gram (Inject l /\
        ((Array.fromFoldable (map (map Inject) (Rev.unreverse left)))
            <> [t]
            <> (Array.fromFoldable (map (map Inject) right))))

zipperToTerm :: forall l r. SanePath (Grammar.DerivLabel l r) -> Grammar.DerivTerm l r -> Term l r
zipperToTerm Nil exp = Gram.Gram (Cursor /\ [map Inject exp])
zipperToTerm (th : path) exp = addToothToTerm th (zipperToTerm path exp)

assertJustExpr :: forall l r. Term l r -> Grammar.DerivTerm l r
assertJustExpr _ = unsafeCrashWith "Error: assertJustExpr assertion failed"
assertJustExpr (Gram.Gram (Inject l /\ kids)) = Gram.Gram (l /\ map assertJustExpr kids)

assertNoneOfList :: forall t b. List t -> (t -> Maybe b) -> List b
assertNoneOfList Nil _f = Nil
assertNoneOfList (x : xs) f = case f x of
    Nothing -> unsafeCrashWith "assertNoneOfList: assertion failed"
    Just y -> y : assertNoneOfList xs f

-- assert that one or zero of the children will return a
oneOrNone :: forall t a b. List t -> (t -> Either a b) -> Either (List b) (List b /\ a /\ List b)
oneOrNone Nil _f = Left Nil
oneOrNone (x : xs) f = case f x of
    Right b ->
        case oneOrNone xs f of
            Right (bs1 /\ a /\ bs2) -> Right ((b : bs1) /\ a /\ bs2)
            Left bs -> Left (b : bs)
    Left a ->
        let bs2 = assertNoneOfList xs
                (\aa -> case f aa of
                        Left _ -> Nothing
                        Right b -> Just b)
        in
        Right (Nil /\ a /\ bs2)

termToZipper :: forall l r. Term l r -> (SanePath (Grammar.DerivLabel l r) /\ Grammar.DerivTerm l r)
termToZipper (Gram.Gram (Cursor /\ [kid])) =
    Nil /\ (assertJustExpr kid)
termToZipper (Gram.Gram ((Inject l) /\ kids)) =
    let kids' = List.fromFoldable $ map termToZipper kids in
    let isPath (p /\ e) = case p of
            Nil -> Right e
            _ -> Left (p /\ e) in
    let pathOrNot = oneOrNone kids' isPath in
    case pathOrNot of
        -- child didn't have cursor
        Left kids'' -> (Nil /\ Gram.Gram (l /\ Array.fromFoldable (kids'')))
        -- child has exactly one cursor
        Right (leftKids /\ (p /\ e) /\ rightKids) ->
            let newTooth = l /\ ZipList.Path {left: Rev.reverse leftKids, right: rightKids} in
            ((newTooth : p) /\ e)
--    let
termToZipper _ = unsafeCrashWith "shouldn't happen"

--------------------------------------------------------------------------------

------------- Code for doing smallstep -------------------------------------

doAnyApply :: forall t out. t -> List (t -> Maybe out) -> Maybe out
doAnyApply t Nil = Nothing
doAnyApply t (r : rs) = case r t of
    Just t' -> Just t'
    Nothing -> doAnyApply t rs

stepSomebody :: forall l r. List (Term l r) -> List (Rule l r) -> Maybe (List (Term l r))
stepSomebody Nil _ = Nothing
stepSomebody (t : ts) rules = case step t rules of
    Just t' -> Just (t' : ts)
    Nothing -> (:) <$> pure t <*> stepSomebody ts rules

step :: forall l r. Term l r -> List (Rule l r) -> Maybe (Term l r)
step t@(Gram.Gram (l /\ kids)) rules =
    case doAnyApply t rules of
        Nothing -> do
            kids' <- stepSomebody (List.fromFoldable kids) rules
            pure $ Gram.Gram (l /\ Array.fromFoldable kids')
        Just t' -> Just t'


-------------- Default rules --------------------------------------------
--type Language l r = Map r (Rule l r)
--data Rule l r = Rule (Set MetaVar) (Array (MetaExpr l)) (MetaExpr l)

-- The sorts in these are Expr (Meta (ChangeLabel (Meta l)).
-- the out Meta is the one that comes from the normal rules. The (ChangeLabel (Meta l))
-- corresponds with the changes in the boundaries, which are Expr (ChangeLabel (Meta l))
type ChLanguage l r = Grammar.Language (Gram.ChangeLabel (Meta l)) r

metaInject :: forall l. Gram.MetaExpr l -> Gram.MetaExpr (Gram.ChangeLabel (Meta l))
metaInject e = map (map (Gram.Expr <<< Meta <<< Right)) e

langToChLang :: forall l r. Grammar.Language l r -> ChLanguage l r
langToChLang lang = map (\(Grammar.Rule vars kids parent)
        -> Grammar.Rule vars (map metaInject kids) (metaInject parent)) lang

-- TODO: everywhere that a boundary is introduced, I should have a function which does that, and only introduces it
-- if the change is not the identity.

-- wraps a boundary unless the change is the identity, in which case so is this function
wrapBoundary :: forall l r. Direction -> MetaChange l -> Term l r -> Term l r
wrapBoundary dir ch t = if isId ch then t else Gram.Gram (Boundary dir ch /\ [t])

-- Down rule that steps boundary through form - defined generically for every typing rule!
defaultDown :: forall l r. Eq l => Ord r => Ord l => ChLanguage l r -> Rule l r
defaultDown lang (Gram.Gram (Boundary Down ch /\
    [Gram.Gram ((Inject (Grammar.DerivLabel ruleName sort)) /\ kids)])) =
    let (Grammar.Rule metaVars crustyKidGSorts crustyParentGSort) = lookup' ruleName lang in
    let freshener = genFreshener metaVars in
    let kidGSorts = map (freshen freshener) crustyKidGSorts in
    let parentGSort = freshen freshener crustyParentGSort in
    -- TODO: is this the right check?
    if not (fst (endpoints ch) == sort) then unsafeCrashWith "assertion failed: ch boundary didn't match sort in defaultDown" else
    do
        sub /\ chBackUp <- doOperation ch parentGSort
        let kidGSorts' = map (fullySubMetaExpr sub) kidGSorts
        let kidsWithBoundaries = (\ch kid -> wrapBoundary Down ch kid) <$> kidGSorts' <*> kids
        let newSort = fst (endpoints chBackUp)
        pure $ wrapBoundary Up chBackUp $ Gram.Gram ((Inject (Grammar.DerivLabel ruleName newSort)) /\ kidsWithBoundaries)
defaultDown _ _ = Nothing

-- finds an element of a list satisfying a property, and splits the list into the pieces before and after it
getFirst :: forall t a b. List t -> (t -> Maybe a) -> Maybe (List t /\ a /\ List t)
getFirst Nil _f = Nothing
getFirst (x : xs) f = case f x of
    Nothing ->
        do ts1 /\ a /\ ts2 <- getFirst xs f
           pure $ ((x : ts1) /\ a /\ ts1)
    Just a -> Just (Nil /\ a /\ xs)

defaultUp :: forall l r. Eq l => Ord r => Ord l => ChLanguage l r -> Rule l r
defaultUp lang (Gram.Gram (Inject (Grammar.DerivLabel ruleName sort) /\ kids)) =
    let (Grammar.Rule metaVars crustyKidGSorts crustyParentGSort) = lookup' ruleName lang in
    let freshener = genFreshener metaVars in
    let kidGSorts = map (freshen freshener) crustyKidGSorts in
    let parentGSort = freshen freshener crustyParentGSort in
    let findUpBoundary = case _ of
            Gram.Gram (Boundary Up ch /\ [kid]) /\ sort -> Just (ch /\ kid /\ sort)
            kid /\ _ -> Nothing
    in
    do
        (leftKidsAndSorts /\ (ch /\ kid /\ gSort) /\ rightKidsAndSorts)
            <- getFirst ((List.fromFoldable (Array.zip kids kidGSorts))) findUpBoundary
--        ch' /\ sub <- unify ch gSort
        sub /\ chBackDown <- doOperation ch gSort
--        let wrapKid (kid /\ gSort) = Gram.Gram (Boundary Down (subMetaExpr sub gSort) /\ [kid])
        let wrapKid (kid /\ gSort) = wrapBoundary Down (fullySubMetaExpr sub gSort) kid
        let leftKids = map wrapKid leftKidsAndSorts
        let rightKids = map wrapKid rightKidsAndSorts
--        let parentBoundary node = Gram.Gram (Boundary Up (subMetaExpr sub parentGSort) /\ [node])
        let parentBoundary node = wrapBoundary Up (fullySubMetaExpr sub parentGSort) node
        pure $ parentBoundary (Gram.Gram (Inject (Grammar.DerivLabel ruleName (fst (endpoints chBackDown))) /\
            (Array.fromFoldable leftKids <> [wrapBoundary Down chBackDown kid] <> Array.fromFoldable rightKids)))
defaultUp _ _ = Nothing