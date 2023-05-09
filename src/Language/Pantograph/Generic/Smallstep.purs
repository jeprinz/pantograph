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

import Data.Gram as Gram
import Partial.Unsafe (unsafeCrashWith)
import Data.Either (Either(..))

data StepLabel l = Inject l | Cursor
type Term l = Gram.Expr (StepLabel l)

type Rule l = Term l -> Maybe (Term l)

-- later Henry can tell me how his definition of path works
type SanePath l = List (Gram.Tooth l)

---------- Code for converting zippers to terms and back ------------------------------------------

addToothToTerm :: forall l. Gram.Tooth l -> Term l -> Term l
addToothToTerm (l /\ ZipList.Path {left, right}) t =
    Gram.Gram (Inject l /\
        ((Array.fromFoldable (map (map Inject) (Rev.unreverse left)))
            <> [t]
            <> (Array.fromFoldable (map (map Inject) right))))

zipperToTerm :: forall l. SanePath l -> Gram.Expr l -> Term l
zipperToTerm Nil exp = Gram.Gram (Cursor /\ [map Inject exp])
zipperToTerm (th : path) exp = addToothToTerm th (zipperToTerm path exp)

assertNoCursor :: forall l. Term l -> Gram.Expr l
assertNoCursor (Gram.Gram (Cursor /\ _)) = unsafeCrashWith "Error: assertNoCursor assertion failed"
assertNoCursor (Gram.Gram (Inject l /\ kids)) = Gram.Gram (l /\ map assertNoCursor kids)

assertNoneOfList :: forall t b. List t -> (t -> Maybe b) -> List b
assertNoneOfList Nil _f = Nil
assertNoneOfList (x : xs) f = case f x of
    Nothing -> unsafeCrashWith "assertNoneOfList: assertion failed"
    Just y -> y : assertNoneOfList xs f

-- assert that exactly one of the children will return a
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

termToZipper :: forall l. Term l -> (SanePath l /\ Gram.Expr l)
termToZipper (Gram.Gram (Cursor /\ [kid])) =
    Nil /\ (assertNoCursor kid)
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

stepSomebody :: forall l. List (Term l) -> List (Rule l) -> Maybe (List (Term l))
stepSomebody Nil _ = Nothing
stepSomebody (t : ts) rules = case step t rules of
    Just t' -> Just (t' : ts)
    Nothing -> (:) <$> pure t <*> stepSomebody ts rules

step :: forall l. Term l -> List (Rule l) -> Maybe (Term l)
step t@(Gram.Gram (l /\ kids)) rules =
    case doAnyApply t rules of
        Nothing -> do
            kids' <- stepSomebody (List.fromFoldable kids) rules
            pure $ Gram.Gram (l /\ Array.fromFoldable kids')
        Just t' -> Just t'
























