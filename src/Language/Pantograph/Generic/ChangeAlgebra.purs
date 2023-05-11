module Language.Pantograph.Generic.ChangeAlgebra where

import Data.Gram
import Prelude

import Data.Array (unzip, fromFoldable)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List.Zip (Path(..))
import Data.List.Zip as ListZip
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Data.List.Rev (unreverse)
import Data.Array (all)
import Data.Map (Map)
import Data.Map as Map
import Data.Unify (MetaVar)
import Data.Either (Either(..))
import Data.Unify (Meta(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Language.Pantograph.Generic.Grammar as Grammar
import Util
import Language.Pantograph.Generic.Unification
import Data.Foldable (foldl)

-- HENRY: due to generic fixpoint form of `Gram`, don't need to manually recurse
invert :: forall l. Change l -> Change l
invert = map case _ of
  Plus th -> Minus th
  Minus th -> Plus th
  Expr l -> Expr l
  Replace e1 e2 -> Replace e2 e1

-- NOTE: this is NOT the same as asking if the change has equal endpoints (a loop in the groupoid), it computes if its an identity under composition
isId :: forall l. Change l -> Boolean
isId (Gram (Expr _l /\ kids)) = all isId kids
isId _ = false

collectMatches :: forall l. Eq l => Change l -> MetaExpr l -> Maybe (Map MetaVar (Set (Change l)))
collectMatches (Gram (Expr l1 /\ kids1)) (Gram (Meta (Right l2) /\ kids2)) | l1 == l2 =
    let subs = collectMatches <$> kids1 <*> kids2 in
--    let combine c1 c2 = if isId c1 then Just c2 else if isId c2 then Just c1 else if c1 == c2 then Just c1 else Nothing in
--    let
--    Array.fold subs
    unsafeCrashWith "TODO"
collectMatches c (Gram (Meta (Left x) /\ [])) = Just $ Map.insert x (Set.singleton c) Map.empty
collectMatches _ _ = unsafeCrashWith "no"

--  = Plus (Tooth l) {-one kid - whatever fits inside the tooth-}
--  | Minus (Tooth l) {-one kid - whatever fits inside the tooth-}
--  | Expr l {-same number of kids that l has-}
--  | Replace (Expr l) (Expr l) {-zero kids?-}
-- not sure how to generalize this correctly, for now I'm writing this
--mEndpoints :: forall l. Grammar.MetaChange l -> MetaExpr l /\ MetaExpr l
--mEndpoints (Gram (Meta l /\ kids)) =
--    case l of
--        Left x -> let e = Gram (Meta (Left x) /\ []) in e /\ e
--        Right chL -> case chL of
--            Plus (l /\ th) ->
--                let leftEp /\ rightEp = mEndpoints (assertSingleton kids) in
--                unTooth (Meta (Right l) /\ (map (map (Meta <<< Right)) th)) leftEp /\ rightEp
--            Minus (l /\ th) ->
--                let leftEp /\ rightEp = mEndpoints (assertSingleton kids) in
--                leftEp /\ unTooth (Meta (Right l) /\ (map (map (Meta <<< Right)) th)) rightEp
--            Expr l ->
--                let leftKids /\ rightKids = unzip (map mEndpoints kids) in
--                expr (Meta (Right l)) leftKids /\ expr (Meta (Right l)) rightKids
--            Replace t1 t2 -> (map (\x -> Meta (Right x)) t1) /\ map (\x -> Meta (Right x)) t2

endpoints :: forall l. Change l -> Expr l /\ Expr l
endpoints = foldMapGram $ flip matchChangeNode
  { plus: \th (leftEp /\ rightEp) ->
      -- - `leftEp` is the left endpoint of the plus's child, and so it is the
      --   plus's left endpoint.
      -- - `rightEp` is the right endpoint of the plus's child, so the plus's
      --   right endpoint is it wrapped in the plus's tooth.
      leftEp /\ unTooth th rightEp
  , minus: \th (leftEp /\ rightEp) ->
      -- inverse of "plus" case
      unTooth th leftEp /\ rightEp
  , expr: \l zippedKids ->
      -- `zippedKids` are the endpoint tuples for each of the kids. Unzipping
      -- them yields the array of the kids' left endpoints and the array of the
      -- kids' right endpoints
      let leftKids /\ rightKids = unzip zippedKids in
      expr l leftKids /\ expr l rightKids
  , replace: \e1 e2 -> e1 /\ e2
  }

-- least upper bound
-- actually, I'm not sure we need this.
-- LUB (+ X -> A) (+ Y -> A) -- no unique solution!
-- if you have changes where Plus and Minus DONT cancel each other out, then changes form a category without inverses.
-- this function returns the unique limit where it exists in that category, and returns Nothing if there is no unique solution.
lub :: forall l. Eq l => Change l -> Change l -> Maybe (Change l)
lub (Gram (Expr l1 /\ kids1)) (Gram (Expr l2 /\ kids2)) | l1 == l2
    = Gram <$> ((/\) <$> (pure $ Expr l1) <*> (sequence (lub <$> kids1 <*> kids2))) -- Oh no I've become a haskell programmer
lub (Gram (Plus _ /\ _)) (Gram (Plus _ /\ _)) = Nothing
lub (Gram (Plus _ /\ _)) (Gram (Minus _ /\ _)) = Nothing
lub (Gram (Minus _ /\ _)) (Gram (Plus _ /\ _)) = Nothing
lub (Gram (Minus _ /\ _)) (Gram (Minus _ /\ _)) = Nothing
lub _ _ = unsafeCrashWith "TODO"

-- HENRY: Unfortunately, can't use `matchChangeNode` or `foldMapGram` since you
--        want to do custom pattern matching. But it's not _so_ bad.
compose :: forall l. Eq l => Change l -> Change l -> Change l
compose (Gram ((Plus l1) /\ [c1])) (Gram ((Minus l2) /\ [c2])) | l1 == l2 = compose c1 c2
compose (Gram ((Minus l1) /\ [c1])) (Gram ((Plus l2) /\ [c2])) | l1 == l2 =
    let (l /\ Path {left, right}) = l1 in
    let toArr exprs = fromFoldable $ map (map Expr) exprs in
    Gram ((Expr l) /\ ((toArr (unreverse left)) <> [compose c1 c2] <> (toArr right)))
compose c1 (Gram ((Plus l) /\ [c2])) = Gram (Plus l /\ [compose c1 c2])
compose (Gram ((Minus l) /\ [c1])) c2 = Gram (Minus l /\ [compose c1 c2])
compose (Gram ((Expr l1) /\ kids1)) (Gram ((Expr l2) /\ kids2)) | l1 == l2 = Gram (Expr l1 /\ (compose <$> kids1 <*> kids2))
compose c1 c2 =
    let (left1 /\ right1) = endpoints c1 in
    let (left2 /\ right2) = endpoints c2 in
    if not (right1 == left2) then
      unsafeCrashWith $ intercalate "\n"
      [ "[ChangeAlgebra.compose]: invalid composition, compose is only valid when endpoints match:",
        "  ch1 = " <> showGramStructure c1,
        "  ch2 = " <> showGramStructure c2
      ]
    else Gram (Replace left1 right2 /\ [])


{-
I don't have a good name for this operation, but what it does is:
input Change c1 and MetaChange c2, and output sub and c3, such that:
c1 o c3 = sub c2
Also, c3 should be orthogonal to c1. If this doesn't exist, it outputs Nothing.
(Note that c2 has metavariables in the change positions, so its (Expr (Meta (ChangeLabel l))))
-}

doOperation :: forall l. Eq l => Ord l => Change l -> Expr (Meta (ChangeLabel l)) -> Maybe (Map MetaVar (Change l) /\ Change l)
doOperation c1 c2 = do
    matches <- getMatches c2 c1
    -- TODO: could this be written better
    let sub = map (foldNonempty (\c1 c2 -> do x <- c1
                                              y <- c2
                                              lub x y))
                (map (Set.map Just) matches)
    sub2 <- sequence sub
    let subc2 = fullySubMetaExpr sub2 c2
    pure $ (sub2 /\ compose (invert c1) subc2)