module Language.Pantograph.Generic.ChangeAlgebra where

import Data.Expr
import Language.Pantograph.Generic.Unification
import Prelude
import Util

import Bug (bug)
import Bug as Bug
import Bug.Assertion (Assertion(..), assert, makeAssertionBoolean)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (findMap, foldl, intercalate, and)
import Data.List as List
import Data.List.Rev (unreverse, reverse)
import Data.List.Zip (Path(..))
import Data.List.Zip as ListZip
import Data.List.Zip as ZipList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (trace)
import Debug (traceM)
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML as HH
import Hole as Hole
import Language.Pantograph.Generic.Rendering.Console (logConsole)
import Text.Pretty (pretty)

inject :: forall l. Expr l -> Change l
inject = map Inject

-- HENRY: due to generic fixpoint form of `Gram`, don't need to manually recurse
invert :: forall l. Change l -> Change l
invert = map case _ of
  Plus th -> Minus th
  Minus th -> Plus th
  Inject l -> Inject l
  Replace e1 e2 -> Replace e2 e1

-- NOTE: this is NOT the same as asking if the change has equal endpoints (a loop in the groupoid), it computes if its an identity under composition
isId :: forall l. IsExprLabel l => Change l -> Boolean
isId (Expr (Inject _) kids) = Array.all isId kids
isId (Expr (Replace e1 e2) []) = e1 == e2 -- NOTE: I'm not sure if this should be considered an identity, but if not then something needs to be done about (doOperation (Replace a b) ?x)
isId _ = false

isIdMaybe :: forall l. IsExprLabel l => Change l -> Maybe (Expr l)
isIdMaybe (Expr (Inject l) kids) = Expr l <$> sequence (map isIdMaybe kids)
isIdMaybe (Expr (Replace e1 e2) []) | e1 == e2 = Just e1
isIdMaybe _ = Nothing

collectMatches :: forall l. Eq l => Change l -> MetaExpr l -> Maybe (Map MetaVar (Set (Change l)))
collectMatches (Expr (Inject l1) kids1) (Expr (Meta (Right l2)) kids2) | l1 == l2 =
    let subs = Array.zipWith collectMatches kids1 kids2 in
--    let combine c1 c2 = if isId c1 then Just c2 else if isId c2 then Just c1 else if c1 == c2 then Just c1 else Nothing in
--    let
--    Array.fold subs
    Hole.hole "TODO: collectMatches"
collectMatches c (Expr (Meta (Left x)) []) = Just $ Map.insert x (Set.singleton c) Map.empty
collectMatches _ _ = Bug.bug "base case in collectMatches"

endpoints :: forall l. IsExprLabel l => Change l -> Expr l /\ Expr l
endpoints ch =
--    assert (wellformedExpr "endpoints" ch) \_ ->
    case ch of
        Expr (Plus th) [kid] -> do
            -- - `leftEp` is the left endpoint of the plus's child, and so it is the
            --   plus's left endpoint.
            -- - `rightEp` is the right endpoint of the plus's child, so the plus's
            --   right endpoint is it wrapped in the plus's tooth.
            let leftEp /\ rightEp = endpoints kid
            leftEp /\ unTooth th rightEp
        Expr (Minus th) [kid] -> do
            -- inverse of "plus" case
            let leftEp /\ rightEp = endpoints kid
            unTooth th leftEp /\ rightEp
        Expr (Inject l) kids -> do
            -- `zippedKids` are the endpoint tuples for each of the kids. Unzipping
            -- them yields the array of the kids' left endpoints and the array of the
            -- kids' right endpoints
            let zippedKids = endpoints <$> kids
            let leftKids /\ rightKids = Array.unzip zippedKids
            Expr l leftKids /\ Expr l rightKids
        Expr (Replace e1 e2) [] -> e1 /\ e2
        _ -> bug "invalid input to endpoints"

lEndpoint :: forall l. IsExprLabel l => Change l -> Expr l
lEndpoint = fst <<< endpoints

rEndpoint :: forall l. IsExprLabel l => Change l -> Expr l
rEndpoint = snd <<< endpoints


-- least upper bound
-- actually, I'm not sure we need this.
-- LUB (+ X -> A) (+ Y -> A) -- no unique solution!
-- if you have changes where Plus and Minus DONT cancel each other out, then changes form a category without inverses.
-- this function returns the unique limit where it exists in that category, and returns Nothing if there is no unique solution.
-- TODO: There might be a simpler way to define this function, which is that it's output either only has + or only has -    ???
lub :: forall l. IsExprLabel l => Change l -> Change l -> Maybe (Change l)
lub c1 c2 =
--    trace ("lub called with: c1 is " <> pretty c1 <> " and c2 is " <> pretty c2) \_ ->
--    assert (wellformedExpr "lub.c1" c1) \_ ->
--    assert (wellformedExpr "lub.c2" c2) \_ ->
--    trace ("got here") \_ ->
    case c1 /\ c2 of
        Expr (Inject l1) kids1 /\ Expr (Inject l2) kids2 | l1 == l2 -> Expr (Inject l1) <$> sequence (Array.zipWith lub kids1 kids2) -- Oh no I've become a haskell programmer
        _ | Just out <- plusLub c1 c2 -> pure out
        _ | Just out <- plusLub c2 c1 -> pure out
        _ | Just out <- minusLub c1 c2 -> pure out
        _ | Just out <- minusLub c2 c1 -> pure out
        _ -> Nothing

plusLub :: forall l. IsExprLabel l => Change l -> Change l -> Maybe (Change l)
plusLub c1 c2 | c1 == c2 = Just c1
plusLub (Expr (Plus th) [c1]) c2 = Expr (Plus th) <<< Array.singleton <$> (plusLub c1 c2)
plusLub c1@(Expr (Replace e1 e2) []) c2 | isId c2 = Just c1
plusLub _ _ = Nothing

minusLub :: forall l. IsExprLabel l => Change l -> Change l -> Maybe (Change l)
minusLub c1 c2 | c1 == c2 = Just c1
minusLub (Expr (Minus th@(Tooth l1 p)) [c1]) (Expr (Inject l2) kids)
    | l1 == l2
    , Array.length kids == 1 + ZipList.leftLength p + ZipList.rightLength p =
    Expr (Minus th) <<< Array.singleton <$> (minusLub c1 (fromJust' "minusLub" $ Array.index kids (ZipList.leftLength p)))
minusLub c1@(Expr (Replace e1 e2) []) c2 | isId c2 = Just c1
minusLub _ _ = Nothing

matchingEndpoints :: forall l. IsExprLabel l => String -> String -> Change l -> Change l -> Assertion Unit
matchingEndpoints source message c1 c2 = makeAssertionBoolean
    { name: "matchingEndpoints"
    , source
    , condition: do
        let _left1 /\ right1 = endpoints c1
        let left2 /\ _right2 = endpoints c2
        right1 == left2
    , message
    }

compose :: forall l. IsExprLabel l => Change l -> Change l -> Change l
compose c1 c2 = 
--    assert (wellformedExpr "compose.c1" c1) \_ ->
--    assert (wellformedExpr "compose.c2" c2) \_ ->
--    assert (matchingEndpoints "ChangeAlgebra.compose" ("Change composition is only defined when endpoints match. Changes are: " <> pretty c1 <> " and " <> pretty c2) c1 c2) \_ ->
    case c1 /\ c2 of
        (Expr (Plus l1) [c1']) /\ (Expr (Minus l2) [c2']) | l1 == l2 -> compose c1' c2'
        (Expr (Minus l1) [c1']) /\ (Expr (Plus l2) [c2']) | l1 == l2 ->
            let Tooth l (Path {left, right}) = l1 in
            Expr (Inject l) $
                (Array.fromFoldable $ map (map Inject) $ unreverse left) <> 
                [compose c1' c2'] <>
                (Array.fromFoldable $ map (map Inject) $ right)
        _ /\ (Expr (Plus l) [c2']) -> Expr (Plus l) [compose c1 c2']
        (Expr (Minus l) [c1']) /\ _ -> Expr (Minus l) [compose c1' c2]
        (Expr (Plus th@(Tooth l1 p)) [c1']) /\ (Expr (Inject l2) kids2)
            | l1 == l2
            , p2 /\ kid <- fromJust' "compose" (ZipList.zipAt (ZipList.leftLength p) (List.fromFoldable kids2))
            , and (List.zipWith (\e c -> inject e == c) (ZipList.unpath p)
                (ZipList.unpath p2)) ->
            Expr (Plus th) [compose c1' kid]
        (Expr (Inject l2) kids1) /\ (Expr (Minus th@(Tooth l1 p)) [c2'])
            | l1 == l2
            , p1 /\ kid <- fromJust' "compose" (ZipList.zipAt (ZipList.leftLength p) (List.fromFoldable kids1))
            , and (List.zipWith (\e c -> inject e == c) (ZipList.unpath p)
                (ZipList.unpath p1)) ->
            Expr (Minus th) [compose kid c2']
        (Expr (Inject l1) kids1) /\ (Expr (Inject l2) kids2) | l1 == l2 ->
            Expr (Inject l1) (Array.zipWith compose kids1 kids2)
        _ -> do
            let left1 /\ _right1 = endpoints c1
            let _left2 /\ right2 = endpoints c2
            Expr (Replace left1 right2) []

{-
I don't have a good name for this operation, but what it does is:
input Change c1 and MetaChange c2, and output sub and c3, such that:
c1 o c3 = sub c2
Also, c3 should be orthogonal to c1. If this doesn't exist, it outputs Nothing.
(Note that c2 has metavariables in the change positions, so its (Expr (Meta (ChangeLabel l))))
-}

doOperation :: forall l. IsExprLabel l => Change l -> Expr (Meta (ChangeLabel l)) -> Maybe (Map MetaVar (Change l) /\ Change l)
doOperation c1 c2 =
--    trace ("doOperation called with c1 = " <> pretty c1 <> " and c2 = " <> pretty c2) \_ ->
    do
    matches <- getMatches c2 c1
--    traceM ("matches is: " <> pretty matches)
    -- TODO: could this be written better
    let sub = map (foldNonempty (\c1 c2 -> do x <- c1
                                              y <- c2
--                                              traceM ("calling lub, c1 is " <> pretty c1 <> " and c2 is " <> pretty c2 <> " result is " <> pretty (lub x y))
                                              lub x y))
                (map (Set.map Just) matches)
--    traceM ("sub is: " <> pretty sub)
    sub2 <- sequence sub
--    traceM ("sub2 is: " <> pretty sub2)
    let subc2 = subMetaExpr sub2 c2
--    traceM ("subc2 is: " <> pretty subc2)
--    traceM ("invert c1 is" <> pretty (invert c1))
    let result = (sub2 /\ compose (invert c1) subc2)
--    traceM ("final result is " <> pretty result)
    pure $ result

{-
Implementing a real tree diff algorithm is hard, so instead I have one that makes some assumptions about the inputs.
Its also dubious if the notion of "shortest edit sequence" is really what we want anyway. Would that really be the
change that correctly preserves the semantic meaning?
This diff algorithm tries to find an unambiguous diff, and if it doesn't exist just returns Replace.
In other words, the set S of pairs of expressions (e1, e2) on which the algorithm deals doesn't just return Replace
consists of pairs satisfying any of the following:
- e1 = e2
- e1 is a subexpression of e2
- e2 is a subexpression of e1
- e1 = Expr l1 [a1, ..., an], e2 = Expr l2 [b1, ..., bn], and for each i<=n, (ai, bi) in S.
-}
diff :: forall l. Eq l => Expr l -> Expr l -> Change l
diff e1 e2 | e1 == e2 = map Inject e1
diff e1@(Expr l1 kids1) e2@(Expr l2 kids2) =
    case isPostfix e1 e2 of
        Just ch -> ch
        Nothing -> case isPostfix e2 e1 of
                        Just ch -> invert ch
                        Nothing -> if l1 == l2 then Expr (Inject l1) (Array.zipWith diff kids1 kids2) else Expr (Replace e1 e2) []

isPostfix :: forall l. Eq l => Expr l -> Expr l -> Maybe (Change l)
isPostfix e1 e2 | e1 == e2 = Just $ map Inject e1
isPostfix (Expr l kids) e2 =
-- TODO: this can probably be rewritten with utilities in Zip.purs like zipAt and zips
    let splits = Array.mapWithIndex (\index kid -> Array.take index kids /\ kid /\ Array.drop (index + 1) kids) kids in
    findMap (\(leftKids /\ kid /\ rightKids) ->
        do
            innerCh <- isPostfix kid e2
            Just $ Expr (Minus (Tooth l (Path {left: reverse $ List.fromFoldable leftKids, right: List.fromFoldable rightKids}))) [innerCh]
          ) splits

subSomeChangeLabel :: forall l. IsExprLabel l => Sub l -> ChangeLabel (Meta l) -> ChangeLabel (Meta l)
subSomeChangeLabel sub =
  let subExpr = subMetaExprPartially sub in
  case _ of
      Plus (Tooth dir (ZipList.Path {left, right})) -> Plus (Tooth dir (ZipList.Path {left: map subExpr left, right: map subExpr right}))
      Minus (Tooth dir (ZipList.Path {left, right})) -> Minus (Tooth dir (ZipList.Path {left: map subExpr left, right: map subExpr right}))
      Inject l -> Inject l -- NOTE: if l was a metavar, we wouldn't get here because subSomeMetaChange would have dealt with it.
      Replace e1 e2 -> Replace (subExpr e1) (subExpr e2)

-- TODO: I need to figure out how this function can really be written without repetition relative to other substitution functions we have in Expr
subSomeMetaChange :: forall l. IsExprLabel l => Sub l -> MetaChange l -> MetaChange l
subSomeMetaChange sub (Expr l kids) =
    case l of
--        Inject (Meta (Left x)) -> inject $ lookup' x sub
        Inject (Meta (Left x)) | Just s <- Map.lookup x sub
            -> inject s
        _ -> Expr (subSomeChangeLabel sub l) (map (subSomeMetaChange sub) kids)
