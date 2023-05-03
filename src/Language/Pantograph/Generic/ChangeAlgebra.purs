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

-- HENRY: due to generic fixpoint form of `Gram`, don't need to manually recurse
invert :: forall l. Change l -> Change l
invert = map case _ of
  Plus th -> Minus th
  Minus th -> Plus th
  Expr l -> Expr l
  Replace e1 e2 -> Replace e2 e1

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
lub :: forall l. Eq l => Change l -> Change l -> Maybe (Change l)
lub (Gram (Expr l1 /\ kids1)) (Gram (Expr l2 /\ kids2)) | l1 == l2
    = Gram <$> ((/\) <$> (pure $ Expr l1) <*> (sequence (lub <$> kids1 <*> kids2))) -- Oh no I've become a haskell programmer
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
