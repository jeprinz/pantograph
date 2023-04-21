module Language.Pantograph.Generic.ChangeAlgebra where

import Data.Gram
import Prelude

import Data.Array (unzip)
import Data.Array as Array
import Data.List.Zip (Path(..))
import Data.List.Zip as ListZip
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception.Unsafe (unsafeThrow)

-- HENRY: due to generic fixpoint form of `Gram`, don't need to manually recurse
invert :: forall l. Change l -> Change l
invert = map case _ of
  Plus th -> Minus th
  Minus th -> Plus th
  Expr l -> Expr l
  Replace e1 e2 -> Replace e2 e1

endpoints :: forall l. Change l -> Expr l /\ Expr l
endpoints = foldMapGram $ flip matchChangeNode
  { plus: \(l /\ p) (leftEp /\ rightEp) ->
      -- `leftEp` is the left endpoint of the plus's child, and so it is the
      -- plus's left endpoint
      leftEp /\ 
      -- `rightEp` is the right endpoint of the plus's child, so the plus's
      -- right endpoint is it wrapped in the plus's tooth
      expr l (Array.fromFoldable (ListZip.unpathAround rightEp p))
  , minus: \(l /\ p) (leftEp /\ rightEp) ->
      expr l (Array.fromFoldable (ListZip.unpathAround leftEp p)) /\
      rightEp
  , expr: \l zippedKids ->
      -- `zippedKids` are the endpoint pairs for each of the kids, unzipping
      -- them yields the array of the kids' left endpoints and the array of the
      -- kids' right endpoints
      let leftKids /\ rightKids = unzip zippedKids in
      Gram (l /\ leftKids) /\ Gram (l /\ rightKids)
  , replace: \e1 e2 -> e1 /\ e2
  }

ex_ch1 :: Change String
ex_ch1 = exprChange "A" [ exprChange "B" [], exprChange "C" [] ]

ex_ex2 :: Expr String
ex_ex2 = expr "A" []

ex_ex1 :: Expr String
ex_ex1 = expr "A" [ expr "B" [], expr "C" [] ]

-- -- Unfortunately, can't use `matchChangeNode` or `foldMapGram` since you want to
-- -- do custom pattern matching. But it's not _so_ bad.
-- compose :: forall l. Change l -> Change l -> Change l
-- compose (Gram (ch1 /\ kids1)) (Gram (ch2 /\ kids2)) = ?a

-- --compose :: forall l. Change l -> Change l -> Change l
-- --compose (Gram (l1 /\ kids1)) (Gram (l2 /\ kids2)) =
-- --    case l1 /\ l2 of
