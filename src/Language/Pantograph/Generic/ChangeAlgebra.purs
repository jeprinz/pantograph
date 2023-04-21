module Language.Pantograph.Generic.ChangeAlgebra where

import Data.Gram
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

-- HENRY: Unfortunately, can't use `matchChangeNode` or `foldMapGram` since you
--        want to do custom pattern matching. But it's not _so_ bad.
compose :: forall l. Change l -> Change l -> Change l
compose (Gram ((Plus _) /\ [kid1])) (Gram ((Plus _) /\ [kid2])) = unsafeCrashWith "!TODO"
compose (Gram ((Plus _) /\ [kid1])) (Gram ((Minus _) /\ [kid2])) = unsafeCrashWith "!TODO"
compose (Gram ((Plus _) /\ [kid1])) (Gram ((Expr _) /\ kids2)) = unsafeCrashWith "!TODO"
compose (Gram ((Plus _) /\ [kid1])) (Gram ((Replace _ _) /\ [])) = unsafeCrashWith "!TODO"

compose (Gram ((Minus _) /\ [kid1])) (Gram (ch2 /\ kids2)) = unsafeCrashWith "!TODO"

compose (Gram ((Expr _) /\ kids1)) (Gram (ch2 /\ kids2)) = unsafeCrashWith "!TODO"

compose (Gram ((Replace _ _) /\ [])) (Gram ((Plus _) /\ [kid2])) = unsafeCrashWith "!TODO"
compose (Gram ((Replace _ _) /\ [])) (Gram ((Minus _) /\ [kid2])) = unsafeCrashWith "!TODO"
compose (Gram ((Replace _ _) /\ [])) (Gram ((Expr _) /\ kids2)) = unsafeCrashWith "!TODO"
compose (Gram ((Replace e1 e2) /\ [])) (Gram ((Replace e2' e3) /\ [])) = unsafeCrashWith "!TODO assert that e2 == e3'; yield Replace e1 e3"

compose ch1 ch2 = unsafeCrashWith $ intercalate "\n"
  [ "[ChangeAlgebra.compose]: invalid composition:",
    "  ch1 = " <> showGramStructure ch1,
    "  ch2 = " <> showGramStructure ch2
  ]
