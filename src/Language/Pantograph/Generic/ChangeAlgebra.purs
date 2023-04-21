module Language.Pantograph.Generic.ChangeAlgebra where

import Prelude
import Data.Tuple.Nested (type (/\), (/\))

--import Language.Pantograph.Generic.Grammar
import Data.Gram
import Data.Array (unzip)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.List.Zip (Path(..))
import Data.Newtype (unwrap)

-- invert :: forall l. Change l -> Change l
-- invert (Gram (l /\ kids)) =
--     let l_inverted = case l of
--             Plus tooth -> Minus tooth
--             Minus tooth -> Plus tooth
--             Expr le -> Expr le
--             Replace e1 e2 -> Replace e2 e1
--     in Gram (l_inverted /\ map invert kids)

-- endpoints :: forall l. Change l -> Expr l /\ Expr l
-- endpoints (Gram (l /\ kids)) =
--     let kids1 /\ kids2 = unzip $ map endpoints kids in
--     case l of
--         Plus ((Path {left, right}) /\ l) ->
--             assertSingleton kids1
--             -- /\ Gram (l /\ {-reverse left <> -} [assertSingleton kids2] {-<> right-})
--             /\ Gram (l /\ {-reverse left <> -} [assertSingleton kids2] {-<> right-})
--         Minus ((Path {left, right}) /\ l) ->
--             Gram (l /\ {-reverse left <> -} [assertSingleton kids1] {-<> right-})
--             /\ assertSingleton kids2
--         Expr le -> Gram (le /\ kids1) /\ Gram (le /\ kids2)
--         Replace e1 e2 -> e1 /\ e2

-- assertSingleton :: forall a . Array a -> a
-- assertSingleton [x] = x
-- assertSingleton _ = unsafeThrow "Error: was not a singleton"


--compose :: forall l. Change l -> Change l -> Change l
--compose (Gram (l1 /\ kids1)) (Gram (l2 /\ kids2)) =
--    case l1 /\ l2 of
