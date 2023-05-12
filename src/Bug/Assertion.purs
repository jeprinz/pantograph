module Bug.Assertion where

import Prelude

import Bug (bug)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

newtype Assertion a = Assertion 
  { name :: String
  , source :: String
  , result :: String \/ a }

renderFailedAssertion :: forall a. Assertion a -> String -> String
renderFailedAssertion (Assertion ass) msg = "[" <> ass.source <> "] Failed assertion '" <> ass.name <> "': " <> msg

makeAssertionBoolean :: 
  { name :: String
  , source :: String
  , condition :: Boolean
  , message :: String } ->
  Assertion Unit
makeAssertionBoolean {name, source, condition, message} = Assertion
  { name, source
  , result: 
      if condition 
        then Right unit
        else Left message
  }

assert :: forall a b. Assertion a -> (Partial => a -> b) -> b
assert (Assertion ass) = \k -> case ass.result of
  Right a -> unsafePartial (k a)
  Left msg -> bug $ renderFailedAssertion (Assertion ass) msg

assert_ :: forall a. Assertion a -> Unit
assert_ ass = assert ass \_ -> unit

assertM :: forall m a. Applicative m => Assertion a -> m a
assertM (Assertion ass) = case ass.result of
  Right a -> pure a
  Left msg -> bug $ renderFailedAssertion (Assertion ass) msg

assertInterface :: forall a b za zb. (a -> Assertion za) -> (b -> Assertion zb) -> (Partial => a -> b) -> (a -> b)
assertInterface ass_a ass_b = \f a -> 
  assert (ass_a a) \_ ->
  let b = f a in
  assert (ass_b b) \_ ->
  b

assertInput :: forall a b z. (a -> Assertion z) -> (Partial => a -> b) -> (a -> b)
assertInput ass_a = \f a -> assert (ass_a a) \_ -> f a

assertOutput :: forall a b z. (b -> Assertion z) -> (Partial => a -> b) -> (a -> b)
assertOutput ass_b = \f a -> unsafePartial
  let b = f a in
  assert (ass_b b) \_ ->
  b

try :: forall a. Assertion a -> Maybe a
try (Assertion ass) = ass.result # either (const Nothing) Just

positif source x = makeAssertionBoolean
  { name: "positif"
  , source
  , condition: 0 <= x
  , message: "A positif number must be greater than or equal to 0."
  }
