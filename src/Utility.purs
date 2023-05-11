module Utility where

import Prelude
import Data.Tuple.Nested
import Bug (bug)

map2 f = map (map f)
infixl 4 map2 as <$$>

map3 f = map (map (map f))
infixl 4 map3 as <$$$>

type Assertion = 
  { condition :: Boolean
  , name :: String
  , source :: String
  , message :: String
  }

renderAssertion :: Assertion -> String
renderAssertion ass = "[" <> ass.source <> "] Failed assertion '" <> ass.name <> "': " <> ass.message

assert :: forall a. Assertion -> (Unit -> a) -> a
assert ass k | ass.condition = k unit
assert ass _ = bug $ renderAssertion ass

assert_ ass = assert ass \_ -> unit

assertM :: forall m a. Applicative m => Assertion -> m Unit
assertM ass | ass.condition = pure unit
assertM ass = bug $ renderAssertion ass
