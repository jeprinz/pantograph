module Utility where

import Prelude

import Partial.Unsafe (unsafeCrashWith)

map2 f = map (map f)
infixl 4 map2 as <$$>

map3 f = map (map (map f))
infixl 4 map3 as <$$$>

assert :: forall a. Boolean -> String -> (Unit -> a) -> a
assert true _ k = k unit
assert false msg k = unsafeCrashWith $ "assertion failed: " <> msg

assertM :: forall m a. Applicative m => Boolean -> String -> m Unit
assertM true _ = pure unit
assertM false msg = unsafeCrashWith $ "assertion failed: " <> msg
