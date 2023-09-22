module Data.Derivative where

import Data.Tuple.Nested
import Prelude

import Data.Foldable (class Foldable, find)
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafePartial)

-- | A functional typeclass that relates a functor to its derivative.
-- |
-- | Laws:
-- | ```
-- | all (f == _) (\(a /\ f') -> integrate a f') (differentiate f)
-- |
-- | differentiate (integrate a f') `at` a == f'
-- | ```
class (Functor f, Foldable f, Functor f') <= Derivative f f' | f -> f' where
  differentiate :: forall a. f a -> f (a /\ f' a)
  integrate :: forall a. a -> f' a -> f a

at f a = unsafePartial (snd (fromJust (find (fst >>> (a == _)) f)))