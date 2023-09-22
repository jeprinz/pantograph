module Data.Derivative where

import Data.Tuple.Nested
import Prelude

import Data.Foldable (class Foldable)

-- | A functional typeclass that relates a functor to its derivative.
-- |
-- | Laws:
-- | ```
-- | map (\(a /\ f') -> (a /\ integrate a)) (differentiate f) =
-- | map (\a -> (a /\ f)) f
-- |
-- | foldr (\(a' /\ f') -> maybe (if a == a' then Just f' else Nothing) Just) Nothing (differentiate (integrate a f')) =
-- | Just f'
-- | ```
class (Functor f, Foldable f, Functor f') <= Derivative f f' | f -> f' where
  differentiate :: forall a. f a -> f (a /\ f' a)
  integrate :: forall a. a -> f' a -> f a

