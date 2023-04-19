module Data.Fix where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)

newtype Fix f = Fix (f (Fix f))

derive instance Generic (Fix f) _
derive instance Newtype (Fix f) _
derive instance Eq1 f => Eq (Fix f)
derive instance Ord1 f => Ord (Fix f)


mapFix :: forall f. Functor f => (f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
mapFix f (Fix g) = Fix $ f $ mapFix f <$> g

foldMapFix :: forall f m. Functor f => Foldable f => Monoid m => (f m -> m) -> Fix f -> m
foldMapFix f (Fix g) = f $ foldMapFix f <$> g

traverseFix :: forall f m. Traversable f => Applicative m => (f (m (Fix f)) -> m (f (Fix f))) -> Fix f -> m (Fix f)
traverseFix k (Fix g) = Fix <$> k (traverseFix k <$> g)
