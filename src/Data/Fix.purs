module Data.Fix where

import Prelude

import Data.Traversable (class Traversable, traverse)
import Text.Pretty (class Pretty, class Pretty1, pretty1)

-- | The fixpoint of a 1-argument type constructor.
data Fix (f :: Type -> Type) = Fix (f (Fix f))

-- | Maps over each layer of the fixpoint.
mapFix :: forall f f'. Functor f => (forall a. f a -> f' a) -> Fix f -> Fix f'
mapFix f (Fix ff) = Fix (f (map (mapFix f) ff))

-- | Traverses over each layer of the fixpoint.
traverseFix :: forall f g m. Traversable f => Monad m => (forall a. f a -> m (g a)) -> Fix f -> m (Fix g)
traverseFix f (Fix ff) = Fix <$> join (f <$> (traverse (traverseFix f) ff))

-- | Unwrap the `Fix` wrapper.
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix ff) = ff

instance Pretty1 f => Pretty (Fix f) where
  pretty (Fix f) = pretty1 f
