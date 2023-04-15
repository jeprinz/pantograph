module Control.Annotation where

import Prelude

class
  Functor f <= Annotation f where
  unannotate :: forall a. f a -> a
  annotateDefault :: forall a. a -> f a
