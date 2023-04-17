module Data.List.Drv where

import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List as List
import Data.List.Rev (RevList, unreverse, (:*))
import Data.List.Rev as Rev
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)

-- | The type of a derivatives of lists.
newtype DrvList a = DrvList {left :: RevList a, right :: List a}

derive instance Generic (DrvList a) _
instance Show a => Show (DrvList a) where show x = genericShow x
derive instance Eq a => Eq (DrvList a)
derive instance Functor DrvList
derive instance Foldable DrvList
derive instance Traversable DrvList
instance Semigroup (DrvList a) where append (DrvList d1) (DrvList d2) = DrvList {left: d1.left <> d2.left, right: d1.right <> d2.right}
instance Monoid (DrvList a) where mempty = DrvList {left: mempty, right: mempty}

appendLeft :: forall a. a -> DrvList a -> DrvList a
appendLeft a (DrvList d) = DrvList d {left = d.left :* a}

appendRight :: forall a. a -> DrvList a -> DrvList a
appendRight a (DrvList d) = DrvList d {right = a : d.right}

underive :: forall a. DrvList a -> List a
underive (DrvList d) = unreverse d.left <> d.right

underiveAround :: forall a. DrvList a -> List a -> List a
underiveAround (DrvList d) xs = unreverse d.left <> xs <> d.right

singletonLeft :: forall a. a -> DrvList a
singletonLeft a = appendLeft a mempty

singletonRight :: forall a. a -> DrvList a
singletonRight a = appendRight a mempty

unsnocLeft :: forall a. DrvList a -> Maybe {init :: DrvList a , last :: a}
unsnocLeft (DrvList d) = Rev.unsnoc d.left <#> \{init, last} -> {init: DrvList d {left = init}, last}

unconsRight :: forall a168. DrvList a168 -> Maybe {head :: a168 , tail :: DrvList a168}
unconsRight (DrvList d) = List.uncons d.right <#> \{head, tail} -> {head, tail: DrvList d {right = tail}}
