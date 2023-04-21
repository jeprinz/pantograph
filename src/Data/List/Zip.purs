module Data.List.Zip where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldl, foldr, intercalate)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as List
import Data.List.Rev ((:*))
import Data.List.Rev as Rev
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Text.Pretty ((<+>))

newtype Tooth a = Zip {path :: Path a, focus :: a}

derive instance Generic (Tooth a) _ 
instance Show a => Show (Tooth a) where show x = genericShow x
derive instance Eq a => Eq (Tooth a)
derive instance Functor Tooth
instance Foldable Tooth where
  foldMap f = foldMap f <<< unzip
  foldl f b = foldl f b <<< unzip
  foldr f b = foldr f b <<< unzip
instance Traversable Tooth where
  traverse f (Zip z) = (\path focus -> Zip {path, focus})   
    <$> traverse f z.path
    <*> f z.focus
  sequence = traverse identity

unzip (Zip z) = unpathAroundList (pure z.focus) z.path

-- | The type of a paths into lists.
newtype Path a = Path {left :: Rev.List a, right :: List.List a}

derive instance Generic (Path a) _
instance Show a => Show (Path a) where show x = genericShow x
derive instance Eq a => Eq (Path a)
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path
instance Semigroup (Path a) where append (Path d1) (Path d2) = Path {left: d1.left <> d2.left, right: d1.right <> d2.right}
instance Monoid (Path a) where mempty = Path {left: mempty, right: mempty}

instance FunctorWithIndex Int Path where 
  mapWithIndex f (Path d) = Path d {left = mapWithIndex f d.left, right = mapWithIndex (\i -> f (i + l)) d.right}
    where l = Rev.length d.left
instance FoldableWithIndex Int Path where
  foldMapWithIndex f = foldMapWithIndex f <<< unpath
  foldrWithIndex f b = foldrWithIndex f b <<< unpath
  foldlWithIndex f b = foldlWithIndex f b <<< unpath
instance TraversableWithIndex Int Path where
  traverseWithIndex f (Path d) =
    let l = Rev.length d.left in
    (\left right -> Path d {left = left, right = right})
      <$> traverseWithIndex f d.left
      <*> traverseWithIndex (\i -> f (i + l)) d.right

lengthLeft (Path p) = Rev.length p.left
lengthRight (Path p) = List.length p.right

appendLeft :: forall a. a -> Path a -> Path a
appendLeft a (Path d) = Path d {left = d.left :* a}

appendRight :: forall a. a -> Path a -> Path a
appendRight a (Path d) = Path d {right = a : d.right}

unpath :: forall a. Path a -> List.List a
unpath (Path d) = Rev.unreverse d.left <> d.right

unpathAround :: forall a. a -> Path a -> List.List a
unpathAround x = unpathAroundList (List.singleton x)

unpathAroundList :: forall a. List.List a -> Path a -> List.List a
unpathAroundList xs (Path d) = Rev.unreverse d.left <> xs <> d.right

singletonLeft :: forall a. a -> Path a
singletonLeft a = appendLeft a mempty

singletonRight :: forall a. a -> Path a
singletonRight a = appendRight a mempty

unsnocLeft :: forall a. Path a -> Maybe {init :: Path a , last :: a}
unsnocLeft (Path d) = Rev.unsnoc d.left <#> \{init, last} -> {init: Path d {left = init}, last}

unconsRight (Path d) = List.uncons d.right <#> \{head, tail} -> {head, tail: Path d {right = tail}}

showPath :: Path String -> String -> String
showPath (Path d) str = intercalate " " d.left <+> str <+> intercalate " " d.right