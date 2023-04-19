module Data.List.Zip where

import Data.Tuple.Nested
import Prelude
import Text.Pretty ((<+>))
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable, bitraverse)
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

-- -- | The type of zippers of lists.
-- newtype ToothH a b = ZipH {path :: Path a, focus :: b}

-- derive instance Generic (ToothH a b) _
-- instance (Show a, Show b) => Show (ToothH a b) where show x = genericShow x
-- derive instance (Eq a, Eq b) => Eq (ToothH a b)
-- derive instance Bifunctor ToothH

-- instance Bifoldable ToothH where
--   bifoldr f g m (ZipH z@{path: Path p}) = flip (foldr f) p.left $ g z.focus $ flip (foldr f) p.right m
--   bifoldl f g m (ZipH z@{path: Path p}) = flip (foldl f) p.left $ flip g z.focus $ flip (foldl f) p.right m
--   bifoldMap f g (ZipH {path, focus}) = foldMap identity $ unpathAround (pure (g focus)) $ f <$> path

-- instance Bitraversable ToothH where 
--   bitraverse f g (ZipH z@{path: Path p}) = (\left focus right -> ZipH {path: Path {left, right}, focus})
--     <$> traverse f p.left
--     <*> g z.focus
--     <*> traverse f p.right
--   bisequence = bitraverse identity identity

-- unzipH (ZipH z) = unpathAround (List.singleton z.focus) z.path

-- showToothH (ZipH z) = showPath z.path z.focus

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

unzip (Zip z) = unpathAround (pure z.focus) z.path

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

unpathAround :: forall a. List.List a -> Path a -> List.List a
unpathAround xs (Path d) = Rev.unreverse d.left <> xs <> d.right

singletonLeft :: forall a. a -> Path a
singletonLeft a = appendLeft a mempty

singletonRight :: forall a. a -> Path a
singletonRight a = appendRight a mempty

unsnocLeft :: forall a. Path a -> Maybe {init :: Path a , last :: a}
unsnocLeft (Path d) = Rev.unsnoc d.left <#> \{init, last} -> {init: Path d {left = init}, last}

unconsRight :: forall a168. Path a168 -> Maybe {head :: a168 , tail :: Path a168}
unconsRight (Path d) = List.uncons d.right <#> \{head, tail} -> {head, tail: Path d {right = tail}}

showPath (Path d) str = intercalate " " d.left <+> str <+> intercalate " " d.right