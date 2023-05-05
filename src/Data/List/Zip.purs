module Data.List.Zip where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Control.Plus (class Plus)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, intercalate)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Rev (RevList, (:*))
import Data.List.Rev as Rev
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, traverse)
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
newtype Path a = Path {left :: RevList a, right :: List.List a}

derive instance Newtype (Path a) _
derive instance Generic (Path a) _
instance Show a => Show (Path a) where show x = genericShow x
derive instance Eq a => Eq (Path a)
derive instance Ord a => Ord (Path a)
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path
instance Semigroup (Path a) where append (Path d1) (Path d2) = Path {left: d1.left <> d2.left, right: d1.right <> d2.right}
instance Monoid (Path a) where mempty = Path {left: mempty, right: mempty}
-- instance (Applicative m, Plus m, Unify m a) => Unify m (Path a) where unify (Path {left: l1, right: r1}) (Path {left: l2, right: r2}) = (\left right -> Path {left, right}) <$> unify l1 l2 <*> unify r1 r2

-- !TODO is this used anywhere?
-- instance FunctorWithIndex Int Path where 
--   mapWithIndex f (Path d) = Path d {left = mapWithIndex f d.left, right = mapWithIndex (\i -> f (i + l)) d.right}
--     where l = Rev.length d.left
-- instance FoldableWithIndex Int Path where
--   foldMapWithIndex f = foldMapWithIndex f <<< unpath
--   foldrWithIndex f b = foldrWithIndex f b <<< unpath
--   foldlWithIndex f b = foldlWithIndex f b <<< unpath
-- instance TraversableWithIndex Int Path where
--   traverseWithIndex f (Path d) =
--     let l = Rev.length d.left in
--     (\left right -> Path d {left = left, right = right})
--       <$> traverseWithIndex f d.left
--       <*> traverseWithIndex (\i -> f (i + l)) d.right

leftLength (Path p) = Rev.length p.left
rightLength (Path p) = List.length p.right

appendLeft :: forall a. a -> Path a -> Path a
appendLeft a (Path d) = Path d {left = d.left :* a}

appendRight :: forall a. a -> Path a -> Path a
appendRight a (Path d) = Path d {right = a : d.right}

-- left :: forall a. Path a -> RevList a
-- left = unwrap >>> _.left

-- right :: forall a. Path a -> List.List a
-- right = unwrap >>> _.right

unpath :: forall a. Path a -> List.List a
unpath (Path d) = Rev.unreverse d.left <> d.right

unpathAround :: forall a. a -> Path a -> List.List a
unpathAround x = unpathAroundList (List.singleton x)

unpathAroundList :: forall a. List.List a -> Path a -> List.List a
unpathAroundList xs (Path d) = Rev.unreverse d.left <> xs <> d.right

zipAt :: forall a. Int -> List.List a -> Maybe (Path a /\ a)
zipAt = go mempty
  where
  go _ _ Nil = Nothing
  go left 0 (Cons x right) = Just (Path {left, right} /\ x)
  go left n (Cons x right) = go (Rev.snoc left x) (n - 1) right

singletonLeft :: forall a. a -> Path a
singletonLeft a = appendLeft a mempty

singletonRight :: forall a. a -> Path a
singletonRight a = appendRight a mempty

unsnocLeft :: forall a. Path a -> Maybe {init :: Path a , last :: a}
unsnocLeft (Path d) = Rev.unsnoc d.left <#> \{init, last} -> {init: Path d {left = init}, last}

unconsRight :: forall a429.
  Path a429
  -> Maybe
       { head :: a429
       , tail :: Path a429
       }
unconsRight (Path d) = List.uncons d.right <#> \{head, tail} -> {head, tail: Path d {right = tail}}

zipLeft :: forall a. (a /\ Path a) -> Maybe (a /\ Path a)
zipLeft (a /\ Path p) = do
  {init: left', last: a'} <- Rev.unsnoc p.left
  Just $ a' /\ Path {left: left', right: Cons a p.right}

zipRight :: forall a. (a /\ Path a) -> Maybe (a /\ Path a)
zipRight (a /\ Path p) = do
  {head: a', tail: right'} <- List.uncons p.right
  Just $ a' /\ Path {left: Rev.snoc p.left a, right: right'}

showPath :: Path String -> String -> String
showPath (Path d) str = intercalate " " d.left <+> str <+> intercalate " " d.right

-- left inside, right outside 
foldrAround :: forall a b. (a -> b -> b) -> b -> (b -> b) -> Path a -> b
foldrAround f b mid (Path d) = foldr f (mid (foldr f b d.left)) d.right

-- left outside, right inside
foldlAround :: forall a b. (b -> a -> b) -> b -> (b -> b) -> Path a -> b
foldlAround f b mid (Path d) = foldl f (mid (foldl f b d.right)) d.left
