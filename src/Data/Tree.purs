module Data.Tree where

import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as Pretty
import Util (fromJust', insertAt)

data Tree a = Tree {node :: a, kids :: Array (Tree a)}
derive instance Generic (Tree a) _
instance Show a => Show (Tree a) where show x = genericShow x
derive instance Eq a => Eq (Tree a)
derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

newtype Tooth a = Tooth {node :: a, i :: Int, kids :: Array (Tree a)}
derive instance Generic (Tooth a) _
instance Show a => Show (Tooth a) where show x = genericShow x
derive instance Eq a => Eq (Tooth a)
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

tooths :: forall a. Tree a -> Array {tooth :: Tooth a, kid :: Tree a}
tooths (Tree {node, kids}) = kids # Array.mapWithIndex \i kid -> {tooth: Tooth {node, i, kids: fromJust' "tooths" $ Array.deleteAt i kids}, kid}

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth {node, i, kids}) kid = Tree {node, kids: fromJust' "unTooth" $ Array.insertAt i kid kids}

newtype Path a = Path (List (Tooth a))
derive instance Generic (Path a) _
instance Show a => Show (Path a) where show x = genericShow x
derive instance Eq a => Eq (Path a)
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path
derive newtype instance Semigroup (Path a)
derive newtype instance Monoid (Path a)

consPath :: forall a. Tooth a -> Path a -> Path a
consPath a (Path as) = Path (Cons a as)

unPath :: forall a. Path a -> Tree a -> Tree a
unPath (Path Nil) tree = tree
unPath (Path (Cons t ts)) tree = unPath (Path ts) (unTooth t tree)

newtype Cursor a = Cursor {outside :: Path a, inside :: Tree a}
derive instance Generic (Cursor a) _
instance Show a => Show (Cursor a) where show x = genericShow x
derive instance Eq a => Eq (Cursor a)
derive instance Functor Cursor

newtype Select a = Select {outside :: Path a, middle :: Path a, inside :: Tree a}
derive instance Generic (Select a) _
instance Show a => Show (Select a) where show x = genericShow x
derive instance Eq a => Eq (Select a)
derive instance Functor Select

data Gyro a = RootGyro (Tree a) | CursorGyro (Cursor a) | SelectGyro (Select a)
derive instance Generic (Gyro a) _
instance Show a => Show (Gyro a) where show x = genericShow x
derive instance Eq a => Eq (Gyro a)
derive instance Functor Gyro

data Change a
  = Shift ShiftSign (Tooth a) (Change a)
  | Replace (Tree a) (Tree a)
  | Reflect a (Array (Change a))
derive instance Generic (Change a) _
instance Show a => Show (Change a) where show x = genericShow x
derive instance Eq a => Eq (Change a)
derive instance Functor Change

data ShiftSign = Plus | Minus
derive instance Generic ShiftSign _
instance Show ShiftSign where show = genericShow
derive instance Eq ShiftSign
derive instance Ord ShiftSign

instance Pretty ShiftSign where
  pretty Plus = "+"
  pretty Minus = "-"

class TreeNode a where
  kidsCount :: a -> Int

assertValidTreeKids :: forall a b c. TreeNode a => String -> a -> (Partial => Array b -> c) -> Array b -> c
assertValidTreeKids msg a k bs = unsafePartial
  if kidsCount a == Array.length bs
    then k bs
    else bug $ "invalid tree kids: " <> msg

class TreeNode a <= PrettyTreeNode a where
  prettyTreeNode :: a -> Array String -> String

instance PrettyTreeNode a => Pretty (Tree a) where
  pretty (Tree {node, kids}) = prettyTreeNode node (pretty <$> kids)

prettyTooth :: forall a. PrettyTreeNode a => Tooth a -> String -> String
prettyTooth (Tooth {node, kids, i}) str = prettyTreeNode node (insertAt "prettyTooth" i str (pretty <$> kids))

instance PrettyTreeNode a => Pretty (Tooth a) where
  pretty tooth = prettyTooth tooth Pretty.cursor

prettyPath :: forall a. PrettyTreeNode a => Path a -> String -> String
prettyPath (Path ts) = go ts
  where
  go Nil str = str
  go (Cons t ts') str = go ts' (prettyTooth t str)

instance PrettyTreeNode a => Pretty (Path a) where
  pretty path = prettyPath path Pretty.cursor

instance PrettyTreeNode a => Pretty (Cursor a) where
  pretty (Cursor {outside, inside}) = prettyPath outside $ Pretty.braces2 $ pretty inside

instance PrettyTreeNode a => Pretty (Select a) where
  pretty (Select {outside, middle, inside}) = prettyPath outside $ Pretty.braces2 $ prettyPath middle $ Pretty.braces2 $ pretty inside

instance PrettyTreeNode a => Pretty (Gyro a) where
  pretty (RootGyro tree) = pretty tree
  pretty (CursorGyro cursor) = pretty cursor
  pretty (SelectGyro select) = pretty select

instance PrettyTreeNode a => Pretty (Change a) where
  pretty (Shift sign tooth change) = pretty sign <> (Pretty.outer (prettyTooth tooth (Pretty.inner (pretty change))))
  pretty (Replace tree1 tree2) = parens (pretty tree1 <+> "~~>" <+> pretty tree2)
  pretty (Reflect node kids) = prettyTreeNode node (pretty <$> kids)
