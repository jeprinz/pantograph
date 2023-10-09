module Data.Tree where

import Prelude

import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.Traversable (class Traversable)
import Util (fromJust')

data Tree a = Tree {node :: a, kids :: Array (Tree a)}
derive instance Eq a => Eq (Tree a)
derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

newtype Tooth a = Tooth {node :: a, i :: Int, kids :: Array (Tree a)}
derive instance Eq a => Eq (Tooth a)
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

tooths :: forall a. Tree a -> Array {tooth :: Tooth a, kid :: Tree a}
tooths (Tree {node, kids}) = kids # Array.mapWithIndex \i kid -> {tooth: Tooth {node, i, kids: fromJust' "tooths" $ Array.deleteAt i kids}, kid}

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth {node, i, kids}) kid = Tree {node, kids: fromJust' "unTooth" $ Array.insertAt i kid kids}

newtype Path a = Path (List (Tooth a))
derive instance Eq a => Eq (Path a)
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path

unPath :: forall a. Path a -> Tree a -> Tree a
unPath (Path Nil) tree = tree
unPath (Path (Cons t ts)) tree = unPath (Path ts) (unTooth t tree)

newtype Cursor a = Cursor {outside :: Path a, inside :: Tree a}
derive instance Eq a => Eq (Cursor a)
derive instance Functor Cursor

newtype Select a = Select {outside :: Path a, middle :: Path a, inside :: Tree a}
derive instance Eq a => Eq (Select a)
derive instance Functor Select

data Gyro a = RootGyro (Tree a) | CursorGyro (Cursor a) | SelectGyro (Select a)
derive instance Eq a => Eq (Gyro a)
derive instance Functor Gyro

data Change a
  = Shift ShiftSign a (Change a)
  | Replace (Tree a) (Tree a)
  | Reflect a (Array (Change a))
derive instance Eq a => Eq (Change a)
derive instance Functor Change

data ShiftSign = Plus | Minus
derive instance Eq ShiftSign
derive instance Ord ShiftSign
