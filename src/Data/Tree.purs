module Data.Tree where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Traversable (class Traversable)

newtype Tree a = Tree {node :: a, kids :: Array (Tree a)}
derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

newtype Tooth a = Tooth {node :: a, i :: Int, zippedKids :: Array (Tree a)}
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

newtype Path a = Path (List (Tooth a))
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path

newtype Cursor a = Cursor {outside :: Path a, inside :: Tree a}
derive instance Functor Cursor

newtype Select a = Select {outside :: Path a, middle :: Path a, inside :: Tree a}
derive instance Functor Select

data Gyro a = RootGyro (Tree a) | CursorGyro (Cursor a) | SelectGyro (Select a)
derive instance Functor Gyro

data Change a
  = Shift ShiftSign a (Change a)
  | Replace (Tree a) (Tree a)
  | Reflect a (Array (Change a))
derive instance Functor Change

data ShiftSign = Plus | Minus
derive instance Eq ShiftSign