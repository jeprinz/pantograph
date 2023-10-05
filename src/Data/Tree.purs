module Data.Tree where

import Prelude

import Data.List (List)

newtype Tree a = Tree {node :: a, kids :: Array (Tree a)}
derive instance Functor Tree

newtype Tooth a = Tooth {node :: a, i :: Int, zippedKids :: Array (Tree a)}
derive instance Functor Tooth

newtype Path a = Path (List (Tooth a))
derive instance Functor Path

newtype Cursor a = Cursor {path :: Path a, tree :: Tree a}
derive instance Functor Cursor

newtype Select a = Select {top :: Path a, mid :: Path a, bot :: Tree a}
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