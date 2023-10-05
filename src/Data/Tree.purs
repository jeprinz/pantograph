module Data.Tree where

import Prelude

import Data.List (List)

newtype Tree a = Tree {node :: a, kids :: Array (Tree a)}

derive instance Functor Tree

newtype Tooth a = Tooth {node :: a, i :: Int, zippedKids :: Array (Tree a)}

derive instance Functor Tooth

newtype Path a = Path (List (Tooth a))

newtype Cursor a = Cursor {path :: Path a, tree :: Tree a}

newtype Select a = Select {top :: Path a, mid :: Path a, bot :: Tree a}

data Change a
  = Shift ShiftSign a (Change a)
  | Replace (Tree a) (Tree a)
  | Reflect a (Array (Change a))

data ShiftSign = Plus | Minus

derive instance Functor Change