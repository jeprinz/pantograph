module Data.Tree.Change where

import Data.Tree.Common
import Prelude

import Data.Tuple.Nested ((/\))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

reflect :: forall a. Tree a -> Change a
reflect (Tree {node, kids}) = InjectChange {node, kids: reflect <$> kids}

invert :: forall a. Change a -> Change a
invert (Shift {sign, tooth, kid}) = Shift {sign: invertShiftSign sign, tooth, kid}
invert (Replace {old, new}) = Replace {old: new, new: old}
invert (InjectChange {node, kids}) = InjectChange {node, kids: invert <$> kids}

invertShiftSign :: ShiftSign -> ShiftSign
invertShiftSign Plus = Minus
invertShiftSign Minus = Plus

toExpr :: forall a. Eq a => Change a -> Maybe (Tree a)
toExpr (InjectChange {node, kids}) = Tree <<< {node, kids: _} <$> toExpr `traverse` kids
toExpr _ = Nothing

endpoints :: forall a. Change a -> {left :: Tree a, right :: Tree a}
endpoints (Shift {sign: Plus, tooth, kid}) =
  let {left, right} = endpoints kid in
  {left, right: unTooth tooth right}
endpoints (Shift {sign: Minus, tooth, kid}) =
  let {left, right} = endpoints kid in
  {left: unTooth tooth left, right}
endpoints (Replace {old, new}) = {left: old, right: new}
endpoints (InjectChange {node, kids}) = 
  let kids' = endpoints <$> kids in
  let leftKids /\ rightKids = Array.unzip $ map (\{left, right} -> left /\ right) kids' in
  {left: Tree {node, kids: leftKids}, right: Tree {node, kids: rightKids}}


