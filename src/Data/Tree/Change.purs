module Data.Tree.Change where

import Data.Tree.Common
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Text.Pretty (pretty, ticks)
import Util (fromJust)

invert :: forall a. Change a -> Change a
invert (Shift sign tooth kid) = Shift (invertShiftSign sign) tooth kid
invert (Replace old new) = Replace new old
invert (Change a kids) = Change a (invert <$> kids)

invertShiftSign :: ShiftSign -> ShiftSign
invertShiftSign Plus = Minus
invertShiftSign Minus = Plus

toExpr :: forall a. Eq a => Change a -> Maybe (Tree a)
toExpr (Change a kids) = Tree a <$> toExpr `traverse` kids
toExpr _ = Nothing

-- NOTE: this is NOT the same as asking if the change has equal endpoints (a loop in the groupoid), it computes if its an identity under composition
isIdentity :: forall a. Eq a => Change a -> Boolean
isIdentity (Change _ cs) = Array.all isIdentity cs
isIdentity (Replace e1 e2) = e1 == e2 -- NOTE: I'm not sure if this should be considered an identity, but if not then something needs to be done about (doOperation (Replace a b) ?x)
isIdentity _ = false

-- least upper bound
-- actually, I'm not sure we need this.
-- LUB (+ X -> A) (+ Y -> A) -- no unique solution!
-- if you have changes where Plus and Minus DONT cancel each other out, then changes form a category without inverses.
-- this function returns the unique limit where it exists in that category, and returns Nothing if there is no unique solution.
-- TODO: There might be a simpler way to define this function, which is that it's output either only has + or only has -    ???
lub :: forall a. Eq a => Change a -> Change a -> Maybe (Change a)
lub c1 c2 = case c1 /\ c2 of
  (Change a cs) /\ (Change a' cs') | a == a' -> Change a <$> Array.zipWithA lub cs cs'
  _ | Just out <- plusLub c1 c2 -> pure out
  _ | Just out <- plusLub c2 c1 -> pure out
  _ | Just out <- minusLub c1 c2 -> pure out
  _ | Just out <- minusLub c2 c1 -> pure out
  _ -> Nothing

plusLub :: forall a. Eq a => Change a -> Change a -> Maybe (Change a)
plusLub c1 c2 | c1 == c2 = Just c1
plusLub (Shift Plus th c1) c2 = Shift Plus th <$> plusLub c1 c2
plusLub c1@(Replace _ _) c2 | isIdentity c2 = Just c1
plusLub _ _ = Nothing

minusLub :: forall a. Eq a => Change a -> Change a -> Maybe (Change a)
minusLub c1 c2 | c1 == c2 = Just c1
minusLub (Shift Minus th@(Tooth a i p) c1) (Change a' cs) | a == a' = Shift Minus th <$> minusLub c1 (fromJust $ Array.index cs i)
minusLub c1@(Replace _ _) c2 | isIdentity c2 = Just c1
minusLub _ _ = Nothing

lub' :: forall a. Eq a => PrettyTreeNode a => Change a -> Change a -> Change a
lub' c1 c2 = case lub c1 c2 of
  Nothing -> bug $ "lub doesn't exist for changes: c1 = " <> ticks (pretty c1) <> "; c2 = " <> ticks (pretty c2)
  Just c -> c
