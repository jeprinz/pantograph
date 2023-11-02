module Data.Tree.Rotate where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Either
import Data.Maybe
import Todo (todo)
import Util (extractAt)

-- Swivel

class Swivel a where
  swivel :: Trajectory -> a -> Maybe a

data Trajectory = Next | Prev

-- Swivel (Cursor a)

instance Swivel (Cursor a) where
  swivel Next = ifMF' _
    { if = swivelCursorDownFirst
    , else = ifMF' _ 
        { if = swivelCursorRight
        , else = swivelCursorUp >=> swivelCursorUpUntilRight } }
  swivel Prev = ifMF' _
    { if = swivelCursorLeft, then = swivelCursorDownRightMost
    , else = swivelCursorUp }

swivelCursorUp (Cursor {outside, inside}) = do
  {outer: outside', inner: th} <- unconsPath outside
  pure $ Cursor {outside: outside', inside: th `unTooth` inside, orientation: Outside}

swivelCursorDownFirst cursor = swivelCursorDownAt 0 cursor

swivelCursorDownLast cursor@(Cursor {inside: Tree _ kids}) = swivelCursorDownAt (Array.length kids - 1) cursor

swivelCursorLeft cursor@(Cursor {outside}) = do
  i <- getPathInnerToothIndex outside
  swivelCursorUp >=> swivelCursorDownAt (i - 1) $ cursor

swivelCursorRight cursor@(Cursor {outside}) = do
  i <- getPathInnerToothIndex outside
  swivelCursorUp >=> swivelCursorDownAt (i + 1) $ cursor

swivelCursorUpUntilRight :: forall a. Cursor a -> Maybe (Cursor a)
swivelCursorUpUntilRight = ifMF' _ { if = swivelCursorRight, else = swivelCursorUp >=> swivelCursorUpUntilRight }

swivelCursorDownRightMost :: forall a. Cursor a -> Maybe (Cursor a)
swivelCursorDownRightMost = ifMF' _ { if = swivelCursorDownLast, then = swivelCursorDownRightMost }

swivelCursorDownAt :: forall a. Int -> Cursor a -> Maybe (Cursor a)
swivelCursorDownAt i (Cursor {outside, inside: Tree a kids}) = do
  kids' /\ kid <- extractAt i kids
  pure $ Cursor {outside: outside `consPath` Tooth a (i /\ kids'), inside: kid, orientation: Outside}

-- Swivel (Cos a)

-- | `Cursor` or `Select`
type Cos a = Cursor a \/ Select a

instance Swivel (Cos a) where
  swivel Next = ifMF' _
    { if = swivelCosDownFirst
    , else = swivelCosUp >=> swivelCosUpUntilRight }
  swivel Prev = ifMF' _
    { if = swivelCosLeft, then = swivelCosDownRightMost
    , else = swivelCosUp }

swivelCosUp :: forall a. Cos a -> Maybe (Cos a)
swivelCosUp (Right (Select {outside, middle, inside, orientation: Outside})) = do
  {outer: outside', inner} <- unconsPath outside
  pure $ Right $ Select {outside: outside', middle: inner `snocNonEmptyPath` middle, inside, orientation: Outside}
swivelCosUp (Right (Select {outside, middle, inside, orientation: Inside})) = do
  case unsnocNonEmptyPath middle of
    {outer, inner: Nothing} -> Just $ Left $ Cursor {outside, inside: outer `unTooth` inside, orientation: Outside}
    {outer, inner: Just inner} -> Just $ Right $ Select {outside, middle: inner, inside: outer `unTooth` inside, orientation: Outside}
swivelCosUp (Left (Cursor {outside, inside})) = do
  {outer, inner} <- unconsPath outside
  pure $ Right $ Select {outside: outer, middle: singletonNonEmptyPath inner, inside, orientation: Outside}

swivelCosLeft :: forall a. Cos a -> Maybe (Cos a)
swivelCosLeft (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosLeft (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosLeft (Left (Cursor {outside, inside})) = todo ""

swivelCosRight :: forall a. Cos a -> Maybe (Cos a)
swivelCosRight (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosRight (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosRight (Left (Cursor {outside, inside})) = todo ""

swivelCosDownFirst :: forall a. Cos a -> Maybe (Cos a)
swivelCosDownFirst (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosDownFirst (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosDownFirst (Left (Cursor {outside, inside})) = todo ""

swivelCosDownLast :: forall a. Cos a -> Maybe (Cos a)
swivelCosDownLast (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosDownLast (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosDownLast (Left (Cursor {outside, inside})) = todo ""

swivelCosUpUntilRight :: forall a. Cos a -> Maybe (Cos a)
swivelCosUpUntilRight (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosUpUntilRight (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosUpUntilRight (Left (Cursor {outside, inside})) = todo ""

swivelCosDownRightMost :: forall a. Cos a -> Maybe (Cos a)
swivelCosDownRightMost (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosDownRightMost (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosDownRightMost (Left (Cursor {outside, inside})) = todo ""

swivelCosDownAt :: forall a. Cos a -> Maybe (Cos a)
swivelCosDownAt (Right (Select {outside, middle, inside, orientation: Outside})) = todo ""
swivelCosDownAt (Right (Select {outside, middle, inside, orientation: Inside})) = todo ""
swivelCosDownAt (Left (Cursor {outside, inside})) = todo ""


-- utilities

ifMF' :: forall a' b' c' a b c.
  ( { if :: a -> Maybe a, then :: b -> Maybe b, else :: c -> Maybe c } ->
    { if :: a' -> Maybe b', then :: b' -> c', else :: a' -> c' } ) ->
  a' -> c'
ifMF' f = ifMF $ f {if: pure, then: pure, else: pure}

ifMF :: forall a b c. {if :: a -> Maybe b, then :: b -> c, else :: a -> c} -> a -> c
ifMF {if: if_, then: then_, else: else_} a = case if_ a of
  Just c -> then_ c
  Nothing -> else_ a

getPathInnerToothIndex :: forall a. Path a -> Maybe Int
getPathInnerToothIndex = unconsPath >=> \{inner: Tooth _ (i /\ _)} -> pure i

getTreeKidsLength :: forall a. Tree a -> Int
getTreeKidsLength (Tree _ kids) = Array.length kids

makeCos {outside, middle, inside, orientation} = case fromPathMaybe middle of
  Nothing -> Left $ Cursor {outside, inside, orientation: Outside}
  Just middle' -> Right $ Select {outside, middle: middle', inside, orientation}
