module Data.Tree.Swivel where

import Data.Tree
import Prelude
import Util

import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

-- Move

moveGyroNext :: forall a. F Gyro a
moveGyroNext = map CursorGyro <<< swivelNext <<< fromSelectGyroToCursor

moveGyroPrev :: forall a. F Gyro a
moveGyroPrev = map CursorGyro <<< swivelPrev <<< fromSelectGyroToCursor

-- Grab

grabGyroNext :: forall a. F Gyro a
grabGyroNext = fromGyroToPreselect Inside >>> swivelNext >>> map fromPreselectToGyro

grabGyroPrev :: forall a. F Gyro a
grabGyroPrev = fromGyroToPreselect Inside >>> swivelPrev >>> map fromPreselectToGyro

-- | # Useful Types

data PathTree a = PathTree (Path a) (Tree a)
data PathPath a = PathPath (Path a) (Path a)

type F f (a :: Type) = f a -> Maybe (f a)

-- | # Swivel

-- | Swivel laws:
-- | ```
-- | swivelLeft <=> swivelRight
-- | swivelUpUntilRight <=> (swivelLeft >=> swivelLowestRightmost)
-- | 
-- | where
-- |   k1 <=> k2  =  (k1 >=> k2) == id  &&  (k1 <=< k2) == id
-- | ```
class Swivel (f :: Type -> Type) where
  getLeftIndex :: forall a. f a -> Maybe Int
  getRightIndex :: forall a. f a -> Maybe Int
  getDownLeftmostIndex :: forall a. f a -> Maybe Int
  getDownRightmostIndex :: forall a. f a -> Maybe Int

  swivelUp :: forall a. F f a
  swivelDownAt :: forall a. Int -> F f a

swivelUpUntilRight :: forall f a. Swivel f => F f a
swivelUpUntilRight a =
  case swivelRight a of
    Just a' -> pure a'
    Nothing -> swivelUp a >>= swivelUpUntilRight

swivelLowestRightmost :: forall f a. Swivel f => F f a
swivelLowestRightmost a =
  case swivelDownRightmost a of
    Nothing -> pure a
    Just a' -> swivelLowestRightmost a'

swivelDownLeftmost :: forall f a. Swivel f => F f a
swivelDownLeftmost a = getDownLeftmostIndex a >>= \i -> swivelDownAt i a

swivelDownRightmost :: forall f a. Swivel f => F f a
swivelDownRightmost a = getDownRightmostIndex a >>= \i -> swivelDownAt i a

swivelLeft :: forall f a. Swivel f => F f a
swivelLeft a = getLeftIndex a >>= \i -> swivelUp >=> swivelDownAt i $ a

swivelRight :: forall f a. Swivel f => F f a
swivelRight a = getRightIndex a >>= \i -> swivelUp >=> swivelDownAt i $ a

swivelPrev :: forall f a. Swivel f => F f a
swivelPrev a = case swivelLeft a of
  Just a' -> swivelLowestRightmost a'
  Nothing -> swivelUp a

swivelNext :: forall f a. Swivel f => F f a
swivelNext a = case swivelDownLeftmost a of
  Just a' -> pure a'
  Nothing -> swivelUpUntilRight a

until :: forall f a. F f a -> (f a -> Boolean) -> F f a
until f cond = go
  where
  go a = f a >>= \a' -> if cond a' then pure a' else go a'

-- Swivel PathTree

instance Swivel PathTree where
  getLeftIndex (PathTree outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ - 1)
  getRightIndex (PathTree outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ + 1)
  getDownLeftmostIndex (PathTree _ (Tree _ kids)) | l <- Array.length kids = guard (l > 0) >>= \_ -> Just 0
  getDownRightmostIndex (PathTree _ (Tree _ kids)) | l <- Array.length kids = guard (l > 0) >>= \_ -> Just (l - 1)

  swivelUp (PathTree outside inside) = do
    -- debugM "Swivel PathTree/swivelUp" {}
    {outer: outside', inner} <- unconsPath outside
    pure $ PathTree outside' (inner `unTooth` inside)

  swivelDownAt i (PathTree outside (Tree a kids)) = do
    -- debugM ("Swivel PathTree/swivelDownAt " <> show i) {}
    kids' /\ kid <- extractAt i kids
    pure $ PathTree (outside `consPath` Tooth a (i /\ kids')) kid

-- Swivel PathPath

instance Swivel PathPath where
  getLeftIndex (PathPath outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ - 1)
  getRightIndex (PathPath outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ + 1)
  getDownLeftmostIndex (PathPath _ middle) = unsnocPath middle <#> _.outer >>> toothIndex
  getDownRightmostIndex (PathPath _ middle) = unsnocPath middle <#> _.outer >>> toothIndex

  swivelUp (PathPath outside middle) = do
    {outer, inner} <- unconsPath outside
    pure $ PathPath outer (inner `snocPath` middle)

  swivelDownAt i (PathPath outside middle) = do
    {outer, inner} <- unsnocPath middle
    guard (toothIndex outer == i)
    pure $ PathPath (outside `consPath` outer) inner

-- Swivel Cursor

instance Swivel Cursor where
  getLeftIndex = fromCursorToPathTree >>> getLeftIndex
  getRightIndex = fromCursorToPathTree >>> getRightIndex
  getDownLeftmostIndex = fromCursorToPathTree >>> getDownLeftmostIndex
  getDownRightmostIndex = fromCursorToPathTree >>> getDownRightmostIndex

  swivelUp = (fromCursorToPathTree >>> swivelUp) <##> fromPathTreeToCursor
  swivelDownAt i = (fromCursorToPathTree >>> swivelDownAt i) <##> fromPathTreeToCursor

-- Swivel Preselect

data Preselect a = Preselect (Path a) (Path a) (Tree a) Orientation

fromGyroToPreselect :: Orientation -> Gyro ~> Preselect
fromGyroToPreselect o (CursorGyro (Cursor {outside, inside})) = Preselect outside mempty inside o
fromGyroToPreselect _ (SelectGyro (Select {outside, middle, inside, orientation})) = Preselect outside (toPath middle) inside orientation

fromPreselectToGyro :: Preselect ~> Gyro
fromPreselectToGyro (Preselect outside middle_ inside orientation) = case fromPathMaybe middle_ of
  Nothing -> CursorGyro (Cursor {outside, inside, orientation: Outside})
  Just middle -> SelectGyro (Select {outside, middle, inside, orientation})

fromPreselectToEitherPathPathOrPathTree :: forall a. Preselect a -> Either (PathPath a) (PathTree a)
fromPreselectToEitherPathPathOrPathTree = case _ of
  Preselect outside middle _ Outside -> Left (PathPath outside middle)
  Preselect _ middle inside Inside -> Right (PathTree middle inside)

instance Swivel Preselect where
  getLeftIndex = fromPreselectToEitherPathPathOrPathTree >>> either getLeftIndex getLeftIndex
  getRightIndex = fromPreselectToEitherPathPathOrPathTree >>> either getRightIndex getRightIndex
  getDownLeftmostIndex = fromPreselectToEitherPathPathOrPathTree >>> either getDownLeftmostIndex getDownLeftmostIndex
  getDownRightmostIndex = fromPreselectToEitherPathPathOrPathTree >>> either getDownRightmostIndex getDownRightmostIndex

  swivelUp = case _ of
    Preselect outside middle inside Outside -> swivelUp (PathPath outside middle) <#> \(PathPath outside' middle') -> Preselect outside' middle' inside Outside
    -- Outside -> Inside
    Preselect outside (Path Nil) inside Inside -> swivelUp (PathPath outside (Path Nil)) <#> \(PathPath outside' middle') -> Preselect outside' middle' inside Outside
    Preselect outside middle inside Inside -> swivelUp (PathTree middle inside) <#> \(PathTree middle' inside') -> Preselect outside middle' inside' Inside

  swivelDownAt i = case _ of
    -- Inside -> Outside
    Preselect outside (Path Nil) inside Outside -> swivelDownAt i (PathTree (Path Nil) inside) <#> \(PathTree middle' inside') -> Preselect outside middle' inside' Inside
    Preselect outside middle inside Outside -> swivelDownAt i (PathPath outside middle) <#> \(PathPath outside' middle') -> Preselect outside' middle' inside Outside
    Preselect outside middle inside Inside -> swivelDownAt i (PathTree middle inside) <#> \(PathTree middle' inside') -> Preselect outside middle' inside' Inside

-- | # Utilities

fromTreeToPathTree :: Tree ~> PathTree
fromTreeToPathTree tree = PathTree mempty tree

fromCursorToPathTree :: forall a. Cursor a -> PathTree a
fromCursorToPathTree (Cursor {outside, inside}) = PathTree outside inside

fromPathTreeToCursor :: forall a. PathTree a -> Cursor a
fromPathTreeToCursor (PathTree outside inside) = Cursor {outside, inside, orientation: Outside}

