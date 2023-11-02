module Data.Tree.Swivel where

import Data.Tree
import Prelude
import Util

import Control.Alternative (guard)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Todo (todo)

data PathTree a = PathTree (Path a) (Tree a)
data PathPath a = PathPath (Path a) (Path a)

type F f (a :: Type) = f a -> Maybe (f a)

-- Swivel

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

swivelNextSuchThat :: forall f a. Swivel f => (f a -> Boolean) -> F f a
swivelNextSuchThat cond = repeatApply1UntilNothingOrFail cond swivelNext

swivelPrevSuchThat :: forall f a. Swivel f => (f a -> Boolean) -> F f a
swivelPrevSuchThat cond = repeatApply1UntilNothingOrFail cond swivelPrev

repeatApply1UntilNothingOrFail :: forall f a. (f a -> Boolean) -> F f a -> F f a
repeatApply1UntilNothingOrFail cond f = go
  where
  go a = do
    a' <- f a
    if cond a' then pure a' else go a'

-- Swivel PathTree

instance Swivel PathTree where
  getLeftIndex (PathTree outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ - 1)
  getRightIndex (PathTree outside _) = unconsPath outside <#> _.inner >>> toothIndex >>> (_ + 1)
  getDownLeftmostIndex (PathTree _ (Tree _ kids)) | l <- Array.length kids = guard (l > 0) >>= \_ -> Just 0
  getDownRightmostIndex (PathTree _ (Tree _ kids)) | l <- Array.length kids = guard (l > 0) >>= \_ -> Just (l - 1)

  swivelUp (PathTree outside inside) = do
    debugM "Swivel PathTree/swivelUp" {}
    {outer: outside', inner} <- unconsPath outside
    pure $ PathTree outside' (inner `unTooth` inside)

  swivelDownAt i (PathTree outside (Tree a kids)) = do
    debugM ("Swivel PathTree/swivelDownAt " <> show i) {}
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

-- Swivel (EitherF PathPath PathTree)

instance Swivel (EitherF PathPath PathTree) where
  getLeftIndex = eitherF getLeftIndex getLeftIndex
  getRightIndex = eitherF getRightIndex getRightIndex
  getDownLeftmostIndex = eitherF getDownLeftmostIndex getDownLeftmostIndex
  getDownRightmostIndex = eitherF getDownRightmostIndex getDownRightmostIndex

  swivelUp = overM swivelUp swivelUp
  swivelDownAt i = overM (swivelDownAt i) (swivelDownAt i)

-- Swivel SelectOrCursor

type SelectOrCursor = EitherF Select Cursor

instance Swivel SelectOrCursor where
  getLeftIndex = fromSelectOrCursor >>> getLeftIndex
  getRightIndex = fromSelectOrCursor >>> getRightIndex
  getDownLeftmostIndex = fromSelectOrCursor >>> getDownLeftmostIndex
  getDownRightmostIndex = fromSelectOrCursor >>> getDownRightmostIndex

  swivelUp esc = (fromSelectOrCursor >>> swivelUp) esc <#> toSelectOrCursor esc
  swivelDownAt i esc = (fromSelectOrCursor >>> swivelDownAt i) esc <#> toSelectOrCursor esc

-- Swivel SelectOrCursorOrTree

type SelectOrCursorOrTree = EitherF (EitherF Select Cursor) Tree

instance Swivel SelectOrCursorOrTree where
  getLeftIndex = eitherF getLeftIndex (const Nothing)
  getRightIndex = eitherF getRightIndex (const Nothing)
  getDownLeftmostIndex = eitherF getDownLeftmostIndex (const (Just 0))
  getDownRightmostIndex = eitherF getDownRightmostIndex (const (Just 0))

  swivelUp = eitherF (swivelUp >>> map LeftF) (const Nothing)
  swivelDownAt i = eitherF (swivelDownAt i >>> map LeftF) (\tree -> pure (LeftF (RightF (fromTreeToCursor tree))))

fromGyroToSelectOrCursorOrTree :: Gyro ~> SelectOrCursorOrTree
fromGyroToSelectOrCursorOrTree (RootGyro tree) = RightF tree
fromGyroToSelectOrCursorOrTree (CursorGyro cursor) = LeftF (RightF cursor)
fromGyroToSelectOrCursorOrTree (SelectGyro select) = LeftF (LeftF select)

fromSelectOrCursorOrTreeToGyro :: SelectOrCursorOrTree ~> Gyro
fromSelectOrCursorOrTreeToGyro (RightF tree) = RootGyro tree
fromSelectOrCursorOrTreeToGyro (LeftF (RightF cursor)) = CursorGyro cursor
fromSelectOrCursorOrTreeToGyro (LeftF (LeftF select)) = SelectGyro select

-- Swivel Gyro

instance Swivel Gyro where
  getLeftIndex (RootGyro _) = Just 0
  getLeftIndex (CursorGyro cursor) = getLeftIndex cursor
  getLeftIndex (SelectGyro select) = getLeftIndex (fromSelectToSelectOrCursor select)

  getRightIndex (RootGyro _) = Just 0
  getRightIndex (CursorGyro cursor) = getRightIndex cursor
  getRightIndex (SelectGyro select) = getRightIndex (fromSelectToSelectOrCursor select)

  getDownLeftmostIndex (RootGyro _) = Just 0
  getDownLeftmostIndex (CursorGyro cursor) = getDownLeftmostIndex cursor
  getDownLeftmostIndex (SelectGyro select) = getDownLeftmostIndex (fromSelectToSelectOrCursor select)

  getDownRightmostIndex (RootGyro _) = Just 0
  getDownRightmostIndex (CursorGyro cursor) = getDownRightmostIndex cursor
  getDownRightmostIndex (SelectGyro select) = getDownRightmostIndex (fromSelectToSelectOrCursor select)

  swivelUp (RootGyro _) = Nothing
  swivelUp (CursorGyro cursor) = CursorGyro <$> swivelUp cursor
  swivelUp (SelectGyro select) = fromSelectOrCursorToGyro <$> swivelUp (fromSelectToSelectOrCursor select)

  swivelDownAt _ (RootGyro tree) = Just (CursorGyro (fromTreeToCursor tree))
  swivelDownAt i (CursorGyro cursor) = CursorGyro <$> swivelDownAt i cursor
  swivelDownAt i (SelectGyro select) = fromSelectOrCursorToGyro <$> swivelDownAt i (fromSelectToSelectOrCursor select)

-- EitherF

data EitherF f g (a :: Type) = LeftF (f a) | RightF (g a)

eitherF :: forall b f g a. (f a -> b) -> (g a -> b) -> EitherF f g a -> b
eitherF f _ (LeftF a)  = f a
eitherF _ g (RightF b) = g b

over :: forall f g a f' g' a368. (f a -> f' a368) -> (g a -> g' a368) -> EitherF f g a -> EitherF f' g' a368
over f _ (LeftF a) = LeftF (f a)
over _ g (RightF b) = RightF (g b)

overM :: forall f g a m f' g' b. Functor m => (f a -> m (f' b)) -> (g a -> m (g' b)) -> EitherF f g a -> m (EitherF f' g' b)
overM f _ (LeftF a) = LeftF <$> f a
overM _ g (RightF b) = RightF <$> g b

-- utilities

getPathInnerToothIndex :: forall a. Path a -> Maybe Int
getPathInnerToothIndex = unconsPath >=> \{inner: Tooth _ (i /\ _)} -> pure i

getTreeKidsLength :: forall a. Tree a -> Int
getTreeKidsLength (Tree _ kids) = Array.length kids

fromSelectOrCursor :: SelectOrCursor ~> EitherF PathPath PathTree
fromSelectOrCursor (LeftF (Select {outside, middle, orientation: Outside})) = LeftF (PathPath outside (toPath middle))
fromSelectOrCursor (LeftF (Select {middle, inside, orientation: Inside})) = RightF (PathTree (toPath middle) inside)
fromSelectOrCursor (RightF cursor) = RightF (fromCursorToPathTree cursor)

toSelectOrCursor :: forall a. SelectOrCursor a -> EitherF PathPath PathTree a -> SelectOrCursor a
toSelectOrCursor esc (LeftF (PathPath outside middle_)) = case fromPathMaybe middle_ of
  Just middle -> LeftF (Select {outside, middle, inside, orientation: Outside})
  Nothing -> RightF (Cursor {outside, inside, orientation: Outside})
  where
  inside = case esc of
    LeftF (Select {inside: i}) -> i
    RightF (Cursor {inside: i}) -> i
toSelectOrCursor esc (RightF (PathTree middle_ inside)) = case fromPathMaybe middle_ of
  Just middle -> LeftF (Select {outside, middle, inside, orientation: Inside})
  Nothing -> RightF (Cursor {outside, inside, orientation: Outside})
  where
  outside = case esc of
    LeftF (Select {outside: i}) -> i
    RightF (Cursor {outside: i}) -> i

fromTreeToPathTree :: Tree ~> PathTree
fromTreeToPathTree tree = PathTree mempty tree

fromTreeToCursor :: Tree ~> Cursor
fromTreeToCursor tree = Cursor {outside: mempty, inside: tree, orientation: Outside}

fromCursorToPathTree :: forall a. Cursor a -> PathTree a
fromCursorToPathTree (Cursor {outside, inside}) = PathTree outside inside

fromPathTreeToCursor :: forall a. PathTree a -> Cursor a
fromPathTreeToCursor (PathTree outside inside) = Cursor {outside, inside, orientation: Outside}

fromSelectToSelectOrCursor :: Select ~> SelectOrCursor
fromSelectToSelectOrCursor = LeftF

fromSelectOrCursorToGyro :: SelectOrCursor ~> Gyro
fromSelectOrCursorToGyro (LeftF select) = SelectGyro select
fromSelectOrCursorToGyro (RightF cursor) = CursorGyro cursor
