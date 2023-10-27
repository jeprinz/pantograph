module Data.Tree.Move where

import Data.Either.Nested (type (\/))
import Data.Tree.Common
import Prelude

import Data.Tuple.Nested
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug as Debug
import Text.Pretty (class Pretty, pretty, (<+>))
import Util (fromJust')

repeatApplyUntil :: forall a. PrettyTreeNode a => (Gyro a -> Maybe (Gyro a)) -> (a -> Orientation -> Boolean) -> Gyro a -> Maybe (Gyro a)
repeatApplyUntil f cond gyro_ = go =<< f gyro_
  where
  go gyro = do
    let node = gyroNode gyro
    let orientation = case gyro of
          RootGyro _ -> Outside
          CursorGyro (Cursor {orientation}) -> orientation
          SelectGyro (Select {orientation}) -> orientation
    if cond node orientation then Just gyro else
      go =<< f gyro

tracePrettyInputOutput :: forall b a. Pretty a => Pretty b => String -> (a -> b) -> a -> b
tracePrettyInputOutput str f a = let fa = f a in Debug.trace (str <+> pretty a <+> "=" <+> pretty (f a)) \_ -> fa

-- Gyro

escapeGyro :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
escapeGyro (RootGyro _) = Nothing
escapeGyro (CursorGyro (Cursor {outside, inside})) = Just $ RootGyro (unPath outside inside)
escapeGyro (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

moveGyroLeft :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
moveGyroLeft (CursorGyro cursor) = CursorGyro <$> moveCursorLeft cursor
moveGyroLeft gyro = ensureGyroIsCursor gyro

moveGyroLeftUntil :: forall a. PrettyTreeNode a => (a -> Orientation -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroLeftUntil = repeatApplyUntil moveGyroLeft

moveGyroRight :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
moveGyroRight (CursorGyro cursor) = CursorGyro <$> moveCursorRight cursor
moveGyroRight gyro = ensureGyroIsCursor gyro

moveGyroRightUntil :: forall a. PrettyTreeNode a => (a -> Orientation -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroRightUntil = repeatApplyUntil moveGyroRight

grabGyroDown :: forall a. PrettyTreeNode a => Int -> Gyro a -> Maybe (Gyro a)
grabGyroDown i (SelectGyro select) = grabSelectDown i select
grabGyroDown i gyro = ensureGyroIsSelectThenGrabDown i gyro

grabGyroLeft = tracePrettyInputOutput "grabGyroLeft" grabGyroLeft'
  where
  grabGyroLeft' :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
  grabGyroLeft' gyro = ensureGyroIsSelectThenGrabLeft gyro

grabGyroLeftUntil :: forall a. PrettyTreeNode a => (a -> Orientation -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroLeftUntil = repeatApplyUntil grabGyroLeft

grabGyroRight = tracePrettyInputOutput "grabGyroRight" grabGyroRight'
  where
  grabGyroRight' :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
  grabGyroRight' gyro = ensureGyroIsSelectThenGrabRight gyro

grabGyroRightUntil :: forall a. PrettyTreeNode a => (a -> Orientation -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroRightUntil = repeatApplyUntil grabGyroRight

-- If `ensureGyroIsCursor gyro == Nothing` then then `gyro` is already a
-- `Cursor`.
ensureGyroIsCursor :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
ensureGyroIsCursor (CursorGyro _) = Nothing
ensureGyroIsCursor (RootGyro expr) = Just $ CursorGyro (Cursor {outside: mempty, inside: expr, orientation: Outside})
ensureGyroIsCursor (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

ensureGyroIsSelectThenGrabLeft :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
ensureGyroIsSelectThenGrabLeft (SelectGyro select) = grabSelectLeft select
ensureGyroIsSelectThenGrabLeft (RootGyro _expr) = Nothing
ensureGyroIsSelectThenGrabLeft (CursorGyro (Cursor {outside, inside})) = do
  {outer: outside', inner: tooth} <- unconsPath outside
  Just $ SelectGyro $ Select {outside: outside', middle: singletonNonEmptyPath tooth, inside, orientation: Outside}

ensureGyroIsSelectThenGrabRight :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
ensureGyroIsSelectThenGrabRight (SelectGyro select) = grabSelectRight select
ensureGyroIsSelectThenGrabRight (RootGyro expr) = do
  Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside: expr, orientation: Inside}
  Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: Inside}
ensureGyroIsSelectThenGrabRight (CursorGyro (Cursor {outside, inside})) = do
  Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside, orientation: Inside}
  Just $ SelectGyro $ Select {outside, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: Inside}

ensureGyroIsSelectThenGrabDown :: forall a. PrettyTreeNode a => Int -> Gyro a -> Maybe (Gyro a)
ensureGyroIsSelectThenGrabDown i (SelectGyro select) = grabSelectDown i select
ensureGyroIsSelectThenGrabDown i (RootGyro expr) = do
  let Cursor {outside: middle, inside: inside'} = moveCursorDown i $ Cursor {outside: mempty, inside: expr, orientation: Inside}
  Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: Inside}
ensureGyroIsSelectThenGrabDown i (CursorGyro (Cursor {outside, inside})) = do
  let Cursor {outside: middle, inside: inside'} = moveCursorDown i $ Cursor {outside: mempty, inside, orientation: Inside}
  Just $ SelectGyro $ Select {outside, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: Inside}

-- Cursor

escapeCursor :: forall a. Cursor a -> Tree a
escapeCursor (Cursor {outside, inside}) = unPath outside inside

-- moveCursorUp

moveCursorUp :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a \/ {upCursor :: Cursor a, i :: Int, kidsCount :: Int})
moveCursorUp (Cursor {outside, inside, orientation}) = case orientation of
  Outside -> case outside of
    Path Nil -> Nothing
    Path (Cons tooth@(Tooth a (i /\ kids)) tooths) -> Just $ Right $ {upCursor: Cursor {outside: Path tooths, inside: unTooth tooth inside, orientation: Inside}, i, kidsCount: Array.length kids + 1}
  Inside -> Just $ Left $ Cursor {outside, inside, orientation: Outside}

-- moveCursorLeft

moveCursorLeft :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a)
moveCursorLeft cursor =
  moveCursorUp cursor >>= case _ of
    Left upCursor -> Just upCursor
    Right {upCursor, i} ->
      pure if i == 0
        then upCursor
        else moveCursorDownRightMost $ moveCursorDown (i - 1) upCursor

moveCursorDown :: forall a. PrettyTreeNode a => Int -> Cursor a -> Cursor a
moveCursorDown i (Cursor {outside, inside, orientation}) = case orientation of
  Outside ->
    Cursor {outside, inside, orientation: Inside}
  Inside -> do
    let tooth /\ inside' = fromJust' "moveCursorDown" $ Array.index (tooths inside) i
    Cursor {outside: consPath outside tooth, inside: inside', orientation: Outside}

-- | Ignores `Inside` orientation `Cursor`s.
moveCursorDownOuter :: forall a. PrettyTreeNode a => Int -> Cursor a -> Cursor a
moveCursorDownOuter i (Cursor {outside, inside}) = do
  let tooth /\ inside' = fromJust' "moveCursorDown" $ Array.index (tooths inside) i
  Cursor {outside: consPath outside tooth, inside: inside', orientation: Outside}

moveCursorDownRightMost :: forall a. PrettyTreeNode a => Cursor a -> Cursor a
moveCursorDownRightMost cursor@(Cursor {outside, inside, orientation}) =
  case orientation of
    Outside ->
      Cursor {outside, inside, orientation: Inside}
    Inside -> 
      case Array.last (tooths inside) of
        Nothing -> cursor
        Just (tooth /\ inside') -> moveCursorDownRightMost (Cursor {outside: consPath outside tooth, inside: inside', orientation: Outside})

-- moveCursorRight

moveCursorRight :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a)
moveCursorRight cursor@(Cursor {outside, inside: inside@(Tree _ kids), orientation}) = case orientation of
  Outside ->
    Just $ Cursor {outside, inside, orientation: Inside}
  Inside ->
    if Array.length kids == 0
      then
        moveCursorUp cursor >>= case _ of
          Left upCursor -> moveCursorUpRightNext upCursor
          Right {upCursor, i, kidsCount} ->
            if i + 1 == kidsCount
              then moveCursorUpRightNext upCursor
              else pure $ moveCursorDown (i + 1) upCursor
      else pure $ moveCursorDown 0 cursor

moveCursorUpRightNext :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a)
moveCursorUpRightNext cursor =
  moveCursorUp cursor >>= case _ of
    Left upCursor -> moveCursorUpRightNext upCursor
    Right {upCursor, i, kidsCount} ->
      if i + 1 == kidsCount
        then moveCursorUpRightNext upCursor
        else pure $ moveCursorDown (i + 1) upCursor

-- moveCursorDownLeftMost :: forall a. PrettyTreeNode a => Cursor a -> Cursor a
-- moveCursorDownLeftMost cursor@(Cursor {outside, inside}) =
--   case Array.index (tooths inside) 0 of
--     Nothing -> cursor
--     Just {kid, tooth} -> moveCursorDownLeftMost (Cursor {outside: consPath outside tooth, inside: kid})

-- moveCursorLeftUntil

moveCursorLeftUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Cursor a -> Maybe (Cursor a)
moveCursorLeftUntil cond cursor = do
  cursorLeft <- moveCursorLeft cursor
  pure $ fromMaybe cursorLeft $ go cursorLeft
  where
  go :: Cursor a -> Maybe (Cursor a)
  go cursor' = do
    cursorLeft@(Cursor {inside: Tree a _}) <- moveCursorLeft cursor'
    if cond a
      then pure cursorLeft
      else go cursorLeft

-- moveCursorRightUntil

moveCursorRightUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Cursor a -> Maybe (Cursor a)
moveCursorRightUntil cond cursor = do
  cursorRight <- moveCursorRight cursor
  pure $ fromMaybe cursorRight $ go cursorRight
  where
  go :: Cursor a -> Maybe (Cursor a)
  go cursor' = do
    cursorRight@(Cursor {inside: Tree a _}) <- moveCursorRight cursor'
    if cond a
      then pure cursorRight
      else go cursorRight

-- Select

escapeSelect :: forall a. PrettyTreeNode a => Select a -> Cursor a
escapeSelect = tracePrettyInputOutput "escapeSelect" escapeSelect'
  where
  escapeSelect' :: forall a. PrettyTreeNode a => Select a -> Cursor a
  escapeSelect' (Select {outside, middle, inside, orientation}) =
    case orientation of
      Outside -> Cursor {outside, inside: unPath (toPath middle) inside, orientation: Outside}
      Inside -> Cursor {outside: outside <> toPath middle, inside, orientation: Inside}

grabSelectUp :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grabSelectUp (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: snocNonEmptyPath tooth middle, inside, orientation}
    Inside -> do
      case unconsNonEmptyPath middle of
        {outer: Nothing, inner: tooth} -> Just $ CursorGyro $ Cursor {outside, inside: unTooth tooth inside, orientation: Inside}
        {outer: Just middle', inner: tooth} -> Just $ SelectGyro $ Select {outside, middle: middle', inside: unTooth tooth inside, orientation}

grabSelectLeft :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grabSelectLeft (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: snocNonEmptyPath tooth middle, inside, orientation}
    Inside -> do
      Cursor {outside: middle', inside: inside'} <- moveCursorLeft (Cursor {outside: toPath middle, inside, orientation: Outside})
      Just $ SelectGyro $ Select {outside, middle: fromPath "grabSelectLeft" middle', inside: inside', orientation}

grabSelectRight :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grabSelectRight (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> case unsnocNonEmptyPath middle of
      {outer: tooth, inner: Nothing} -> Debug.trace "here1" \_ -> Just $ CursorGyro $ Cursor {outside: consPath outside tooth, inside, orientation: Outside}
      {outer: tooth, inner: Just middle'} -> Debug.trace "here2" \_ -> Just $ SelectGyro $ Select {outside: consPath outside tooth, middle: middle', inside, orientation}
    Inside -> do
      Cursor {outside: middle', inside: inside'} <- moveCursorRight (Cursor {outside: toPath middle, inside, orientation: Inside})
      Just $ SelectGyro $ Select {outside, middle: fromPath "grabSelectRight" middle', inside: inside', orientation}

grabSelectDown :: forall a. PrettyTreeNode a => Int -> Select a -> Maybe (Gyro a)
grabSelectDown i (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> case unsnocNonEmptyPath middle of
      {outer: tooth@(Tooth _ (i' /\ _)), inner: Nothing} | i == i' -> Just $ CursorGyro $ Cursor {outside: consPath outside tooth, inside, orientation: Outside}
      {outer: tooth@(Tooth _ (i' /\ _)), inner: Just middle'} | i == i' -> Just $ SelectGyro $ Select {outside: consPath outside tooth, middle: middle', inside, orientation}
      _ -> Nothing
    Inside -> do
      let Cursor {outside: middle', inside: inside'} = moveCursorDownOuter i (Cursor {outside: toPath middle, inside, orientation: Inside})
      Just $ SelectGyro $ Select {outside, middle: fromPath "grabSelectRight" middle', inside: inside', orientation}
