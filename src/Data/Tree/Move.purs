module Data.Tree.Move where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Debug as Debug
import Hole (hole)
import Util (fromJust')

-- Gyro

escapeGyro :: forall a. Gyro a -> Maybe (Gyro a)
escapeGyro (RootGyro _) = Nothing
escapeGyro (CursorGyro (Cursor {outside, inside})) = Just $ RootGyro (unPath outside inside)
escapeGyro (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

moveGyroLeft :: forall a. Gyro a -> Maybe (Gyro a)
moveGyroLeft (RootGyro tree) = pure $ normalizeGyro $ CursorGyro (Cursor {outside: Path Nil, inside: tree})
moveGyroLeft (CursorGyro cursor) = normalizeGyro <<< CursorGyro <$> moveCursorLeft cursor
moveGyroLeft (SelectGyro select) = hole "TODO: moveGyroLeft (SelectGyro select)"

moveGyroRight :: forall a. Gyro a -> Maybe (Gyro a)
moveGyroRight (RootGyro tree) = pure $ normalizeGyro $ CursorGyro (Cursor {outside: Path Nil, inside: tree})
moveGyroRight (CursorGyro cursor) = normalizeGyro <<< CursorGyro <$> moveCursorRight cursor
moveGyroRight (SelectGyro select) = hole "TODO: moveGyroRight (SelectGyro select)"

normalizeGyro :: forall a. Gyro a -> Gyro a
normalizeGyro gyro@(RootGyro _) = gyro
normalizeGyro (CursorGyro (Cursor {outside, inside})) = CursorGyro (Cursor {outside, inside})
normalizeGyro (SelectGyro (Select {outside, middle, inside, isReversed})) = case middle of
  Path Nil -> normalizeGyro $ CursorGyro (Cursor {outside, inside})
  _ -> (SelectGyro (Select {outside, middle, inside, isReversed}))

-- If `ensureGyroIsCursor gyro == Nothing` then then `gyro` is already a
-- `Cursor`.
ensureGyroIsCursor :: forall a. Gyro a -> Maybe (Gyro a)
ensureGyroIsCursor (RootGyro expr) = Just $ CursorGyro (Cursor {outside: mempty, inside: expr})
ensureGyroIsCursor (CursorGyro _) = Nothing
ensureGyroIsCursor (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

escapeSelect :: forall a. Select a -> Cursor a
escapeSelect (Select {outside, middle, inside, isReversed}) =
  if isReversed
    then Cursor {outside, inside: unPath middle inside}
    else Cursor {outside: outside <> middle, inside}

-- Cursor

-- moveCursorUp

moveCursorUp :: forall a. Cursor a -> Maybe {upCursor :: Cursor a, i :: Int, kidsCount :: Int}
moveCursorUp (Cursor {outside, inside}) = case outside of
  Path Nil -> Nothing
  Path (Cons tooth@(Tooth {i, kids}) tooths) -> Just {upCursor: Cursor {outside: Path tooths, inside: unTooth tooth inside}, i, kidsCount: Array.length kids + 1}

-- moveCursorLeft

moveCursorLeft :: forall a. Cursor a -> Maybe (Cursor a)
moveCursorLeft cursor = do
  {upCursor, i} <- moveCursorUp cursor
  pure if i == 0
    then upCursor
    else moveCursorDownRightMost $ moveCursorDown (i - 1) upCursor

moveCursorDown :: forall a. Int -> Cursor a -> Cursor a
moveCursorDown i (Cursor {outside, inside}) = do
  let {kid, tooth} = fromJust' "moveCursorDown" $ Array.index (tooths inside) i
  Cursor {outside: consPath outside tooth, inside: kid}

moveCursorDownRightMost :: forall a. Cursor a -> Cursor a
moveCursorDownRightMost cursor@(Cursor {outside, inside}) =
  case Array.last (tooths inside) of
    Nothing -> cursor
    Just {kid, tooth} -> moveCursorDownRightMost (Cursor {outside: consPath outside tooth, inside: kid})

-- moveCursorRight

moveCursorRight :: forall a. Cursor a -> Maybe (Cursor a)
moveCursorRight cursor@(Cursor {inside: Tree {kids}}) =
  if Array.length kids == 0
    then do
      {upCursor, i, kidsCount} <- moveCursorUp cursor
      if i + 1 == kidsCount
        then moveCursorUpRightNext upCursor
        else pure $ moveCursorDown (i + 1) upCursor
    else pure $ moveCursorDown 0 cursor

moveCursorUpRightNext :: forall a. Cursor a -> Maybe (Cursor a)
moveCursorUpRightNext cursor@(Cursor {inside: Tree {kids}}) = do
  {upCursor, i, kidsCount} <- moveCursorUp cursor
  if i + 1 == kidsCount
    then moveCursorUpRightNext upCursor
    else pure $ moveCursorDown (i + 1) upCursor

-- moveCursorDownLeftMost :: forall a. Cursor a -> Cursor a
-- moveCursorDownLeftMost cursor@(Cursor {outside, inside}) =
--   case Array.index (tooths inside) 0 of
--     Nothing -> cursor
--     Just {kid, tooth} -> moveCursorDownLeftMost (Cursor {outside: consPath outside tooth, inside: kid})

-- moveGyroLeftUntil

moveGyroLeftUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroLeftUntil _cond gyro@(RootGyro _) = moveGyroLeft gyro
moveGyroLeftUntil cond (CursorGyro cursor) = CursorGyro <$> moveCursorLeftUntil cond cursor
moveGyroLeftUntil cond (SelectGyro _) = hole "TODO: moveGyroLeftUntil cond (SelectGyro _)"

moveCursorLeftUntil :: forall a. (a -> Boolean) -> Cursor a -> Maybe (Cursor a)
moveCursorLeftUntil cond cursor = do
  cursorLeft <- moveCursorLeft cursor
  pure $ fromMaybe cursorLeft $ go cursorLeft
  where
  go :: Cursor a -> Maybe (Cursor a)
  go cursor' = do
    cursorLeft@(Cursor {inside: Tree {node}}) <- moveCursorLeft cursor'
    if cond node
      then pure cursorLeft
      else go cursorLeft

-- moveGyroRightUntil

moveGyroRightUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroRightUntil _cond gyro@(RootGyro _) = moveGyroRight gyro
moveGyroRightUntil cond (CursorGyro cursor) = CursorGyro <$> moveCursorRightUntil cond cursor
moveGyroRightUntil cond (SelectGyro _) = hole "TODO"

moveCursorRightUntil :: forall a. (a -> Boolean) -> Cursor a -> Maybe (Cursor a)
moveCursorRightUntil cond cursor = do
  cursorRight <- moveCursorRight cursor
  pure $ fromMaybe cursorRight $ go cursorRight
  where
  go :: Cursor a -> Maybe (Cursor a)
  go cursor' = do
    cursorRight@(Cursor {inside: Tree {node}}) <- moveCursorRight cursor'
    if cond node
      then pure cursorRight
      else go cursorRight
