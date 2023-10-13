module Data.Tree.Move where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug as Debug
import Text.Pretty (class Pretty, pretty, (<+>))
import Util (fromJust')

repeatApplyUntil :: forall a. (Gyro a -> Maybe (Gyro a)) -> (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
repeatApplyUntil f cond gyro_ = go =<< f gyro_
  where
  go gyro =
    let node = gyroNode gyro in 
    if cond node then Just gyro else
    f gyro

tracePrettyInputOutput :: forall b a. Pretty a => Pretty b => String -> (a -> b) -> a -> b
tracePrettyInputOutput str f a = let fa = f a in Debug.trace (str <+> pretty a <+> "=" <+> pretty (f a)) \_ -> fa

-- Gyro

escapeGyro :: forall a. Gyro a -> Maybe (Gyro a)
escapeGyro (RootGyro _) = Nothing
escapeGyro (CursorGyro (Cursor {outside, inside})) = Just $ RootGyro (unPath outside inside)
escapeGyro (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

moveGyroLeft :: forall a. Gyro a -> Maybe (Gyro a)
moveGyroLeft (CursorGyro cursor) = CursorGyro <$> moveCursorLeft cursor
moveGyroLeft gyro = ensureGyroIsCursor gyro

moveGyroLeftUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroLeftUntil = repeatApplyUntil moveGyroLeft

moveGyroRight :: forall a. Gyro a -> Maybe (Gyro a)
moveGyroRight (CursorGyro cursor) = CursorGyro <$> moveCursorRight cursor
moveGyroRight gyro = ensureGyroIsCursor gyro

moveGyroRightUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroRightUntil = repeatApplyUntil moveGyroRight

grabGyroLeft :: forall a. Gyro a -> Maybe (Gyro a)
grabGyroLeft (SelectGyro select) = grapSelectLeft select
grabGyroLeft gyro = ensureGyroIsSelect OutsideOrientation gyro

grabGyroLeftUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroLeftUntil = repeatApplyUntil grabGyroLeft

grabGyroRight :: forall a. Gyro a -> Maybe (Gyro a)
grabGyroRight (SelectGyro select) = grapSelectRight select
grabGyroRight gyro = ensureGyroIsSelect InsideOrientation gyro

grabGyroRightUntil :: forall a. (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroRightUntil = repeatApplyUntil grabGyroRight

-- If `ensureGyroIsCursor gyro == Nothing` then then `gyro` is already a
-- `Cursor`.
ensureGyroIsCursor :: forall a. Gyro a -> Maybe (Gyro a)
ensureGyroIsCursor (CursorGyro _) = Nothing
ensureGyroIsCursor (RootGyro expr) = Just $ CursorGyro (Cursor {outside: mempty, inside: expr})
ensureGyroIsCursor (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

ensureGyroIsSelect :: forall a. Orientation -> Gyro a -> Maybe (Gyro a)
ensureGyroIsSelect _ (SelectGyro _) = Nothing
ensureGyroIsSelect orientation (RootGyro expr) = do
  Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside: expr}
  Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation}
ensureGyroIsSelect orientation (CursorGyro (Cursor {outside, inside})) =
  case orientation of
    OutsideOrientation -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: singletonNonEmptyPath tooth, inside, orientation}
    InsideOrientation -> do
      Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside}
      Just $ SelectGyro $ Select {outside, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation}

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

-- moveCursorLeftUntil

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

-- moveCursorRightUntil

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

-- Select

escapeSelect :: forall a. Select a -> Cursor a
escapeSelect (Select {outside, middle, inside, orientation}) =
  case orientation of
    OutsideOrientation -> Cursor {outside, inside: unPath (toPath middle) inside}
    InsideOrientation -> Cursor {outside: outside <> toPath middle, inside}

grapSelectUp :: forall a. Select a -> Maybe (Gyro a)
grapSelectUp (Select {outside, middle, inside, orientation}) =
  case orientation of
    OutsideOrientation -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: snocNonEmptyPath tooth middle, inside, orientation}
    InsideOrientation -> do
      case unconsNonEmptyPath middle of
        {outer: Nothing, inner: tooth} -> Just $ CursorGyro $ Cursor {outside, inside: unTooth tooth inside}
        {outer: Just middle', inner: tooth} -> Just $ SelectGyro $ Select {outside, middle: middle', inside: unTooth tooth inside, orientation}

grapSelectLeft :: forall a. Select a -> Maybe (Gyro a)
grapSelectLeft = grapSelectUp

grapSelectRight :: forall a. Select a -> Maybe (Gyro a)
grapSelectRight (Select {outside, middle, inside, orientation}) =
  case orientation of
    OutsideOrientation -> case unsnocNonEmptyPath middle of
      {outer: tooth, inner: Nothing} -> Just $ CursorGyro $ Cursor {outside: consPath outside tooth, inside}
      {outer: tooth, inner: Just middle'} -> Just $ SelectGyro $ Select {outside: consPath outside tooth, middle: middle', inside, orientation}
    InsideOrientation -> do
      Cursor {outside: middle', inside: inside'} <- moveCursorRight (Cursor {outside: toPath middle, inside})
      Just $ SelectGyro $ Select {outside, middle: fromPath "grabSelectRight" middle', inside: inside', orientation}
