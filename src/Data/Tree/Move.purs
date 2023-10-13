module Data.Tree.Move where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug as Debug
import Hole (hole)
import Text.Pretty (class Pretty, pretty, (<+>))
import Util (fromJust')

repeatApplyUntil :: forall a. PrettyTreeNode a => (Gyro a -> Maybe (Gyro a)) -> (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
repeatApplyUntil f cond gyro_ = go =<< f gyro_
  where
  go gyro =
    let node = gyroNode gyro in 
    if cond node then Just gyro else
    f gyro

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

moveGyroLeftUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroLeftUntil = repeatApplyUntil moveGyroLeft

moveGyroRight :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
moveGyroRight (CursorGyro cursor) = CursorGyro <$> moveCursorRight cursor
moveGyroRight gyro = ensureGyroIsCursor gyro

moveGyroRightUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
moveGyroRightUntil = repeatApplyUntil moveGyroRight

grabGyroLeft = tracePrettyInputOutput "grabGyroLeft" grabGyroLeft'
  where
  grabGyroLeft' :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
  grabGyroLeft' (SelectGyro select) = grapSelectLeft select
  grabGyroLeft' gyro = ensureGyroIsSelect Outside gyro

grabGyroLeftUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroLeftUntil = repeatApplyUntil grabGyroLeft

grabGyroRight = tracePrettyInputOutput "grabGyroRight" grabGyroRight'
  where
  grabGyroRight' :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
  grabGyroRight' (SelectGyro select) = grapSelectRight select
  grabGyroRight' gyro = ensureGyroIsSelect Inside gyro

grabGyroRightUntil :: forall a. PrettyTreeNode a => (a -> Boolean) -> Gyro a -> Maybe (Gyro a)
grabGyroRightUntil = repeatApplyUntil grabGyroRight

-- If `ensureGyroIsCursor gyro == Nothing` then then `gyro` is already a
-- `Cursor`.
ensureGyroIsCursor :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
ensureGyroIsCursor (CursorGyro _) = Nothing
ensureGyroIsCursor (RootGyro expr) = Just $ CursorGyro (Cursor {outside: mempty, inside: expr, orientation: Outside})
ensureGyroIsCursor (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

ensureGyroIsSelect :: forall a. PrettyTreeNode a => Orientation -> Gyro a -> Maybe (Gyro a)
ensureGyroIsSelect _ (SelectGyro _) = Nothing
ensureGyroIsSelect target (RootGyro expr) = do
  Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside: expr, orientation: Inside}
  Just $ SelectGyro $ Select {outside: mempty, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: target}
ensureGyroIsSelect target (CursorGyro (Cursor {outside, inside})) =
  case target of
    Outside -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: singletonNonEmptyPath tooth, inside, orientation: target}
    Inside -> do
      Cursor {outside: middle, inside: inside'} <- moveCursorRight $ Cursor {outside: mempty, inside, orientation: Inside}
      Just $ SelectGyro $ Select {outside, middle: fromPath "ensureGyroIsSelect" middle, inside: inside', orientation: target }

-- Cursor

-- moveCursorUp

moveCursorUp :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a \/ {upCursor :: Cursor a, i :: Int, kidsCount :: Int})
moveCursorUp (Cursor {outside, inside, orientation}) = case orientation of
  Outside -> case outside of
    Path Nil -> Nothing
    Path (Cons tooth@(Tooth {i, kids}) tooths) -> Just $ Right $ {upCursor: Cursor {outside: Path tooths, inside: unTooth tooth inside, orientation: Inside}, i, kidsCount: Array.length kids + 1}
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
    let {kid, tooth} = fromJust' "moveCursorDown" $ Array.index (tooths inside) i
    Cursor {outside: consPath outside tooth, inside: kid, orientation: Outside}

moveCursorDownRightMost :: forall a. PrettyTreeNode a => Cursor a -> Cursor a
moveCursorDownRightMost cursor@(Cursor {outside, inside, orientation}) =
  case orientation of
    Outside ->
      Cursor {outside, inside, orientation: Inside}
    Inside -> 
      case Array.last (tooths inside) of
        Nothing -> cursor
        Just {kid, tooth} -> moveCursorDownRightMost (Cursor {outside: consPath outside tooth, inside: kid, orientation: Outside})

-- moveCursorRight

moveCursorRight :: forall a. PrettyTreeNode a => Cursor a -> Maybe (Cursor a)
moveCursorRight cursor@(Cursor {outside, inside: inside@(Tree {kids}), orientation}) = case orientation of
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
moveCursorUpRightNext cursor@(Cursor {inside: Tree {kids}}) =
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
    cursorLeft@(Cursor {inside: Tree {node}}) <- moveCursorLeft cursor'
    if cond node
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
    cursorRight@(Cursor {inside: Tree {node}}) <- moveCursorRight cursor'
    if cond node
      then pure cursorRight
      else go cursorRight

-- Select

escapeSelect = tracePrettyInputOutput "escapeSelect" escapeSelect'
  where
  escapeSelect' :: forall a. PrettyTreeNode a => Select a -> Cursor a
  escapeSelect' (Select {outside, middle, inside, orientation}) =
    case orientation of
      Outside -> Cursor {outside, inside: unPath (toPath middle) inside, orientation: Outside}
      Inside -> Cursor {outside: outside <> toPath middle, inside, orientation: Inside}

grapSelectUp :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grapSelectUp (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> do
      {outer: outside', inner: tooth} <- unconsPath outside
      Just $ SelectGyro $ Select {outside: outside', middle: snocNonEmptyPath tooth middle, inside, orientation}
    Inside -> do
      case unconsNonEmptyPath middle of
        {outer: Nothing, inner: tooth} -> Just $ CursorGyro $ Cursor {outside, inside: unTooth tooth inside, orientation: Inside}
        {outer: Just middle', inner: tooth} -> Just $ SelectGyro $ Select {outside, middle: middle', inside: unTooth tooth inside, orientation}

grapSelectLeft :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grapSelectLeft = grapSelectUp

grapSelectRight :: forall a. PrettyTreeNode a => Select a -> Maybe (Gyro a)
grapSelectRight (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> case unsnocNonEmptyPath middle of
      {outer: tooth, inner: Nothing} -> Just $ CursorGyro $ Cursor {outside: consPath outside tooth, inside, orientation: Outside}
      {outer: tooth, inner: Just middle'} -> Just $ SelectGyro $ Select {outside: consPath outside tooth, middle: middle', inside, orientation}
    Inside -> do
      Cursor {outside: middle', inside: inside'} <- moveCursorRight (Cursor {outside: toPath middle, inside, orientation: Inside})
      Just $ SelectGyro $ Select {outside, middle: fromPath "grabSelectRight" middle', inside: inside', orientation}
