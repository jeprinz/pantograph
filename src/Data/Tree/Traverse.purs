module Data.Tree.Traverse where

import Data.Tree
import Data.Tuple.Nested
import Prelude
import Util

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))

type Z a = {outside :: Path a, middle :: Path a, inside :: Tree a}
type FZ a m (b :: Type) = Z a -> m b

traverseTree :: forall a m b. Monad m => FZ a m b -> FZ a m (Tree b)
traverseTree f z = Tree <$> f z <*> (traverseTree f <$>> (tooths z.inside <#> \(th /\ inside') -> z {middle = z.middle `consPath` th, inside = inside'}))

traversePath :: forall a m b. Monad m => FZ a m b -> FZ a m (Path b)
traversePath f z@{outside} = case unconsPath z.middle of
  Nothing -> pure mempty
  Just {outer: middle', inner: th@(Tooth _ (i /\ _))} -> do
    let inside' = th `unTooth` z.inside
    outside' <- traversePath f {outside, middle: middle', inside: inside'}
    b <- f {outside, middle: middle', inside: inside'}
    kids' <- map (Tuple i) $ traverseTree f <$>> (fromJust (Array.deleteAt i (tooths inside')) <#> \(thAdj /\ insideAdj) -> 
      {outside, middle: middle' `consPath` thAdj, inside: insideAdj})
    let th' = Tooth b kids'
    pure $ consPath outside' th'

traverseCursor :: forall a m b. Monad m => FZ a m b -> Cursor a -> m (Cursor b)
traverseCursor f (Cursor {outside, inside, orientation}) = do
  outside' <- traversePath f {outside: mempty, middle: outside, inside}
  inside' <- traverseTree f {outside: mempty, middle: outside, inside}
  pure $ Cursor {inside: inside', outside: outside', orientation}

traverseSelect :: forall a m b. Monad m => FZ a m b -> Select a -> m (Select b)
traverseSelect f (Select {outside, middle, inside, orientation}) = do
  outside' <- traversePath f {outside: mempty, middle: outside, inside}
  middle' <- traversePath f {outside, middle: toPath middle, inside} <#> fromPath "traverseSelect"
  inside' <- traverseTree f {outside: mempty, middle: outside, inside}
  pure $ Select {inside: inside', middle: middle', outside: outside', orientation}

traverseGyro :: forall a m b. Monad m => FZ a m b -> Gyro a -> m (Gyro b)
traverseGyro f (RootGyro inside) = RootGyro <$> traverseTree f {outside: mempty, middle: mempty, inside}
traverseGyro f (CursorGyro cursor) = CursorGyro <$> traverseCursor f cursor
traverseGyro f (SelectGyro select) = SelectGyro <$> traverseSelect f select
