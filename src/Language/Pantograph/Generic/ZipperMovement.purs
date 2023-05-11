module Language.Pantograph.Generic.ZipperMovement where

import Data.Either.Nested
import Data.Tuple.Nested
import Prelude
import Type.Direction

import Data.Array as Array
import Data.Either (Either(..))
import Data.Gram (Path(..), Path1, Zipper, Zipper', stepPath1, unstepPath, zipDowns, zipLeft, zipRight, zipUp)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Partial.Unsafe (unsafeCrashWith)

data MoveDir
  = MoveUp | MoveDown | MoveLeft | MoveRight 
  | MovePrev | MoveNext

moveZipper :: forall l. MoveDir -> Zipper l -> Maybe (Zipper l)
moveZipper MoveUp    z = snd <$> zipUp z
moveZipper MoveDown  z = Array.head $ zipDowns z
moveZipper MoveLeft  z = zipLeft z
moveZipper MoveRight z = zipRight z
moveZipper MovePrev  z = Nothing
moveZipper MoveNext  z = Nothing

moveZipper' :: forall l. MoveDir -> Zipper' l -> Maybe (Zipper l \/ Zipper' l)
moveZipper' MoveUp zp@{path, dir: Up, path1} = do
  th /\ path' <- unstepPath path
  pure <<< pure $ zp {path = path', path1 = stepPath1 th (Just path1)}
moveZipper' MoveDown zp@{dir: Up} = unsafeCrashWith "TODO"
moveZipper' MoveLeft zp@{dir: Up} = unsafeCrashWith "TODO"
moveZipper' MoveRight zp@{dir: Up} = unsafeCrashWith "TODO"
moveZipper' MovePrev zp@{dir: Up} = unsafeCrashWith "TODO"
moveZipper' MoveNext zp@{dir: Up} = unsafeCrashWith "TODO"
moveZipper' MoveUp zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' MoveDown zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' MoveLeft zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' MoveRight zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' MovePrev zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' MoveNext zp@{dir: Down} = unsafeCrashWith "TODO"
