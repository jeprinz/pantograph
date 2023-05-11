module Language.Pantograph.Generic.ZipperMovement where

import Data.Either.Nested
import Data.Expr
import Data.Tuple.Nested
import Prelude
import Type.Direction

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Variant (case_, on)
import Partial.Unsafe (unsafeCrashWith)

-- data MoveDir
--   = MoveUp | MoveDown | MoveLeft | MoveRight 
--   | MovePrev | MoveNext

moveZipper :: forall l. MoveDir -> Zipper l -> Maybe (Zipper l)
-- moveZipper MoveNext  z = Nothing
moveZipper = case_
  # on _up (\_ -> map snd <<< zipUp)
  # on _down (\_ -> Array.head <<< zipDowns)
  # on _left (\_ -> zipLeft)
  # on _right (\_ -> zipRight)
  # on _prev (\_ -> unsafeCrashWith "!TODO moveZipper prev")
  # on _next (\_ -> unsafeCrashWith "!TODO moveZipper next")

moveZipper' :: forall l. MoveDir -> Zipper' l -> Maybe (Zipper l \/ Zipper' l)
-- moveZipper' MoveUp zp@{path, dir: Up, path1} = do
--   th /\ path' <- unstepPath path
--   pure <<< pure $ zp {path = path', path1 = stepPath1 th (Just path1)}
-- moveZipper' MoveDown zp@{dir: Up} = unsafeCrashWith "TODO"
-- moveZipper' MoveLeft zp@{dir: Up} = unsafeCrashWith "TODO"
-- moveZipper' MoveRight zp@{dir: Up} = unsafeCrashWith "TODO"
-- moveZipper' MovePrev zp@{dir: Up} = unsafeCrashWith "TODO"
-- moveZipper' MoveNext zp@{dir: Up} = unsafeCrashWith "TODO"
-- moveZipper' MoveUp zp@{dir: Down} = unsafeCrashWith "TODO"
-- moveZipper' MoveDown zp@{dir: Down} = unsafeCrashWith "TODO"
-- moveZipper' MoveLeft zp@{dir: Down} = unsafeCrashWith "TODO"
-- moveZipper' MoveRight zp@{dir: Down} = unsafeCrashWith "TODO"
-- moveZipper' MovePrev zp@{dir: Down} = unsafeCrashWith "TODO"
-- moveZipper' MoveNext zp@{dir: Down} = unsafeCrashWith "TODO"
moveZipper' _ _ = unsafeCrashWith "TODO"

