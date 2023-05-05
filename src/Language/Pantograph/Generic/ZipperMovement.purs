module Language.Pantograph.Generic.ZipperMovement where

import Prelude

import Data.Array as Array
import Data.Gram (Path(..), Zipper, zipDowns, zipLeft, zipRight, zipUp)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)

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
