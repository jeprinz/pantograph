module Type.Direction where

import Prelude
import Prim.Row

import Control.Plus (empty)
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

-- Jacob: what is this file? Is this AI generated? Does this code need to use variants and be so repetetive and strange?

-- proxies
_up    = Proxy :: Proxy "up"
_down  = Proxy :: Proxy "down"
_left  = Proxy :: Proxy "left"
_right = Proxy :: Proxy "right"
_prev  = Proxy :: Proxy "prev"
_next  = Proxy :: Proxy "next"

-- symbols
type Up    = "up"
type Down  = "down"
type Left  = "left"
type Right = "right"
type Prev  = "prev"
type Next  = "next"

-- atomic
type UpDir    dirs = (up    :: Proxy Up    | dirs)
type DownDir  dirs = (down  :: Proxy Down  | dirs)
type LeftDir  dirs = (left  :: Proxy Left  | dirs)
type RightDir dirs = (right :: Proxy Right | dirs)
type PrevDir  dirs = (prev  :: Proxy Prev  | dirs)
type NextDir  dirs = (next  :: Proxy Next  | dirs)

-- up, down
type VerticalDirs dirs = UpDir (DownDir dirs)
-- left, right
type HorizontalDirs dirs = LeftDir (RightDir dirs)
-- prev, next
type OrdinalDirs dirs = NextDir (PrevDir dirs)
-- up, down, left, right
type CompassDirs dirs = VerticalDirs (HorizontalDirs dirs)
-- up, down, left, right, prev, next
type MoveDirs dirs = OrdinalDirs (CompassDirs dirs)

-- dir values
type VerticalDir = Variant (VerticalDirs ())
type HorizontalDir = Variant (HorizontalDirs ())
type OrdinalDir = Variant (OrdinalDirs ())
type CompassDir = Variant (CompassDirs ())
type MoveDir = Variant (MoveDirs ())

class Opposite (dir1 :: Symbol) (dir2 :: Symbol) | dir1 -> dir2
instance Opposite Up Down
instance Opposite Down Up
instance Opposite Left Right
instance Opposite Right Left
instance Opposite Next Prev
instance Opposite Prev Next

upDir :: forall dirs. Variant (UpDir dirs)
upDir = inj _up (Proxy :: Proxy Up)

downDir :: forall dirs. Variant (DownDir dirs)
downDir = inj _down (Proxy :: Proxy Down)

leftDir :: forall dirs. Variant (LeftDir dirs)
leftDir = inj _left (Proxy :: Proxy Left)

rightDir :: forall dirs. Variant (RightDir dirs)
rightDir = inj _right (Proxy :: Proxy Right)

prevDir :: forall dirs. Variant (PrevDir dirs)
prevDir = inj _prev (Proxy :: Proxy Prev)

nextDir :: forall dirs. Variant (NextDir dirs)
nextDir = inj _next (Proxy :: Proxy Next)

readMoveDir :: String -> Maybe MoveDir
readMoveDir "ArrowLeft" = pure prevDir
readMoveDir "ArrowRight" = pure nextDir
--readMoveDir "ArrowLeft" = pure leftDir
--readMoveDir "ArrowRight" = pure rightDir
readMoveDir "ArrowUp" = pure upDir
readMoveDir "ArrowDown" = pure downDir
readMoveDir _ = empty

readVerticalDir :: String -> Maybe VerticalDir
readVerticalDir "ArrowUp" = pure upDir
readVerticalDir "ArrowDown" = pure downDir
readVerticalDir _ = empty