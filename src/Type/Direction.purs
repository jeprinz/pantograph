module Type.Direction where

class Dir (d :: Symbol)
instance Dir "down"
instance Dir "up"

class (Dir d1, Dir d2) <= Rev d1 d2 | d1 -> d2
instance Rev "down" "up"
instance Rev "up" "down"
