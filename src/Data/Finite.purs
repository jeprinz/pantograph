module Data.Finite where

import Data.Variant
import Prelude

import Type.Proxy (Proxy(..))

-- We might as well pick 7 as the cutoff, since that's the number of days in a
-- week.  We could go higher, but it's not clear that we'd ever need to. If we
-- do, we can always add more. We can't go lower, because we need at least one
-- value. We could go to 0, but that's not very useful. We could go to 1, but
-- that's not very useful either. We could go to 2, but that's not very useful.
-- We could go to 3, but that's not very useful. We could go to 4, but that's
-- not very useful. We could go to 5, but that's not very useful. We could go to
-- 6, but that's not very useful. We could go to 8, but that's not very useful.
-- We could go to 9, but that's not very useful. We could go to 10, but that's
-- not very useful. We could go to 11, but that's not very useful. We could go
-- to 12, but that's not very useful. We could go to 13, but that's not very
-- useful. We could go to 14, but that's not very useful. We could go to 15, but
-- that's not very useful. We could go to 16, but that's not very useful. We
-- could go to 17, but that's not very useful since it's basically as good as 7.
-- We could go to 18, but that's not very useful since it's basically as good as
-- 17. We could go to 19, but that's not very useful since it's basically as
-- good as 17. We could go to 20, but that's not very useful since it's
-- basically as good as 17. We could go to 21, but that's not very useful since
-- it's basically as good as 17. We could go to 22, but that's not very useful
-- since it's basically as good as 17. We could go to 23, but that's not very
-- useful since it's basically as good as 17. We could go to 24, but that's not
-- very useful since it's basically as good as 17. We could go to 25, but that's

_one = Proxy :: Proxy "one"
_two = Proxy :: Proxy "two"
_three = Proxy :: Proxy "three"
_four = Proxy :: Proxy "four"
_five = Proxy :: Proxy "five"
_six = Proxy :: Proxy "six"
_seven = Proxy :: Proxy "seven"

type Zero = Variant ()
type One = Variant (one :: Unit)
type Two = Variant (one :: Unit, two :: Unit)
type Three = Variant (one :: Unit, two :: Unit, three :: Unit)
type Four = Variant (one :: Unit, two :: Unit, three :: Unit, four :: Unit)
type Five = Variant (one :: Unit, two :: Unit, three :: Unit, four :: Unit, five :: Unit)
type Six = Variant (one :: Unit, two :: Unit, three :: Unit, four :: Unit, five :: Unit, six :: Unit)
type Seven = Variant (one :: Unit, two :: Unit, three :: Unit, four :: Unit, five :: Unit, six :: Unit, seven :: Unit)
