module Bug where

import Prelude

import Data.Array as Array
import Partial.Unsafe (unsafeCrashWith)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ Array.intercalate "\n"
  [ ""
  , "==[ BUG ]================================================================="
  , msg
  , "=========================================================================="
  , ""
  ]
