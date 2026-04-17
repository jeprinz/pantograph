module Language.Pantograph.Specific.SquareInterpreter (interpreter) where

import Partial.Unsafe (unsafeCrashWith)

interpreter :: forall t1 a2. t1 -> a2
interpreter _dterm = unsafeCrashWith "TODO: implement Square interpreter"
