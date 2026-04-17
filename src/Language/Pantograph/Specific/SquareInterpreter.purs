module Language.Pantograph.Specific.SquareInterpreter (interpreter) where

import Prelude

import Data.Maybe (Maybe)
import Language.Pantograph.Generic.Grammar (SortLabel)
import Language.Pantograph.Generic.Rendering.RunnableEditor (Interpreter)
import Language.Pantograph.Specific.Square (LangD(..), PreSortLabel, RuleLabel)
import Partial.Unsafe (unsafeCrashWith)

interpreter :: forall l r. Interpreter PreSortLabel RuleLabel LangD
interpreter _dterm = unsafeCrashWith "TODO: implement Square interpreter"
