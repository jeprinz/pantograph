module Language.Pantograph.Specific.CustomLanguageInterpreter where

import Language.Pantograph.Specific.CustomLanguage
import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Language.Pantograph.Generic.Grammar as Grammar

interpereter :: Grammar.DerivTerm PreSortLabel RuleLabel -> String /\ Unit
interpereter dterm = "TODO: implement CustomLanguageInterpreter" /\ unit
