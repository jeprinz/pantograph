module Data.Rexp.Example1 where

import Prelude

import Data.Rexp as Rexp
import Data.Variant (Variant, case_, inj, on)
import Type.Proxy (Proxy(..))

--
-- define a new language
--

_string = Proxy :: Proxy "string"
_var = Proxy :: Proxy "var"

data TermLabel_
instance Rexp.LabelForms TermLabel_ 
  ( string :: (value :: Rexp.Literal String) 
  , var :: (var :: Rexp.Labeled VarLabel_) )
type TermEnc = Variant
  ( string :: Record (value :: String) 
  , var :: Record (var :: Var) )
type Term = Rexp.Rexp TermLabel_ TermEnc

data VarLabel_
instance Rexp.LabelForms VarLabel_ (string :: (name :: String))
type VarEnc = Variant (string :: Record (name :: String))
type Var = Rexp.Rexp VarLabel_ VarEnc

{-
--
-- required for each language-specific function
--

class Rexp.IsRexp label enc <= ShowLabel label enc where
  showLabel :: Rexp.Rexp label enc -> String

-- instance Rexp.InterpLabel TermLabel_ TermEnc => ShowLabel Term where
--   showLabel (Term term) = case_
--     # on _string (\_ -> "Term")
--     # on _var (\_ -> "Var")
--     $ term

exampleTerm1 :: Term
exampleTerm1 = Rexp.Rexp (inj _string {value: "example value 1"})
-}