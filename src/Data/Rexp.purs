module Data.Rexp where

import Prelude

import Data.Variant (Variant)
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Type.Proxy (Proxy)

type FormsRow = Row (Row Type)
class LabelForms (label :: Type) (formsRow :: FormsRow) | label -> formsRow

type Forms = RL.RowList (Row Type)
class LabelFormsList (label :: Type) (forms :: Forms) | label -> forms
instance (LabelForms label formsRow, RowToList formsRow forms) => LabelFormsList label forms

class InterpLabel (label :: Type) (enc :: Type) | label -> enc
instance 
  ( LabelFormsList label forms
  , InterpForms forms enc ) 
  => InterpLabel label enc

class InterpForms (forms :: Forms) (enc :: Type) | forms -> enc
instance InterpForms RL.Nil (Variant ())
instance 
  ( InterpForms forms (Variant vs_) -- forms ==> vs_
  , RowToList fieldsRow fields
  , InterpFields fields r -- fields ==> r
  , R.Cons x r vs_ vs ) -- (x :: r | vs_) == vs
  => InterpForms (RL.Cons x fieldsRow forms) (Variant vs) -- (x :: fields | forms) ==> (x :: r | vs)

data Literal (literal :: Type)
data Labeled (label :: Type)

type Fields = RL.RowList Type

class InterpFields (fields :: Fields) (enc :: Type) | fields -> enc
instance InterpFields RL.Nil (Record ())
instance
  ( InterpFields fields (Record rs_)
  , InterpField a b
  , R.Cons x b rs_ rs )
  => InterpFields (RL.Cons x a fields) (Record rs)

class InterpField (a :: Type) (b :: Type) | a -> b
instance InterpField (Literal a) a
instance 
  ( LabelFormsList label forms
  , InterpForms forms a )
  => InterpField (Labeled label) a

newtype Rexp (label :: Type) (enc :: Type) = Rexp enc
class IsRexp (a :: Type)
instance (InterpLabel label enc) => IsRexp (Rexp label enc)
