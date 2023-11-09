module Pantograph.Library.Rendering where

import Prelude

import CSS as CSS
import DOM.HTML.Indexed (HTMLimg)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Display (Html, display)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Debug as Debug
import Effect.Aff (Aff)
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Javascript as Javascript
import Pantograph.Generic.Rendering (ArrangeKid(..))
import Todo (todo)
import Util (fromJust)

-- Arrangable

class Arrangable f where 
  arrange :: forall (sn :: Type) (el :: Type). f ~> ArrangeKid sn el

instance Arrangable Identity                         where arrange (Identity a)       = ArrangeKid a
instance Arrangable (Const String)                   where arrange (Const string)     = ArrangeHtml [El.π string]
instance Arrangable (Const (Array Html))             where arrange (Const htmls)      = ArrangeHtml htmls 
instance Arrangable (Const (Array String))           where arrange (Const cns)        = ArrangeHtml [El.ℓ [El.Classes $ El.ClassName <$> cns] []] 
instance Arrangable (Const (Array String /\ String)) where arrange (Const (cns /\ t)) = ArrangeHtml [El.ℓ [El.Classes $ El.ClassName <$> cns] [El.τ t]]

consArrangable :: forall sn el f a. Arrangable f => f a -> List (ArrangeKid sn el a) -> List (ArrangeKid sn el a)
consArrangable a aks = List.Cons (arrange a) aks

consIdentityArrangable :: forall sn el a. a -> List (ArrangeKid sn el a) -> List (ArrangeKid sn el a)
consIdentityArrangable a = consArrangable (Identity a)

consConstArrangable :: forall sn el a b. Arrangable (Const a) => a -> List (ArrangeKid sn el b) -> List (ArrangeKid sn el b)
consConstArrangable a = consArrangable (Const a)

infixr 6 consIdentityArrangable as ˜⊕
infixr 6 consConstArrangable as ⊕

-- π

π =
  { "=":          ["keysymbol", "equal"]                              /\ "="
  , "\"":         ["keysymbol", "quote2"]                             /\ "\""
  , ":":          ["keysymbol", "colon"]                              /\ ":"
  , ".":          ["keysymbol", "period"]                             /\ "."
  , "#":          ["keysymbol", "period"]                             /\ "♯"
  , "(":          ["keysymbol", "paren-left"]                         /\ "("
  , ")":          ["keysymbol", "paren-right"]                        /\ ")"
  , "{":          ["keysymbol", "brace-left"]                         /\ "{"
  , "}":          ["keysymbol", "brace-right"]                        /\ "}"
  , "{{":         ["keysymbol", "double-brace-left"]                  /\ "⦃"
  , "}}":         ["keysymbol", "double-brace-right"]                 /\ "⦄"
  , "^...":       ["keysymbol", "dotted-fence"]                       /\ "⦙"
  , "->":         ["keysymbol", "arrow-right"]                        /\ "→"
  , "?":          ["keysymbol", "interrogative"]                      /\ "?"
  , "box":        ["keysymbol", "box"]                                /\ "◌" -- "◯"
  , "λ":          ["keysymbol", "lambda"]                             /\ "λ"
  , "~":          ["keysymbol", "emptyString"]                        /\ "◌" -- "◯" -- "~"
  , "let":        ["keysymbol", "let"]                                /\ "let"
  , "in":         ["keysymbol", "in"]                                 /\ "in"
  , "Z":          ["keysymbol", "zero"]                               /\ "Z"
  , "S":          ["keysymbol", "suc"]                                /\ "S"
  , "F":          ["keysymbol", "free"]                               /\ "F"
  , "if":         ["keysymbol", "if"]                                 /\ "if"
  , "then":       ["keysymbol", "then"]                               /\ "then"
  , "else":       ["keysymbol", "else"]                               /\ "else"
  , "/*":         ["keysymbol", "comment-left"] /\ ""
  , "*/":         ["keysymbol", "comment-right"] /\ ""
  }

-- special rendering

-- uuidSplotch :: UUID -> Array HH.ClassName -> Html
-- uuidSplotch uuid classNames = HH.img ([HP.src src, HP.classes classNames])
--   where
--   cps = Array.take 8 $ String.toCodePointArray $ UUID.toString uuid
--   byteStrings = map String.fromCodePointArray $ map (cps # _) $ [Array.slice 0 2, Array.slice 2 4, Array.slice 4 6, Array.slice 6 8]
--   bytes = byteStrings <#> Int.fromStringAs Int.hexadecimal >>> fromJust
--   src = Javascript.fromByteArrayToImageSrc bytes 32

