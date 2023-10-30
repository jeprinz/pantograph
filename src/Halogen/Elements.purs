module Halogen.Elements where

import Prelude

import Bug (bug)
import DOM.HTML.Indexed as HTML
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities as HU
import Todo (todo)
import Util (debug)
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Props i = Array (Prop i)

data Prop i
  = Classes (Array ClassName)
  | Id HU.ElementId
  | StrictHover i
  | OnMouseDown (MouseEvent -> i)
  | OnMouseUp (MouseEvent -> i)

data ClassName
  = Whitespace
  | Punctuation
  | VarSN
  | Inline
  | Block

derive instance Generic ClassName _
instance Show ClassName where show = genericShow

compileProp :: forall i. Prop i -> Array (HP.IProp HTML.HTMLdiv i)
compileProp (Id elemId) = [ HU.id elemId ]
compileProp (Classes clsNames) = [ HP.classes $ HH.ClassName <<< show <$> clsNames ]
compileProp (OnMouseDown k) = [ HE.onMouseDown k ]
compileProp (OnMouseUp k) = [ HE.onMouseUp k ]
compileProp (StrictHover i) = 
  [ HE.onMouseOver \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      let target = case Event.target event >>= Element.fromEventTarget of
            Nothing -> bug "[StrictHover] invalid target"
            Just target_ -> target_
      HU.updateElementClassName target (HH.ClassName "hover") (Just true)
      pure i
  , HE.onMouseOut \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      let target = case Event.target event >>= Element.fromEventTarget of
            Nothing -> bug "[StrictHover] invalid target"
            Just target_ -> target_
      HU.updateElementClassName target (HH.ClassName "hover") (Just false)
      pure i
  ]

element :: forall w i. Array (Prop i) -> Array (HH.HTML w i) -> HH.HTML w i
element props = HH.div (Array.foldMap compileProp props)

ℓ = element

whitespace string = ℓ [Classes [Whitespace]] [HH.text string]
punctuation string = ℓ [Classes [Punctuation]] [HH.text string]
inline = ℓ [Classes [Inline]]
block = ℓ [Classes [Block]]
