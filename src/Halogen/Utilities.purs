module Halogen.Utilities where

import Prelude
import Bug as Bug
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen as HH
import Halogen.HTML.Properties as HP
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

newtype ElementId = ElementId String

setClassName ∷ ∀ (m ∷ Type -> Type). MonadEffect m ⇒ Element.Element → String → Boolean → m Unit
setClassName elem className classValue = do
  H.liftEffect do
    classList <- Element.classList elem
    void $ DOMTokenList.toggleForce classList className classValue

setClassNameByElementId ∷ ElementId → String → Boolean → Effect Unit
setClassNameByElementId (ElementId elemId) className classValue = do
  doc <- Window.document =<< HTML.window
  NonElementParentNode.getElementById elemId (Document.toNonElementParentNode $ HTMLDocument.toDocument doc) >>= case _ of
    Nothing -> do
      Bug.bug $ "[setClassName] There is no element with this element id: " <> elemId
    Just elem -> setClassName elem className classValue


classNames = HP.classes <<< map HH.ClassName

foreign import fromInputEventToTargetValue :: Event -> Effect String
