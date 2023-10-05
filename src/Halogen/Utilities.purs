module Halogen.Utilities where

import Prelude

import Bug (bug)
import Bug as Bug
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen as HH
import Halogen.HTML.Properties as HP
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

newtype ElementId = ElementId String

derive newtype instance Show ElementId

id :: forall r i. ElementId -> HP.IProp (id :: String | r) i
id (ElementId str) = HP.id str

freshElementId :: Unit -> ElementId
freshElementId _ = ElementId $ UUID.toString $ unsafePerformEffect UUID.genUUID

getElementById :: ElementId -> Effect Element
getElementById (ElementId elemId) = do
  doc <- Window.document =<< HTML.window
  NonElementParentNode.getElementById elemId (Document.toNonElementParentNode $ HTMLDocument.toDocument doc) >>= case _ of
    Nothing -> bug $ "Can't find element with id: " <> show elemId
    Just elem -> pure elem

-- | Update the className of an element by ElementId
-- | toggle className: `updateClassName elemId className Nothing`
-- | add className: `updateClassName elemId className (Just true)`
-- | remove className: `updateClassName elemId className (Just false)`
updateClassName :: ElementId -> HH.ClassName -> Maybe Boolean -> Effect Unit
updateClassName elemId (HH.ClassName className) mb_classValue = do
  classList <- Element.classList =<< getElementById elemId
  case mb_classValue of
    Nothing -> void $ DOMTokenList.toggle classList className
    Just true -> void $ DOMTokenList.add classList className
    Just false -> void $ DOMTokenList.remove classList className

setClassName :: ElementId -> HH.ClassName -> Effect (Effect Unit)
setClassName elemId (HH.ClassName className) =
  Element.setClassName className <$> getElementById elemId

foreign import fromInputEventToTargetValue :: Event -> Effect String
