module Halogen.Utilities where

import Prelude

import Bug (bug)
import Bug as Bug
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
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
updateClassName elemId clsName mb_classValue = do
  elem <- getElementById elemId
  updateElementClassName elem clsName mb_classValue

setClassName :: ElementId -> HH.ClassName -> Effect Unit
setClassName elemId (HH.ClassName className) = do
  elem <- getElementById elemId
  Element.setClassName className elem

setClassNames :: ElementId -> Array HH.ClassName -> Effect Unit
setClassNames elemId classNames = do
  elem <- getElementById elemId
  Element.setClassName (classNames # map unwrap >>> Array.intercalate " ") elem

foreign import fromInputEventToTargetValue :: Event -> Effect String

-- | Update the className of an Element
-- | toggle className: `updateClassName elem className Nothing`
-- | add className: `updateClassName elem className (Just true)`
-- | remove className: `updateClassName elem className (Just false)`
updateElementClassName :: Element -> HH.ClassName -> Maybe Boolean -> Effect Unit
updateElementClassName elem (HH.ClassName className) mb_classValue = do
  classList <- Element.classList elem
  case mb_classValue of
    Nothing -> void $ DOMTokenList.toggle classList className
    Just true -> void $ DOMTokenList.add classList className
    Just false -> void $ DOMTokenList.remove classList className