module Halogen.Utilities where

import Prelude

import Bug as Bug
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Log as Log
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM as DOM
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

type ElementId
  = String

setClassName ∷ ∀ (m ∷ Type -> Type). MonadEffect m ⇒ Element.Element → String → Boolean → m Unit
setClassName elem className classValue = do
  H.liftEffect do
    classList <- Element.classList elem
    void $ DOMTokenList.toggleForce classList className classValue

setClassNameByElementId ∷ String → String → Boolean → Effect Unit
setClassNameByElementId elemId className classValue = do
  doc <- Window.document =<< HTML.window
  NonElementParentNode.getElementById elemId (Document.toNonElementParentNode $ HTMLDocument.toDocument doc)
    >>= case _ of
        Nothing -> do
          Bug.bug $ "[setClassName] There is no element with this element id: " <> elemId
        Just elem -> setClassName elem className classValue

classNames = HP.classes <<< map HH.ClassName

foreign import fromInputEventToTargetValue :: Event -> Effect String

-- returns "" if the string if the param is not found
foreign import get_url_search_param :: String -> Effect String

foreign import encode_uri_string :: String -> String

foreign import navigator_clipboard_text_ :: Effect (Promise String)

navigator_clipboard_text :: Aff String
navigator_clipboard_text = toAffE navigator_clipboard_text_
