module Halogen.Utilities where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

type ElementId = String

setClassName elem className classValue = do
  H.liftEffect do
    classList <- Element.classList elem
    void $ DOMTokenList.toggleForce classList className classValue

setClassNameByElementId elemId className classValue = do
  doc <- Window.document =<< HTML.window
  NonElementParentNode.getElementById elemId (Document.toNonElementParentNode $ HTMLDocument.toDocument doc) >>= case _ of
    Nothing -> do
      -- unsafeCrashWith $ "couldn't find element ref label"
      log $ "[ClassList.setClassName] There is no element with this element id: " <> elemId
    Just elem -> setClassName elem className classValue


classNames = HP.classes <<< map HH.ClassName

foreign import fromInputEventToTargetValue :: Event -> Effect String