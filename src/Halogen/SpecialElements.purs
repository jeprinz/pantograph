module Halogen.SpecialElements where

import Prelude

import Data.Display (Html)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse, traverse_)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Class (liftEffect)
import Halogen.Elements as El
import Halogen.Utilities as HU
import Util (fromJust)
import Web.DOM.Document as Document
import Web.DOM.HTMLCollection as HTMLCollection
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

uuidSplotch :: UUID -> Array Html -> Html
uuidSplotch uuid body = 
  El.ℓ 
    [ El.Style style
    , El.Classes [El.UuidSplotch, El.ClassName $ uuidClassName]
    , El.OnMouseOver \_mouseEvent -> do
        doc <- liftEffect $ Window.document =<< HTML.window
        elems <- liftEffect $ Document.getElementsByClassName uuidClassName (HTMLDocument.toDocument doc) >>= HTMLCollection.toArray
        elems # traverse_ \elem -> liftEffect $ El.updateElementClassName elem (El.ClassName "LinkedUuidSplotch") (Just true)
    , El.OnMouseOut \_mouseEvent -> do
        doc <- liftEffect $ Window.document =<< HTML.window
        elems <- liftEffect $ Document.getElementsByClassName uuidClassName (HTMLDocument.toDocument doc) >>= HTMLCollection.toArray
        elems # traverse_ \elem -> liftEffect $ El.updateElementClassName elem (El.ClassName "LinkedUuidSplotch") (Just false)
    ] 
    body
  where
  uuidString = uuid # UUID.toString >>> String.take 6
  uuidClassName = "UuidSplotch-" <> uuidString
  n = uuidString # Int.fromStringAs Int.hexadecimal >>> fromJust >>> (_ `mod` 360)
  h  = n # Int.toNumber
  h' = ((n + 180) `mod` 360) # Int.toNumber
  s = 100.0
  l = 50.0
  color1 = "hsl(" <> show h  <> ", " <> show s <> "%, " <> show l <> "%)"
  color2 = "hsl(" <> show h' <> ", " <> show s <> "%, " <> show l <> "%)"
  style = 
    "color:            " <> color1 <> ";" <>
    "background-color: " <> color2 <> ";" <>
    "outline-color:     " <> color1 <> ";"
