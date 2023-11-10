module Pantograph.Generic.App.BufferInfo (bufferInfoComponent) where

import Data.Either.Nested
import Data.Tree
import Data.Tree.Swivel
import Data.Tuple.Nested
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Common
import Pantograph.Generic.App.Preview
import Pantograph.Generic.App.Toolbox
import Pantograph.Generic.Dynamics
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Prelude
import Util

import Bug (bug)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Data.Array as Array
import Data.CodePoint.Unicode as CodePoint
import Data.Display (display, embedHtml)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.Tree.Traverse (traverseGyro)
import Data.Tuple (snd)
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Rendering.Common (BufferInfoInput, BufferInfoQuery)
import Pantograph.Generic.Rendering.Hook.Keyboard as Keyboard
import Record as R
import Todo (todo)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent

bufferInfoComponent :: forall sn el ctx env. Dynamics sn el ctx env => H.Component (BufferInfoQuery sn el) (BufferInfoInput sn el ctx env) BufferInfoOutput Aff
bufferInfoComponent = HK.component \{queryToken, slotToken, outputToken} (BufferInfoInput input) -> debug "[render:bufferInfo]" {} \_ -> HK.do

  mbSort /\ mbSortId <- HK.useState (Nothing :: Maybe (Sort sn))
  mbClipboard /\ mbClipboardId <- HK.useState (Nothing :: Maybe (Clipboard sn el))

  let handleQuery :: forall a. BufferInfoQuery sn el a -> HK.HookM Aff (Maybe a)
      handleQuery (BufferInfoQuery query) = (query # _) $ case_
        # on (Proxy :: Proxy "set mbSort") (\(mbSort' /\ a) -> do
            HK.modify_ mbSortId (const mbSort')
            pure (Just a) 
          )
        # on (Proxy :: Proxy "set mbClipboard") (\(mbClipboard' /\ a) -> do
            HK.modify_ mbClipboardId (const mbClipboard')
            pure (Just a) 
          )
  HK.useQuery queryToken handleQuery 
  
  HK.pure do
    El.ℓ [El.Classes [El.BufferInfo]] $ Array.catMaybes [
      mbSort <#> \sort -> embedHtml mempty $
        El.ℓ [El.Classes [El.BufferInfoItem]]
          [ El.ℓ [El.Classes [El.BufferInfoItemInner]]
            [ El.ℓ [El.Classes [El.BufferInfoItemTitle]] [display "sort"]
            , El.ℓ [El.Classes [El.BufferInfoItemValue]] [display sort] ] ]
    ,
      mbClipboard <#> \clipboard -> embedHtml mempty $
        El.ℓ [El.Classes [El.BufferInfoItem]]
          [ El.ℓ [El.Classes [El.BufferInfoItemInner]]
            [ El.ℓ [El.Classes [El.BufferInfoItemTitle]] [display "clipboard"]
            , El.ℓ [El.Classes [El.BufferInfoItemValue]] [displayClipboard clipboard] ] ]
    ]

