module Pantograph.Generic.Rendering.Console where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK

consoleComponent = HK.component \{queryToken} (ConsoleInput input) -> HK.do

  -- state
  items /\ itemsStateId <- HK.useState (mempty :: List ConsoleItem)

  -- query
  HK.useQuery queryToken case _ of
    WriteConsole item a -> do
      HK.modify_ itemsStateId (List.Cons item)
      pure (Just a)

  -- render
  HK.pure $ HH.fromPlainHTML $
    HH.div
      [HP.classes [HH.ClassName "Console"]] $
      (List.toUnfoldable items <#> \(ConsoleItem item) -> do
        HH.div
          [HP.classes [HH.ClassName "ConsoleItem"]]
          [ HH.div [HP.classes [HH.ClassName "ConsoleItemTag"]] [renderTag item.tag]
          , HH.div [HP.classes [HH.ClassName "ConsoleItemContent"]] [item.html] ])

renderTag :: ConsoleItemTag -> HH.PlainHTML
renderTag = case _ of
  DebugConsoleItemTag -> HH.div [HP.classes [HH.ClassName "DebugConsoleItemTag"]] [HH.text "[debug]"]
