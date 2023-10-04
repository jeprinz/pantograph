module Pantograph.Generic.Rendering.Terminal where

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

terminalComponent = HK.component \{queryToken} (TerminalInput input) -> HK.do

  -- state
  items /\ itemsStateId <- HK.useState (mempty :: List TerminalItem)

  -- query
  HK.useQuery queryToken case _ of
    WriteTerminal item a -> do
      HK.modify_ itemsStateId (List.Cons item)
      pure (Just a)

  -- render
  HK.pure $ HH.fromPlainHTML $
    HH.div
      [HP.classes [HH.ClassName "Terminal"]] $
      (List.toUnfoldable items <#> \(TerminalItem item) -> do
        HH.div
          [HP.classes [HH.ClassName "TerminalItem"]]
          [ HH.div [HP.classes [HH.ClassName "TerminalItemTag"]] [renderTag item.tag]
          , HH.div [HP.classes [HH.ClassName "TerminalItemContent"]] [item.html] ])

renderTag :: TerminalItemTag -> HH.PlainHTML
renderTag = case _ of
  DebugTerminalItemTag -> HH.div [HP.classes [HH.ClassName "DebugTerminalItemTag"]] [HH.text "[debug]"]
