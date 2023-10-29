module Pantograph.Generic.Rendering.Terminal.TerminalItems where

import Prelude

import Data.List (List(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML as HH

data TerminalItemTag = DebugTerminalItemTag
newtype TerminalItem = TerminalItem {tag :: TerminalItemTag, html :: HH.PlainHTML}

terminalItem = {debug, debugString}
  where
  debug html = TerminalItem {tag: DebugTerminalItemTag, html}
  debugString = debug <<< HH.text

terminalItemsRef :: Ref (List TerminalItem)
terminalItemsRef = unsafePerformEffect $ Ref.new Nil

getTerminalItems :: Unit -> List TerminalItem
getTerminalItems _ = unsafePerformEffect $ Ref.read terminalItemsRef

modifyTerminalItems :: forall a. (List TerminalItem -> List TerminalItem) -> (Unit -> a) -> a
modifyTerminalItems f k = k (unsafePerformEffect $ Ref.modify_ f terminalItemsRef)

add = modifyTerminalItems <<< Cons <<< terminalItem.debug