module Pantograph.Generic.Rendering.Terminal.TerminalItems where

import Prelude hiding (add)

import Bug as Bug
import Data.Display (Html)
import Data.List (List(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Elements as El
import Halogen.HTML as HH

data TerminalItemTag = DebugTerminalItemTag
newtype TerminalItem = TerminalItem {tag :: TerminalItemTag, html :: Html}

terminalItem = {debug, debugString}
  where
  debug html = TerminalItem {tag: DebugTerminalItemTag, html}
  debugString = debug <<< El.text

terminalItemsRef :: Ref (List TerminalItem)
terminalItemsRef = unsafePerformEffect $ Ref.new Nil

getTerminalItems :: Unit -> List TerminalItem
getTerminalItems _ = unsafePerformEffect $ Ref.read terminalItemsRef

modifyTerminalItems :: forall a. (List TerminalItem -> List TerminalItem) -> (Unit -> a) -> a
modifyTerminalItems f k = k (unsafePerformEffect $ Ref.modify_ f terminalItemsRef)

add = modifyTerminalItems <<< Cons <<< terminalItem.debug

bug html = add html (\_ -> Bug.bug "bug info printed to terminal")