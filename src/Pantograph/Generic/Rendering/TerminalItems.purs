module Pantograph.Generic.Rendering.TerminalItems where

import Prelude hiding (add)

import Data.Tuple.Nested ((/\))
import Bug as Bug
import Data.Array as Array
import Data.Display (Html)
import Data.List (List(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Elements as El
import Halogen.HTML as HH
import Type.Row.Homogeneous (class Homogeneous)
import Util (fromHomogenousRecordToTupleArray)

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

add :: forall a. Html -> (Unit -> a) -> a
add = modifyTerminalItems <<< Cons <<< terminalItem.debug

addM :: forall m. Monad m => Html -> m Unit
addM html = do
  pure unit
  add html \_ -> pure unit

bug :: forall a. Html -> a
bug html = add html (\_ -> Bug.bug "bug info printed to terminal")

debug :: forall r a. Homogeneous r Html => Html -> Record r -> (Unit -> a) -> a
debug title r = 
  let keysAndValues = fromHomogenousRecordToTupleArray r in
  add $
    El.ι $ 
      [title] <>
      if Array.null keysAndValues then [] else
      Array.foldMap (\(k /\ v) -> [El.β [El.ι [El.ℓ [El.Classes [El.TerminalItemDebugRecordKey]] [El.τ k], v]]]) keysAndValues

debugM :: forall m r. Monad m => Homogeneous r Html => Html -> Record r -> m Unit
debugM title r = do
  pure unit
  debug title r \_ -> pure unit
