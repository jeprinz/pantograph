module Pantograph.Generic.GlobalMessageBoard where

import Prelude hiding (add)

import Bug as Bug
import Data.Array as Array
import Data.Display (Html)
import Data.List (List(..))
import Data.List as List
import Data.Tuple.Nested ((/\))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Elements as El
import Partial.Unsafe (unsafeCrashWith)
import Type.Row.Homogeneous (class Homogeneous)
import Util (fromHomogenousRecordToTupleArray)

newtype GlobalMessage 
  = GlobalMessage {tag :: GlobalMessageTag, html :: Html}

data GlobalMessageTag 
  = DebugGlobalMessageTag
  | ErrorGlobalMessageTag
  | InfoGlobalMessageTag

terminalItemsRef :: Ref (List GlobalMessage)
terminalItemsRef = unsafePerformEffect $ Ref.new Nil

getGlobalMessages :: Unit -> List GlobalMessage
getGlobalMessages _ = unsafePerformEffect $ Ref.read terminalItemsRef

maximumGlobalMessages :: Int
maximumGlobalMessages = 100

modifyGlobalMessages :: forall a. (List GlobalMessage -> List GlobalMessage) -> (Unit -> a) -> a
modifyGlobalMessages f k = k (unsafePerformEffect $ Ref.modify_ (List.take maximumGlobalMessages <<< f) terminalItemsRef)

addGlobalMessage :: forall a. GlobalMessageTag -> Html -> (Unit -> a) -> a
addGlobalMessage tag html k = modifyGlobalMessages (Cons (make tag html)) k

make :: GlobalMessageTag -> Html -> GlobalMessage
make tag html = GlobalMessage {tag, html}

-- record

renderRecord :: forall r. Homogeneous r Html => Record r -> Html
renderRecord r =
  let keysAndValues = fromHomogenousRecordToTupleArray r in
  El.matrix $ keysAndValues <#> \(k /\ v) -> [El.ℓ [El.Classes [El.GlobalMessageRecordKey]] [El.τ k], v]

-- error

error :: forall a. Html -> a
error html = addGlobalMessage ErrorGlobalMessageTag html \_ ->
  unsafeCrashWith "global message board error"

errorR :: forall r a. Homogeneous r Html => Html -> Record r -> a
errorR title r = 
  addGlobalMessage ErrorGlobalMessageTag (El.β [title, El.br, renderRecord r]) \_ ->
    unsafeCrashWith "global message board error"

-- log

log :: forall a. GlobalMessageTag -> Html -> (Unit -> a) -> a
log = addGlobalMessage

logM tag html = do
  pure unit
  log tag html \_ -> pure unit

logR tag title r =
  log tag $ El.β [title, El.br, renderRecord r]
      

logRM tag title r = do
  pure unit
  logR tag title r \_ -> pure unit

-- debug

debug = log DebugGlobalMessageTag
debugM = logM DebugGlobalMessageTag
debugR = logR DebugGlobalMessageTag
debugRM = logRM DebugGlobalMessageTag

-- info

info = log InfoGlobalMessageTag
infoM = logM InfoGlobalMessageTag
infoR = logR InfoGlobalMessageTag
infoRM = logRM InfoGlobalMessageTag
