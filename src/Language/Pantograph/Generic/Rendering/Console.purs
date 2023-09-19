module Language.Pantograph.Generic.Rendering.Console where

import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as HK
import Halogen.Utilities (classNames)
import Type.Proxy (Proxy(..))

_consoleSlot = Proxy :: Proxy "console"

type Console = Array HH.PlainHTML

consoleRef :: Ref Console
consoleRef = unsafePerformEffect $ Ref.new []

getConsole :: forall a. (Console -> a) -> a
getConsole k = unsafePerformEffect do
  k <$> Ref.read consoleRef

setConsole :: forall a. (Console -> Console) -> (Console -> a) -> a
setConsole f k = unsafePerformEffect do
  Ref.modify_ f consoleRef
  k <$> Ref.read consoleRef

logConsole :: forall a. HH.PlainHTML -> (Unit -> a) -> a
logConsole log k = setConsole (Array.cons log) \_ -> k unit

consoleComponent :: forall q i o. H.Component q i o Aff
consoleComponent = HK.component \tokens spec -> HK.do

  bit /\ bitId <- HK.useState false

  HK.pure $
    HH.div [classNames ["console"]] $
      [HH.div [classNames ["console-header"]]
          [HH.text "Console"]
       
      , HH.div [classNames ["console-controls"]] 
          [ HH.button 
              [HE.onClick \event ->
                logConsole (HH.text ("[force console update]")) \_ ->
                  HK.modify_ bitId not]
              [HH.text "force update"]
          , HH.button 
              [HE.onClick \event ->
                logConsole (HH.text ("[clear console]")) \_ ->
                  setConsole (\_ -> []) (\_ -> HK.modify_ bitId not)]
              [HH.text "clear"] 
          ]
      , HH.div [classNames ["console-logs"]] $
          getConsole \logs -> Array.reverse logs <#> \log -> 
          HH.div [classNames ["console-log"]]
            [HH.fromPlainHTML log]
      ]
