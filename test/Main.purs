module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.UnifyChange as UnifyChange

main :: Effect Unit
main = do
  UnifyChange.main
