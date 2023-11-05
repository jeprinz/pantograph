module Pantograph.Generic.Rendering.Hook.Timer where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Hooks as HK
import Halogen.Subscription as HS

useTimer :: forall m. MonadAff m => Milliseconds -> HK.HookM m Unit -> HK.Hook m HK.UseEffect Unit
useTimer ms m = HK.useLifecycleEffect do
  -- initialize
  subId <- HK.subscribe =<< timer ms m
  pure $ Just do
    -- finalize
    HK.unsubscribe subId

timer :: forall m a. MonadAff m => Milliseconds -> a -> m (HS.Emitter a)
timer ms m = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay ms
    H.liftEffect $ HS.notify listener m
  pure emitter
