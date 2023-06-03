module Log where

import Prelude

_logging :: Boolean
_logging = false

foreign import _log :: forall x a. String -> x -> (Unit -> a) -> a

log :: forall x a. String -> x -> (Unit -> a) -> a
log | _logging = _log
log = \_ _ k -> k unit

logM :: forall m x. Monad m => String -> x -> m Unit
logM | _logging = \tag x -> do
  pure unit
  log tag x \_ -> pure unit
logM = const <<< const $ pure unit
