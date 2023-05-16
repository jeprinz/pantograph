module Hole where

import Prim.TypeError (class Warn, Text)

class HoleWarning

instance warnHoleWarning :: Warn (Text "Contains holes") => HoleWarning

foreign import _hole :: forall a b. a -> b

hole :: forall a b. HoleWarning => a -> b
hole a = _hole a
