module Hole where

import Prim.TypeError (class Warn, Text)
import Data.Unit (Unit)
import Data.Either (Either)

class HoleWarning

instance warnHoleWarning :: Warn (Text "Contains holes") => HoleWarning

foreign import _hole :: forall a b. a -> b

foreign import realCatchException :: forall a. (forall x y. x -> Either x y) -> (forall x y. x -> Either y x)
    -> (Unit -> a) -> Either String a

--realCatchException :: forall a. (Unit -> a) -> Either String a
--realCatchException = _realCatchException

hole :: forall a b. HoleWarning => a -> b
hole a = _hole a
