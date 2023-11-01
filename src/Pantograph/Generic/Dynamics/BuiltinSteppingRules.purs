module Pantograph.Generic.Dynamics.BuiltinSteppingRules (builtinSteppingRules) where

import Data.Tree
import Pantograph.Generic.Dynamics.Common
import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))

builtinSteppingRules :: forall sn el. Eq sn => Show sn => PrettyTreeNode sn => Array (SteppingRule sn el)
builtinSteppingRules =
  [ passThroughRule
  , combineUpRule
  , combineDownRule
  , idBoundaryRule
  , threadUpBoundaryThroughMarker
  , threadDownBoundaryThroughMarker
  ]

passThroughRule :: forall el sn. Eq sn => Show sn => PrettyTreeNode sn => SteppingRule sn el
passThroughRule = SteppingRule "passThroughRule" case _ of
  Boundary (Down /\ down) (Boundary (Up /\ up) kid) -> Just
    let hypotenuse = lub' down up in
    let up' = invert down <> hypotenuse in
    let down' = invert up <> hypotenuse in
    Boundary (Up /\ up') (Boundary (Down /\ down') kid)
  _ -> Nothing

threadUpBoundaryThroughMarker :: forall el sn. SteppingRule sn el
threadUpBoundaryThroughMarker = SteppingRule "threadUpBoundaryThroughMarker" case _ of
  Marker (Boundary (Up /\ ch) e) -> Just $ Boundary (Up /\ ch) (Marker e)
  _ -> Nothing

threadDownBoundaryThroughMarker :: forall el sn. SteppingRule sn el
threadDownBoundaryThroughMarker = SteppingRule "threadDownBoundaryThroughMarker" case _ of
  Boundary (Down /\ ch) (Marker e) -> Just $ Marker (Boundary (Down /\ ch) e)
  _ -> Nothing

combineDownRule :: forall sn el. Eq sn => SteppingRule sn el
combineDownRule = SteppingRule "combineDownRule" case _ of
  Boundary (Down /\ down1) (Boundary (Down /\ down2) kid) -> Just $
    Boundary (Down /\ (down1 <> down2)) kid
  _ -> Nothing

combineUpRule :: forall sn el. Eq sn => SteppingRule sn el
combineUpRule = SteppingRule "combineUpRule" case _ of
  Boundary (Up /\ up1) (Boundary (Up /\ up2) kid) -> Just $
    Boundary (Up /\ (up1 <> up2)) kid
  _ -> Nothing

idBoundaryRule :: forall sn el. Eq sn => SteppingRule sn el
idBoundaryRule = SteppingRule "idBoundaryRule" case _ of
  Boundary (_ /\ ch) e | isIdentity ch -> Just e
  _ -> Nothing
