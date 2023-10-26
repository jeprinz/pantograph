module Pantograph.Library.Language.Step where

import Pantograph.Generic.Language
import Prelude
import Data.Tuple.Nested

import Data.Maybe (Maybe(..), fromMaybe, maybe)

matchDirection :: Maybe Direction -> Direction -> Boolean
matchDirection mbDir dir = maybe true (_ == dir) mbDir

-- | Erase the boundary if the boundary's change satisfies a condition.
eraseBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Boolean) -> SteppingRule sn el
eraseBoundary mbDir cond = SteppingRule case _ of
  Boundary (dir /\ ch) kid 
    | matchDirection mbDir dir
    , cond ch 
    -> Just kid
  _ -> Nothing

-- | Interpret a boundary with a change as a transformation over the inner expression.
dischargeBoundary :: forall sn el. Maybe Direction -> (SortChange sn -> Maybe (StepExpr sn el -> Maybe (StepExpr sn el))) -> SteppingRule sn el
dischargeBoundary mbDir f = SteppingRule case _ of
  Boundary (dir /\ ch) kid
    | matchDirection mbDir dir
    , Just f' <- f ch
    -> f' kid
  _ -> Nothing
