module Pantograph.Generic.Dynamics.Edit where

import Pantograph.Generic.Dynamics.Common
import Pantograph.Generic.Language
import Prelude
import Data.Tree

import Data.Maybe (Maybe)
import Pantograph.Generic.Rendering (class Rendering)

applyEdit :: forall sn el ctx env. Dynamics sn el ctx env => Edit sn el -> ExprGyro sn el -> Maybe (ExprGyro sn el)
applyEdit edit gyro@(RootGyro _) = applyEdit edit =<< ensureGyroIsCursor gyro
applyEdit edit (SelectGyro (Select {outside, middle: _, inside})) = runStepExpr $ setupEdit (Cursor {outside, inside, orientation: Outside}) edit
applyEdit edit (CursorGyro cursor) = runStepExpr $ setupEdit cursor edit
