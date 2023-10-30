module Pantograph.Generic.Language.Edit where

import Data.Tree
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Step
import Prelude

import Data.Maybe (Maybe)
import Pantograph.Generic.Rendering.Common (class Rendering)

applyEdit :: forall sn el ctx env. Rendering sn el ctx env => Edit sn el -> ExprGyro sn el -> Maybe (ExprGyro sn el)
applyEdit edit gyro@(RootGyro _) = applyEdit edit =<< ensureGyroIsCursor gyro
applyEdit edit (SelectGyro (Select {outside, middle: _, inside})) = runStepExpr $ setupEdit (Cursor {outside, inside, orientation: Outside}) edit
applyEdit edit (CursorGyro cursor) = runStepExpr $ setupEdit cursor edit
