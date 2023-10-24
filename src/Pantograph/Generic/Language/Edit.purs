module Pantograph.Generic.Language.Edit where

import Data.Tree
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Step
import Prelude

import Data.Maybe (Maybe)

applyEdit :: forall sn el. Language sn el => Edit sn el -> ExprGyro sn el -> Maybe (ExprGyro sn el)
applyEdit edit gyro@(RootGyro _) = applyEdit edit =<< ensureGyroIsCursor gyro
applyEdit edit (SelectGyro (Select {outside, middle: _, inside})) = runStepExpr $ setupEdit (Cursor {outside, inside, orientation: Outside}) edit
applyEdit edit (CursorGyro cursor) = runStepExpr $ setupEdit cursor edit
