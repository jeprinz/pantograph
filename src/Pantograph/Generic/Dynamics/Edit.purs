module Pantograph.Generic.Dynamics.Edit where

import Data.Tree
import Pantograph.Generic.Dynamics.Common
import Pantograph.Generic.Language
import Prelude

import Data.Maybe (Maybe, maybe, maybe')
import Pantograph.Generic.Dynamics.Run (runStepExpr)
import Text.Pretty (pretty)
import Util (debug)

applyEdit :: forall sn el ctx env. Dynamics sn el ctx env => Edit sn el -> ExprGyro sn el -> Maybe (ExprGyro sn el)
applyEdit edit gyro@(RootGyro _) = applyEdit edit =<< ensureGyroIsCursor gyro
applyEdit edit (SelectGyro (Select {outside, middle: _, inside})) = runStepExpr $ setupEdit (Cursor {outside, inside, orientation: Outside}) edit
applyEdit edit (CursorGyro cursor) = runStepExpr $ setupEdit cursor edit

setupEdit :: forall sn el. Language sn el => ExprCursor sn el -> Edit sn el -> StepExpr sn el
setupEdit (Cursor cursor) (Edit edit) =
  wrapExprPath (maybe identity applySortVarSubst edit.sigma cursor.outside) $
  edit.outerChange # maybe identity (boundary Up) $
  edit.middle # maybe identity (wrapExprPath <<< toPath)  $
  edit.innerChange # maybe identity (boundary Down)  $
  marker $
  toStepExpr (edit.inside # maybe' (\_ -> maybe identity applySortVarSubst edit.sigma cursor.inside) identity)
