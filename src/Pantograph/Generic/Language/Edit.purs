module Pantograph.Generic.Language.Edit where

import Data.Tree
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Step
import Prelude

import Bug (bug)
import Data.Maybe (Maybe)

applyEdit :: forall sn el. Language sn el =>
  ExprEdit sn el -> 
  ExprGyro sn el -> 
  Maybe (ExprGyro sn el)

-- Root must be turned into a Cursor before applying the Edit
applyEdit 
  edit
  gyro@(RootGyro _) = 
  applyEdit edit =<< ensureGyroIsCursor gyro

-- InsertEdit

applyEdit 
  (InsertEdit {outerChange, middle, innerChange}) 
  (CursorGyro (Cursor {outside, inside, orientation})) = 
  runStepExpr $ setupInsert {outside, outerChange, middle, innerChange, inside, orientation}

applyEdit
  (InsertEdit _)
  (SelectGyro _) = 
  bug $ "should this be allowed? or should you have to delete first, before inserting"

-- ReplaceEdit

applyEdit 
  (ReplaceEdit {outerChange, inside}) 
  (CursorGyro (Cursor {outside, inside: _})) = 
  runStepExpr $ setupReplace {outside, outerChange, inside}

applyEdit 
  edit@(ReplaceEdit _)
  gyro@(SelectGyro _) = 
  applyEdit edit =<< ensureGyroIsCursor gyro

