module Pantograph.Generic.Language.Edit where

import Data.Tree
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Step
import Prelude

import Bug (bug)
import Data.Maybe (Maybe)

applyEdit :: forall sn el. PrettyTreeNode el => Eq sn => Eq el => Show sn => PrettyTreeNode sn =>
  Language sn el -> 
  ExprEdit sn el -> 
  ExprGyro sn el -> 
  Maybe (ExprGyro sn el)

-- Root must be turned into a Cursor before applying the Edit
applyEdit language 
  edit
  gyro@(RootGyro _) = 
  applyEdit language edit =<< ensureGyroIsCursor gyro

-- InsertEdit

applyEdit language 
  (InsertEdit {outerChange, middle, innerChange}) 
  (CursorGyro (Cursor {outside, inside, orientation})) = 
  runStepExpr language $ setupInsert {outside, outerChange, middle, innerChange, inside, orientation}

applyEdit _ 
  (InsertEdit _)
  (SelectGyro _) = 
  bug $ "should this be allowed? or should you have to delete first, before inserting"

-- ReplaceEdit

applyEdit language 
  (ReplaceEdit {outerChange, inside}) 
  (CursorGyro (Cursor {outside, inside: _})) = 
  runStepExpr language $ setupReplace {outside, outerChange, inside}

applyEdit language 
  edit@(ReplaceEdit _)
  gyro@(SelectGyro _) = 
  applyEdit language edit =<< ensureGyroIsCursor gyro

