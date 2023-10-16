module Pantograph.Generic.Language.Edit where

import Data.Tree
import Data.Tree.Move
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Language
import Prelude

import Data.Maybe (Maybe(..))
import Hole (hole)

applyEdit :: forall sn el. PrettyTreeNode el => Language sn el -> ExprEdit sn el -> ExprGyro sn el -> Maybe (ExprGyro sn el)
applyEdit language edit gyro@(RootGyro _) = applyEdit language edit =<< ensureGyroIsCursor gyro

applyEdit (Language _language) (InsertEdit edit) (CursorGyro (Cursor {outside, inside, orientation})) =
  -- TODO: do changes
  Just $ CursorGyro $ Cursor {outside: outside <> toPath edit.middle, inside, orientation}
applyEdit (Language _language) (InsertEdit edit) (SelectGyro (Select {outside, middle: _, inside, orientation})) = 
  -- TODO: do changes
  Just $ SelectGyro $ Select {outside, middle: edit.middle, inside, orientation}
applyEdit (Language _language) (ReplaceEdit edit) (CursorGyro (Cursor {outside, inside: _, orientation})) = 
  -- TODO: do changes
  Just $ CursorGyro $ Cursor {outside, inside: edit.inside, orientation}
applyEdit language edit@(ReplaceEdit _) gyro@(SelectGyro _) = applyEdit language edit =<< ensureGyroIsCursor gyro

