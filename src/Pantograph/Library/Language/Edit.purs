module Pantograph.Library.Language.Edit where

import Pantograph.Generic.Language
import Data.Maybe (Maybe(..))

editFromExprNonEmptyPath :: forall sn el.
  { splitExprPathChanges :: ExprNonEmptyPath sn el -> Sort sn -> {outerChange :: SortChange sn, innerChange :: SortChange sn} } ->
  ExprNonEmptyPath sn el ->
  Sort sn ->
  Edit sn el
editFromExprNonEmptyPath {splitExprPathChanges} middle sort =
  Edit
    { outerChange: Just outerChange
    , middle: Just middle
    , innerChange: Just innerChange
    , inside: Nothing }
  where
  {outerChange, innerChange} = splitExprPathChanges middle sort
