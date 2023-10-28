module Pantograph.Library.Language.Edit where

import Pantograph.Generic.Language
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tree (fromPath, singletonNonEmptyPath, tooths)
import Data.Tuple (fst)
import Util (fromJust)

type SplitChange sn = SortChange sn -> {outerChange :: SortChange sn, innerChange :: SortChange sn}

buildEditFromExprNonEmptyPath :: forall sn el.
  Language sn el =>
  {splitExprPathChanges :: SplitChange sn} ->
  ExprNonEmptyPath sn el ->
  Edit sn el
buildEditFromExprNonEmptyPath {splitExprPathChanges} middle =
  Edit
    { outerChange: Just outerChange
    , middle: Just middle
    , innerChange: Just innerChange
    , inside: Nothing }
  where
  ch = getExprNonEmptyPathSortChange middle
  {outerChange, innerChange} = splitExprPathChanges ch

buildEditsFromExpr :: forall sn el.
  Language sn el =>
  {splitExprPathChanges :: SplitChange sn} ->
  Expr sn el ->
  NonEmptyArray (Edit sn el)
buildEditsFromExpr {splitExprPathChanges} expr =
  let middles = singletonNonEmptyPath <<< fst <$> tooths expr in
  fromJust $ NonEmptyArray.fromArray $ middles <#> \middle -> 
    buildEditFromExprNonEmptyPath {splitExprPathChanges} middle
