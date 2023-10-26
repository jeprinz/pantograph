module Pantograph.Library.Language.Edit where

import Pantograph.Generic.Language
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tree (fromPath, singletonNonEmptyPath, tooths)
import Data.Tuple (fst)
import Util (fromJust)

type SplitExprPathChanges sn el = ExprNonEmptyPath sn el -> Sort sn -> {outerChange :: SortChange sn, innerChange :: SortChange sn}

buildEditFromExprNonEmptyPath :: forall sn el.
  { splitExprPathChanges :: SplitExprPathChanges sn el } ->
  ExprNonEmptyPath sn el ->
  Sort sn ->
  Edit sn el
buildEditFromExprNonEmptyPath {splitExprPathChanges} middle sort =
  Edit
    { outerChange: Just outerChange
    , middle: Just middle
    , innerChange: Just innerChange
    , inside: Nothing }
  where
  {outerChange, innerChange} = splitExprPathChanges middle sort

buildEditsFromExpr :: forall sn el.
  { splitExprPathChanges :: SplitExprPathChanges sn el } ->
  Expr sn el ->
  Sort sn ->
  NonEmptyArray (Edit sn el)
buildEditsFromExpr {splitExprPathChanges} expr sort =
  let middles = singletonNonEmptyPath <<< fst <$> tooths expr in
  fromJust $ NonEmptyArray.fromArray $ middles <#> \middle -> 
    buildEditFromExprNonEmptyPath {splitExprPathChanges} middle sort
