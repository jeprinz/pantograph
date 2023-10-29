module Pantograph.Library.Language.Edit where

import Pantograph.Generic.Language
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
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

buildExprToothEditsFromExpr :: forall sn el.
  Language sn el =>
  {splitExprPathChanges :: SplitChange sn} ->
  Expr sn el ->
  NonEmptyArray (Edit sn el)
buildExprToothEditsFromExpr {splitExprPathChanges} expr =
  let middles = singletonNonEmptyPath <<< fst <$> tooths expr in
  fromJust $ NonEmptyArray.fromArray $ middles <#> \middle -> 
    buildEditFromExprNonEmptyPath {splitExprPathChanges} middle

identitySpecialEdits :: forall sn el. SpecialEdits sn el
identitySpecialEdits = 
  { deleteExpr: const Nothing
  , copyExpr: const Nothing
  , deletePath: const Nothing
  , copyPath: const Nothing
  , enter: const Nothing
  , tab: const Nothing
  }

identityEdit :: forall sn el. Edit sn el
identityEdit = Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Nothing}

makeOuterChangeEdit :: forall sn el. SortChange sn -> Edit sn el
makeOuterChangeEdit ch = identityEdit # Newtype.over Edit _ {outerChange = Just ch}

makeInnerChangeEdit :: forall sn el. SortChange sn -> Edit sn el
makeInnerChangeEdit ch = identityEdit # Newtype.over Edit _ {innerChange = Just ch}

makeInsideChangeEdit :: forall sn el. Expr sn el -> Edit sn el
makeInsideChangeEdit e = identityEdit # Newtype.over Edit _ {inside = Just e}
