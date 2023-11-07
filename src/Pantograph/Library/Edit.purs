module Pantograph.Library.Edit where

import Pantograph.Generic.Language
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Tree (epL, epR, fromPath, singletonNonEmptyPath, tooths)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Pantograph.Generic.Language.Unification (unifySort, unifySort3)
import Text.Pretty (pretty)
import Util (debugM, fromJust)

type SplitChange sn = SortChange sn -> {outerChange :: SortChange sn, innerChange :: SortChange sn}

-- | ```
-- | p : ExprNonEmptyPath a b
-- | a : Sort
-- | m[•] : ExprNonEmptyPath c e
-- | (δ₁ : Change c e) /\ (δ₂ : Change e d) = splitExprPathChanges m[•]
-- | σ = a ~ c ~ d
-- | 
-- | p[e : Expr a] ~~~> σp[ {σδ₂}↑{ σm[ {σδ₁}↓{σe} ] } ]
-- | ```
buildEditFromExprNonEmptyPath :: forall sn el.
  Language sn el =>
  {splitExprPathChanges :: SplitChange sn} ->
  Sort sn ->
  ExprNonEmptyPath sn el ->
  Maybe (Edit sn el)
buildEditFromExprNonEmptyPath {splitExprPathChanges} sort middle = do
  let ch = getExprNonEmptyPathSortChange middle
  let {outerChange, innerChange} = splitExprPathChanges ch
  _ /\ sigma <- unifySort3 sort (epL innerChange) (epR outerChange)
  let outerChange' = applySortVarSubst sigma outerChange
  let middle' = applySortVarSubst sigma middle
  let innerChange' = applySortVarSubst sigma innerChange
  Just $ Edit
    { outerChange: Just outerChange'
    , middle: Just middle'
    , innerChange: Just innerChange'
    , inside: Nothing
    , sigma: Just sigma }

identitySpecialEdits :: forall sn el. SpecialEdits sn el
identitySpecialEdits = 
  { deleteExpr: const Nothing
  , copyExpr: const Nothing
  , deleteExprPath: const Nothing
  , copyExprPath: const Nothing
  , enter: const Nothing
  , tab: const Nothing
  }

identityEdit :: forall sn el. Edit sn el
identityEdit = Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Nothing, sigma: Nothing}

makeOuterChangeEdit :: forall sn el. SortChange sn -> Edit sn el
makeOuterChangeEdit ch = identityEdit # Newtype.over Edit _ {outerChange = Just ch}

makeInnerChangeEdit :: forall sn el. SortChange sn -> Edit sn el
makeInnerChangeEdit ch = identityEdit # Newtype.over Edit _ {innerChange = Just ch}

makeInsideChangeEdit :: forall sn el. Expr sn el -> Edit sn el
makeInsideChangeEdit e = identityEdit # Newtype.over Edit _ {inside = Just e}
