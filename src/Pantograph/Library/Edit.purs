module Pantograph.Library.Edit where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Tree (epL, epR, invert)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Pantograph.Generic.Language (class Language, Edit(..), Expr, Sort, SortChange, SpecialEdits, ExprNonEmptyPath, applySortVarSubst, getExprNonEmptyPathSortChange, unifySort3)

-- | ## Edit Makers

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
makeEditFromExprNonEmptyPath :: forall sn el.
  Language sn el =>
  {splitExprPathChanges :: SplitChange sn} ->
  Sort sn ->
  ExprNonEmptyPath sn el ->
  Maybe (Edit sn el)
makeEditFromExprNonEmptyPath {splitExprPathChanges} sort middle = do
  let middleChange = getExprNonEmptyPathSortChange middle
  let {outerChange: outerChange_, innerChange} = splitExprPathChanges middleChange
  let outerChange = invert outerChange_

  -- GMB.debugRM (El.τ "makeEditFromExprNonEmptyPath [1]")
  --   { sort: display sort
  --   , middle: El.τ $ pretty middle
  --   , middleChange: display middleChange
  --   , outerChange: display outerChange 
  --   , innerChange: display innerChange 
  --   , "epL innerChange": display $ epL innerChange
  --   , "epR innerChange": display $ epR innerChange
  --   , "epL outerChange": display $ epL outerChange
  --   , "epR outerChange": display $ epR outerChange
  --   }
  
  _ /\ sigma <- unifySort3 sort (epL innerChange) (epR outerChange)

  let outerChange' = applySortVarSubst sigma outerChange
  let middle' = applySortVarSubst sigma middle
  let innerChange' = applySortVarSubst sigma innerChange

  -- GMB.debugRM (El.τ "makeEditFromExprNonEmptyPath [2]")
  --   { sort: display sort
  --   , middle: El.τ $ pretty middle
  --   , middleChange: display middleChange
  --   , outerChange: display outerChange 
  --   , innerChange: display innerChange 
  --   , sigma: El.τ $ pretty sigma
  --   , outerChange': display outerChange'
  --   , innerChange': display innerChange'
  --   }

  Just $ Edit
    { sigma: Just sigma
    , outerChange: Just outerChange'
    , middle: Just middle'
    , innerChange: Just innerChange'
    , inside: Nothing }

-- | ## Simple Edits

identityEdit :: forall sn el. Edit sn el
identityEdit = Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Nothing, sigma: Nothing}

buildEdit :: forall sn el. Newtype (Edit sn el) _ => _ -> Edit sn el
buildEdit f = identityEdit # Newtype.over Edit f

identitySpecialEdits :: forall sn el. SpecialEdits sn el
identitySpecialEdits = 
  { copyExpr: const Nothing
  , copyExprPath: const Nothing
  , cut: const Nothing
  , paste: const (const Nothing)
  }

-- | ## Utilities

makeEditRow :: forall sn el. String -> Array (Maybe (Edit sn el)) -> Maybe (String /\ NonEmptyArray (Edit sn el))
makeEditRow key edits = map (key /\ _) $ join $ map NonEmptyArray.fromArray $ Array.fold $ map (map Array.singleton) $ edits

makeEditRows :: forall sn el. Array (String /\ Array (Maybe (Edit sn el))) -> Array (String /\ NonEmptyArray (Edit sn el))
makeEditRows = map (uncurry makeEditRow) >>> Array.foldMap Array.fromFoldable
