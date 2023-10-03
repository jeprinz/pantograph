module Pantograph.Generic.Language.Common where

import Prelude

import Data.List (List)
import Data.Map as Map
import Data.Set as Set

-- Sort

data Sort n d = Sort (SortNode n d) (Array (Sort n d))
data SortNode n d = SortNode n {|d}
data SortTooth n d = SortTooth (SortNode n d) Int (Array (Sort n d))

-- RuleSort

data RuleSort n d = RuleSort (SortNode n d) | VarRuleSort RuleVar

data RuleVar = MakeRuleVar String
newtype RuleVarSubst a = RuleVarSubst (Map.Map RuleVar a)

-- Change

data Change n d
  = Plus (SortTooth n d) (Change n d)
  | Minus (SortTooth n d) (Change n d)
  | Replace (Sort n d) (Sort n d)
  | Reflect (SortNode n d) (Array (Change n d))

-- Expr

data Expr r n d s = Expr (ExprNode r n d s) (Array (Expr r n d s))
data ExprNode r n d s = ExprNode r n {|d} (RuleVarSubst s)
data ExprTooth r n d s = ExprTooth (ExprNode r n d s) Int (Array (Expr r n d s))
type ExprPath r n d s = List (ExprTooth r n d s)

-- CursorData, SelectData

type CursorNode n = (cursor :: CursorData | n)
data CursorData
  = OutsideCursorNode
  | AtCursorNode
  | InsideCursorNode

type SelectNode n = (select :: SelectData | n)
data SelectData
  = OutsideSelectNode
  | AtOuterSelectNode
  | InSelectNode
  | AtInnerSelectNode
  | InsideSelectNode

-- Language

newtype Language r n d = Language
  { getSortingRule :: r -> SortingRule n d
  , getChangingRule :: r -> ChangingRule n d }

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule n d = SortingRule
  { parameters :: Set.Set RuleVar
  , kids :: Array (RuleSort n d)
  , parent :: RuleSort n d }

-- | A `ChangeRule` specifies the changes from the prent to eahc kid of a
-- | corresponding `SortingRule`.
newtype ChangingRule n d = ChangingRule 
  { parameters :: Set.Set RuleVar
  , kids :: Array (Change n d) }
