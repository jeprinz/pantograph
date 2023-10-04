module Pantograph.Generic.Language.Common where

import Prelude

import Bug (bug)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Record as R
import Type.Proxy (Proxy(..))

-- Sort

data Sort n d = Sort {node :: SortNode n d, kids :: Array (Sort n d)}
data SortNode n d = SortNode {node :: n, dat :: {|d}}
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

data Expr r n d s = Expr {node :: ExprNode r n d s, kids :: Array (Expr r n d s)}
data ExprNode r n d s = ExprNode {rule :: r, node :: n, sigma :: RuleVarSubst s, dat :: {|d}}
data ExprTooth r n d s = ExprTooth (ExprNode r n d s) Int (Array (Expr r n d s))
type ExprPath r n d s = List (ExprTooth r n d s)

mapExprNode_data :: forall r n d d' s. (Record d -> Record d') -> ExprNode r n d s -> ExprNode r n d' s
mapExprNode_data f (ExprNode node) = ExprNode node {dat = f node.dat}

-- CursorData, SelectData

type CursorData n = (cursor :: Maybe CursorStatus | n)
data CursorStatus = CursorStatus

unCursorData = R.delete (Proxy :: Proxy "cursor")

type SelectData n = (select :: Maybe SelectStatus | n)
data SelectStatus = OuterSelectStatus | InnerSelectStatus

unSelectData = R.delete (Proxy :: Proxy "select")

-- Language

newtype Language r n d = Language
  { name :: String
  , getSortingRule :: r -> SortingRule n d
  , getChangingRule :: r -> ChangingRule n d
  , topSort :: Sort n d
  , defaultExpr :: Sort n d -> Maybe (Expr r n d (Sort n d)) }

derive instance Newtype (Language r n d) _

topExpr (Language language) = case language.defaultExpr language.topSort of
  Nothing -> bug $ "language.defaultExpr language.topSort == Nothing"
  Just expr -> expr

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule n d = SortingRule
  { parameters :: Set.Set RuleVar
  , kids :: Array (RuleSort n d)
  , parent :: RuleSort n d }

derive instance Newtype (SortingRule n d) _

-- | A `ChangeRule` specifies the changes from the prent to eahc kid of a
-- | corresponding `SortingRule`.
newtype ChangingRule n d = ChangingRule 
  { parameters :: Set.Set RuleVar
  , kids :: Array (Change n d) }

derive instance Newtype (ChangingRule n d) _
