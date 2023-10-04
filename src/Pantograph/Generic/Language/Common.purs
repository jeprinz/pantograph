module Pantograph.Generic.Language.Common where

import Prelude

import Bug (bug)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Text.Pretty (class Pretty, parens, pretty, spaces)
import Type.Proxy (Proxy(..))

-- Sort

data Sort n d = Sort {node :: SortNode n d, kids :: Array (Sort n d)}
data SortNode n d = SortNode {n :: n, d :: Record d}
data SortTooth n d = SortTooth (SortNode n d) Int (Array (Sort n d))

instance Pretty n => Pretty (Sort n d) where
  pretty (Sort {node: SortNode {n}, kids}) = parens $ spaces $ [pretty n] <> (pretty <$> kids)

makeSort n d kids = Sort {node: SortNode {n, d}, kids}

-- RuleSort

data RuleSort n d 
  = RuleSort {node :: SortNode n d, kids :: Array (RuleSort n d)}
  | VarRuleSort RuleVar

data RuleSortTooth n d = RuleSortTooth (SortNode n d) Int (Array (RuleSort n d))

makeRuleSort n d kids = RuleSort {node: SortNode {n, d}, kids}
makeVarRuleSort = VarRuleSort

data RuleVar = MakeRuleVar String

derive instance Eq RuleVar
derive instance Ord RuleVar

newtype RuleVarSubst a = RuleVarSubst (Map.Map RuleVar a)

substRuleVar :: forall a. RuleVarSubst a -> RuleVar -> a
substRuleVar (RuleVarSubst m) x = case Map.lookup x m of
  Nothing -> bug $ "a RuleVar was not substituted by the RuleVarSubst"
  Just a -> a

substRuleSort :: forall n d. RuleVarSubst (Sort n d) -> RuleSort n d -> Sort n d
substRuleSort sigma (RuleSort {node, kids}) = Sort {node, kids: substRuleSort sigma <$> kids}
substRuleSort sigma (VarRuleSort x) = substRuleVar sigma x

substRuleChange :: forall n d. RuleVarSubst (Sort n d) -> RuleChange n d -> Change n d
substRuleChange = hole "TODO"


-- Change

data Change n d
  = Plus (SortTooth n d) (Change n d)
  | Minus (SortTooth n d) (Change n d)
  | Replace (Sort n d) (Sort n d)
  | Reflect (SortNode n d) (Array (Change n d))

-- RuleChange

data RuleChange n d
  = RulePlus (RuleSortTooth n d) (RuleChange n d)
  | RuleMinus (RuleSortTooth n d) (Change n d)
  | RuleReplace (RuleSort n d) (RuleSort n d)
  | RuleReflect (SortNode n d) (Array (RuleChange n d))

makeRuleReflect n d kids = RuleReflect (SortNode {n, d}) kids

-- Expr

data Expr r n d s = Expr {node :: ExprNode r n d s, kids :: Array (Expr r n d s)}
data ExprNode r n d s = ExprNode {r :: r, n :: n, sigma :: RuleVarSubst s, d :: Record d}
data ExprTooth r n d s = ExprTooth (ExprNode r n d s) Int (Array (Expr r n d s))
type ExprPath r n d s = List (ExprTooth r n d s)

makeExpr r n sigma d kids = Expr {node: ExprNode {r, n, sigma, d}, kids}

mapExprNode_data :: forall r n d d' s. (Record d -> Record d') -> ExprNode r n d s -> ExprNode r n d' s
mapExprNode_data f (ExprNode node) = ExprNode node {d = f node.d}

-- CursorData, SelectData

-- TODO: could include more info here, such as OutSideCursorStatus,
-- InsideCursorState, if its efficient enough to directly manipulate the dom
-- that much.

type CursorData d = (cursor :: Maybe CursorStatus | d)
data CursorStatus = CursorStatus

unCursorData = R.delete (Proxy :: Proxy "cursor")
enCursorData :: forall d. Lacks "cursor" d => Record (CursorData ()) -> Record d -> Record (CursorData d)
enCursorData {cursor} = 
  R.insert (Proxy :: Proxy "cursor") cursor

type SelectData d = (select :: Maybe SelectStatus | d)
data SelectStatus = OuterSelectStatus | InnerSelectStatus

unSelectData = R.delete (Proxy :: Proxy "select")
enSelectData :: forall d. Lacks "select" d => Record (SelectData ()) -> Record d -> Record (SelectData d)
enSelectData {select} = 
  R.insert (Proxy :: Proxy "select") select

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

getExprNodeSort (Language language) (ExprNode {n, r, sigma, d}) = do
  let SortingRule sortingRule = language.getSortingRule r
  substRuleSort sigma sortingRule.parent

getExprSort language (Expr {node}) = getExprNodeSort language node

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
  , kids :: Array (RuleChange n d) }

derive instance Newtype (ChangingRule n d) _
