module Pantograph.Generic.Language.Common where

import Prelude
import Util

import Bug (bug)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Hole (hole)
import Prim.Row (class Lacks)
import Record as R
import Text.Pretty (class Pretty, parens, pretty, spaces, ticks)
import Type.Proxy (Proxy(..))
import Data.Tree

makeConstRuleSort n kids = Tree {node: ConstRuleSortNode (SortNode n), kids}
makeVarRuleSort x = Tree {node: VarRuleSortNode x, kids: []}
makeSort sn kids = Tree {node: SortNode sn, kids}
makeExpr label sigma dat kids = Tree {node: ExprNode {label, sigma}, kids}

-- Sort

data SortNode (sn :: Type)
  = SortNode sn

derive instance Generic (SortNode sn) _
derive instance Eq sn => Eq (SortNode sn)
instance Show sn => Show (SortNode sn) where show = genericShow
derive instance Functor SortNode

type Sort sn = Tree (SortNode sn)
type SortTooth sn = Tooth (SortNode sn)
type SortPath sn = Path (SortNode sn)

-- RuleSort

data RuleSortNode sn
  = ConstRuleSortNode (SortNode sn)
  | VarRuleSortNode RuleSortVar

derive instance Generic (RuleSortNode sn) _
instance Show sn => Show (RuleSortNode sn) where show = genericShow

fromConstRuleSortNode _ (ConstRuleSortNode sn) = sn
fromConstRuleSortNode msg n = bug $ msg <> "expected " <> ticks "ConstRuleSortNode _" <> " but found " <> ticks (show n)

type RuleSort sn = Tree (RuleSortNode sn)
type RuleSortTooth sn = Tooth (RuleSortNode sn)
type RuleSortPath sn = Path (RuleSortNode sn)

instance ApplyRuleSortVarSubst sn (RuleSort sn) (Sort sn) where
  applyRuleSortVarSubst sigma (Tree {node: ConstRuleSortNode sn, kids}) = Tree {node: sn, kids: applyRuleSortVarSubst sigma <$> kids}
  applyRuleSortVarSubst sigma (Tree {node: VarRuleSortNode x}) = applyRuleSortVarSubst sigma x

-- SortChange

type SortChange sn = Change (SortNode sn)

-- RuleSortChange

type RuleSortChange sn = Change (RuleSortNode sn)

instance Show sn => ApplyRuleSortVarSubst sn (RuleSortChange sn) (SortChange sn) where
  applyRuleSortVarSubst sigma (Shift sign sn c) = Shift sign (fromConstRuleSortNode "invalid Shift" sn) (applyRuleSortVarSubst sigma c)
  applyRuleSortVarSubst sigma (Replace s1 s2) = Replace (applyRuleSortVarSubst sigma s1) (applyRuleSortVarSubst sigma s2)
  applyRuleSortVarSubst sigma (Reflect sn cs) = Reflect (fromConstRuleSortNode "invalid Reflect" sn) (applyRuleSortVarSubst sigma <$> cs)

-- AnnExpr

type AnnExprNodeRow (sn :: Type) (el :: Type) (er :: Row Type) = (sigma :: RuleSortVarSubst sn, label :: el | er)
newtype AnnExprNode (sn :: Type) (el :: Type) (er :: Row Type) = ExprNode (Record (AnnExprNodeRow sn el er))
derive instance Newtype (AnnExprNode sn el er) _
derive instance (Eq sn, Eq (Record (AnnExprNodeRow sn el er))) => Eq (AnnExprNode sn el er)

type AnnExpr sn el er = Tree (AnnExprNode sn el er)
type AnnExprTooth sn el er = Tooth (AnnExprNode sn el er)
type AnnExprPath sn el er = Path (AnnExprNode sn el er)
type AnnExprCursor sn el er = Cursor (AnnExprNode sn el er)
type AnnExprSelect sn el er = Select (AnnExprNode sn el er)
type AnnExprGyro sn el er = Gyro (AnnExprNode sn el er)

-- Expr (no annotation)

type Expr sn el = AnnExpr sn el ()
type ExprTooth sn el = AnnExprTooth sn el ()
type ExprPath sn el = AnnExprPath sn el ()
type ExprCursor sn el = AnnExprCursor sn el ()
type ExprSelect sn el = AnnExprSelect sn el ()
type ExprGyro sn el = AnnExprGyro sn el ()

-- Language

newtype Language sn el = Language
  { name :: String
  , getSortingRule :: el -> SortingRule sn
  , getChangingRule :: el -> ChangingRule sn 
  , topSort :: Sort sn 
  , defaultExpr :: Sort sn -> Maybe (Expr sn el) }

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule sn = SortingRule
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSort sn) 
  , parent :: RuleSort sn }

-- | A `ChangeRule` specifies the changes from the prent to eahc kid of a
-- | corresponding `SortingRule`.
newtype ChangingRule sn = ChangingRule 
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSortChange sn) }

-- RuleSortVar

newtype RuleSortVar = MakeRuleSortVar String

derive newtype instance Show RuleSortVar
derive instance Eq RuleSortVar
derive instance Ord RuleSortVar

newtype RuleSortVarSubst sn = RuleSortVarSubst (Map.Map RuleSortVar (Sort sn))
derive instance Eq sn => Eq (RuleSortVarSubst sn)

class ApplyRuleSortVarSubst sn a b | a -> b where
  applyRuleSortVarSubst :: RuleSortVarSubst sn -> a -> b

instance ApplyRuleSortVarSubst sn RuleSortVar (Sort sn) where
  applyRuleSortVarSubst (RuleSortVarSubst m) x = case Map.lookup x m of
    Nothing -> bug $ "Could not substitute RuleVar: " <> show x
    Just s -> s