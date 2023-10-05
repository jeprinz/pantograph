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

-- Sort

data SortNode (sn :: Type)
  = SortNode sn

derive instance Generic (SortNode sn) _
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

-- Expr

newtype ExprNode (el :: Type) (ed :: Row Type) (sn :: Type) = ExprNode 
  { label :: el
  , sigma :: RuleSortVarSubst sn
  , dat :: Record ed }

type Expr el ed sn = Tree (ExprNode el ed sn)
type ExprTooth el ed sn = Tooth (ExprNode el ed sn)
type ExprPath el ed sn = Path (ExprNode el ed sn)
type ExprCursor el ed sn = Cursor (ExprNode el ed sn)
type ExprSelect el ed sn = Select (ExprNode el ed sn)
type ExprGyro el ed sn = Gyro (ExprNode el ed sn)

-- Language

newtype Language el ed sn = Language
  { name :: String
  , getSortingRule :: el -> SortingRule sn
  , getChangingRule :: el -> ChangingRule sn 
  , topSort :: Sort sn 
  , defaultExpr :: Sort sn -> Maybe (Expr el ed sn) }

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

class ApplyRuleSortVarSubst sn a b | a -> b where
  applyRuleSortVarSubst :: RuleSortVarSubst sn -> a -> b

instance ApplyRuleSortVarSubst sn RuleSortVar (Sort sn) where
  applyRuleSortVarSubst (RuleSortVarSubst m) x = case Map.lookup x m of
    Nothing -> bug $ "Could not substitute RuleVar: " <> show x
    Just s -> s