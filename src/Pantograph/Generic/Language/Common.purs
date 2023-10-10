module Pantograph.Generic.Language.Common where

import Data.Tree
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
import Prim.Row (class Lacks, class Union)
import Record as R
import Text.Pretty (class Pretty, parens, pretty, spaces, ticks)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

makeConstRuleSort n kids = Tree {node: ConstRuleSortNode (SortNode n), kids}
makeVarRuleSort x = Tree {node: VarRuleSortNode x, kids: []}
makeSort sn kids = Tree {node: SortNode sn, kids}
makeExpr label sigma dat kids = Tree {node: AnnExprNode {label, sigma}, kids}

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

fromConstRuleSortNode msg = case _ of
  ConstRuleSortNode sn -> sn
  rn -> bug $ msg <> "expected " <> ticks "ConstRuleSortNode _" <> " but found " <> ticks (show rn)

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
  applyRuleSortVarSubst sigma (Shift sign st c) = Shift sign (fromConstRuleSortNode "invalid Reflect" <$> st) (applyRuleSortVarSubst sigma c)
  applyRuleSortVarSubst sigma (Replace s1 s2) = Replace (applyRuleSortVarSubst sigma s1) (applyRuleSortVarSubst sigma s2)
  applyRuleSortVarSubst sigma (Reflect sn cs) = Reflect (fromConstRuleSortNode "invalid Reflect" sn) (applyRuleSortVarSubst sigma <$> cs)

-- AnnExpr

type AnnExprNodeRow (sn :: Type) (el :: Type) (er :: Row Type) = (label :: el, sigma :: RuleSortVarSubst sn | er)
newtype AnnExprNode (sn :: Type) (el :: Type) (er :: Row Type) = AnnExprNode (Record (AnnExprNodeRow sn el er))
derive instance Newtype (AnnExprNode sn el er) _
derive newtype instance (Eq (Record (AnnExprNodeRow sn el er))) => Eq (AnnExprNode sn el er)
derive newtype instance (Show (Record (AnnExprNodeRow sn el er))) => Show (AnnExprNode sn el er)

instance PrettyTreeNode el => PrettyTreeNode (AnnExprNode sn el er) where
  prettyTreeNode (AnnExprNode {label}) prettiedKids = prettyTreeNode label prettiedKids

type AnnExpr sn el er = Tree (AnnExprNode sn el er)
type AnnExprTooth sn el er = Tooth (AnnExprNode sn el er)
type AnnExprPath sn el er = Path (AnnExprNode sn el er)
type AnnExprCursor sn el er = Cursor (AnnExprNode sn el er)
type AnnExprSelect sn el er = Select (AnnExprNode sn el er)
type AnnExprGyro sn el er = Gyro (AnnExprNode sn el er)

-- Expr (no annotation)

type Expr sn el = AnnExpr sn el ()
type ExprNode sn el = AnnExprNode sn el ()
type ExprTooth sn el = AnnExprTooth sn el ()
type ExprPath sn el = AnnExprPath sn el ()
type ExprCursor sn el = AnnExprCursor sn el ()
type ExprSelect sn el = AnnExprSelect sn el ()
type ExprGyro sn el = AnnExprGyro sn el ()

-- erase annotations without mapping
unAnnExprNode :: forall sn el er. AnnExprNode sn el er -> ExprNode sn el
unAnnExprNode = unsafeCoerce
unAnnExpr :: forall sn el er. AnnExpr sn el er -> Expr sn el
unAnnExpr = unsafeCoerce
unAnnExprTooth :: forall sn el er. AnnExprTooth sn el er -> ExprTooth sn el
unAnnExprTooth = unsafeCoerce
unAnnExprPath :: forall sn el er. AnnExprPath sn el er -> ExprPath sn el
unAnnExprPath = unsafeCoerce
unAnnExprCursor :: forall sn el er. AnnExprCursor sn el er -> ExprCursor sn el
unAnnExprCursor = unsafeCoerce
unAnnExprSelect :: forall sn el er. AnnExprSelect sn el er -> ExprSelect sn el
unAnnExprSelect = unsafeCoerce
unAnnExprGyro :: forall sn el er. AnnExprGyro sn el er -> ExprGyro sn el
unAnnExprGyro = unsafeCoerce

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
derive newtype instance Eq sn => Eq (RuleSortVarSubst sn)
derive newtype instance Show sn => Show (RuleSortVarSubst sn)

class ApplyRuleSortVarSubst sn a b | a -> b where
  applyRuleSortVarSubst :: RuleSortVarSubst sn -> a -> b

instance ApplyRuleSortVarSubst sn RuleSortVar (Sort sn) where
  applyRuleSortVarSubst (RuleSortVarSubst m) x = case Map.lookup x m of
    Nothing -> bug $ "Could not substitute RuleVar: " <> show x
    Just s -> s