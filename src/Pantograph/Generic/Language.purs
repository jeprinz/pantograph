module Pantograph.Generic.Language where

import Prelude

import Bug (bug)
import Data.Derivative (class Derivative, differentiate, integrate)
import Data.Fix (Fix)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable)
import Data.UUID (UUID)
import Data.UUID as UUID
import Text.Pretty (class Pretty, class Pretty1, class PrettyS1, angles, bullets, newlines, pretty, pretty1, prettyS1)

-- ## Joints and Teeth

-- ### RuleJoint, RuleTooth

data RuleJoint (joint :: Type -> Type) a
  = RuleVar (joint RuleVar)
  | InjectRuleJoint (joint a)

instance Pretty1 joint => Pretty1 (RuleJoint joint) where
  pretty1 (RuleVar jRuleVar) = pretty1 jRuleVar
  pretty1 (InjectRuleJoint j) = pretty1 j

derive instance Functor joint => Functor (RuleJoint joint)
derive instance Foldable joint => Foldable (RuleJoint joint)
derive instance Traversable joint => Traversable (RuleJoint joint)

data RuleTooth (tooth :: Type -> Type) a
  = InjectRuleTooth (tooth a)

instance Derivative joint tooth => Derivative (RuleJoint joint) (RuleTooth tooth) where
  differentiate (RuleVar ruleVar) = RuleVar ruleVar
  differentiate (InjectRuleJoint j) = InjectRuleJoint $ map (map InjectRuleTooth) $ differentiate j
  
  integrate a (InjectRuleTooth j) = InjectRuleJoint $ integrate a j

instance Pretty1 tooth => Pretty1 (RuleTooth tooth) where
  pretty1 (InjectRuleTooth j) = pretty1 j

derive instance Functor tooth => Functor (RuleTooth tooth)
derive instance Foldable tooth => Foldable (RuleTooth tooth)
derive instance Traversable tooth => Traversable (RuleTooth tooth)

-- ### OpenJoint, OpenTooth

data OpenJoint (joint :: Type -> Type) a
  = HoleVar (joint HoleVar)
  | InjectOpenJoint (joint a)

instance Pretty1 joint => Pretty1 (OpenJoint joint) where
  pretty1 (HoleVar jHoleVar) = pretty1 jHoleVar
  pretty1 (InjectOpenJoint j) = pretty1 j

derive instance Functor joint => Functor (OpenJoint joint)
derive instance Foldable joint => Foldable (OpenJoint joint)
derive instance Traversable joint => Traversable (OpenJoint joint)

data OpenTooth (tooth :: Type -> Type) a
  = InjectHoleTooth (tooth a)

instance Derivative joint tooth => Derivative (OpenJoint joint) (OpenTooth tooth) where
  differentiate (HoleVar ruleVar) = HoleVar ruleVar
  differentiate (InjectOpenJoint j) = InjectOpenJoint $ map (map InjectHoleTooth) $ differentiate j
  
  integrate a (InjectHoleTooth j) = InjectOpenJoint $ integrate a j

instance Pretty1 tooth => Pretty1 (OpenTooth tooth) where
  pretty1 (InjectHoleTooth j) = pretty1 j

derive instance Functor tooth => Functor (OpenTooth tooth)
derive instance Foldable tooth => Foldable (OpenTooth tooth)
derive instance Traversable tooth => Traversable (OpenTooth tooth)

-- ### SortJoint

data SortJoint (joint :: Type -> Type) a
  = InjectSortJoint (joint a)

instance Pretty1 joint => Pretty1 (SortJoint joint) where
  pretty1 (InjectSortJoint j) = pretty1 j

derive instance Functor joint => Functor (SortJoint joint)
derive instance Foldable joint => Foldable (SortJoint joint)
derive instance Traversable joint => Traversable (SortJoint joint)

data SortTooth (tooth :: Type -> Type) a
  = InjectSortTooth (tooth a)

instance PrettyS1 tooth => PrettyS1 (SortTooth tooth) where
  prettyS1 (InjectSortTooth j) str = prettyS1 j str

instance PrettyS1 tooth => Pretty1 (SortTooth tooth) where
  pretty1 a = prettyS1 a "⌶"

derive instance Functor tooth => Functor (SortTooth tooth)
derive instance Foldable tooth => Foldable (SortTooth tooth)
derive instance Traversable tooth => Traversable (SortTooth tooth)

instance Derivative joint tooth => Derivative (SortJoint joint) (SortTooth tooth) where
  differentiate (InjectSortJoint j) = InjectSortJoint $ map (map InjectSortTooth) $ differentiate j

  integrate a (InjectSortTooth j) = InjectSortJoint $ integrate a j

-- ## Expr

data ExprJoint rule sort (joint :: Type -> Type) a
  = Expr rule (RuleVarSubst sort) (joint a)

derive instance Functor joint => Functor (ExprJoint rule sort joint)
derive instance Foldable joint => Foldable (ExprJoint rule sort joint)
derive instance Traversable joint => Traversable (ExprJoint rule sort joint)

data ExprTooth rule sort (tooth :: Type -> Type) a
  = ExprTooth rule (RuleVarSubst sort) (tooth a)

derive instance Functor tooth => Functor (ExprTooth rule sort tooth)
derive instance Foldable tooth => Foldable (ExprTooth rule sort tooth)
derive instance Traversable tooth => Traversable (ExprTooth rule sort tooth)

instance Derivative joint tooth => Derivative (ExprJoint rule sort joint) (ExprTooth rule sort tooth) where
  differentiate (Expr rule sigma j) = Expr rule sigma $ map (map (ExprTooth rule sigma)) $ differentiate j
  integrate a (ExprTooth rule sigma t) = Expr rule sigma $ integrate a t

-- ### ChangeJoint

data ChangeJoint (joint :: Type -> Type) (tooth :: Type -> Type) a
  = Plus (tooth (Fix joint)) a
  | Minus (tooth (Fix joint)) a
  | Replace (Fix joint) (Fix joint)
  | InjectChangeJoint (joint a)

-- ### CursorJoint

-- | A Cursor is wrapped teeth until an end.
data CursorJoint (tooth :: Type -> Type) x y (a :: Type)
  = CursorThere (tooth x) a
  | CursorHere y

-- | A Select is a Cursor to a Cursor.
type SelectJoint tooth x = CursorJoint tooth x (Fix (CursorJoint tooth x x))

-- ## Path

newtype Path (tooth :: Type -> Type) a = Path (List (tooth a))

-- ## Language

class
  ( Pretty rule
  , Pretty1 joint, Functor joint, Foldable joint, Traversable joint
  , PrettyS1 tooth, Functor tooth, Foldable tooth, Traversable tooth, Derivative joint tooth)
  <= Language rule (joint :: Type -> Type) (tooth :: Type -> Type)
  | joint -> rule tooth, rule -> joint tooth, tooth -> joint rule
  where
  getProductionRule :: rule -> ProductionRule joint
  getChangeRule :: rule -> ChangeRule tooth joint
  defaultExpr :: Fix (PrgmSortJoint joint) -> Maybe (Fix (PrgmExprJoint rule joint joint))

type PrgmSortJoint joint = SortJoint (OpenJoint joint)
type PrgmExprJoint rule sortJoint exprJoint = ExprJoint rule (Fix (PrgmSortJoint sortJoint)) (OpenJoint exprJoint)
type PrgmCursorJoint rule sortJoint exprJoint exprTooth = CursorJoint (ExprTooth rule (Fix (PrgmSortJoint sortJoint)) (OpenTooth exprTooth)) (Fix (PrgmExprJoint rule sortJoint exprJoint)) (Fix (PrgmExprJoint rule sortJoint exprJoint))
type PrgmSelectJoint rule sortJoint exprJoint exprTooth = SelectJoint (ExprTooth rule (Fix (PrgmSortJoint sortJoint)) (OpenTooth exprTooth)) (Fix (PrgmExprJoint rule sortJoint exprJoint))

type RuleSortJoint joint = SortJoint (RuleJoint joint)
type SortChangeJoint joint tooth = ChangeJoint (SortJoint (RuleJoint joint)) (SortTooth (RuleTooth tooth))

-- | A `ProductionRule` specifies the relationship between the sorts of the
-- | parent and kids of a production.
newtype ProductionRule joint = ProductionRule
  { parameters :: Set.Set RuleVar
  , kids :: joint (Fix (RuleSortJoint joint))
  , parent :: Fix (RuleSortJoint joint) }

-- | A `ChangeRule` specifies the changes from the parent to each kid of a
-- | corresponding `ProductionRule`.
newtype ChangeRule tooth joint = ChangeRule
  { parameters :: Set.Set RuleVar
  , kids :: joint (Fix (SortChangeJoint joint tooth)) }

-- ## Vars

-- ### HoleVar

data HoleVar = MakeHoleVar RuleVar UUID

derive instance Generic HoleVar _
instance Show HoleVar where show = genericShow
instance Pretty HoleVar where pretty (MakeHoleVar ruleVar uuid) = pretty ruleVar <> "@" <> String.take 2 (UUID.toString uuid)

-- ### RuleVar

newtype RuleVarSubst a = RuleVarSubst (Map.Map RuleVar a)

data RuleVar = MakeRuleVar String

instance Pretty RuleVar where
  pretty (MakeRuleVar str) = angles str

derive instance Generic RuleVar _
derive instance Eq RuleVar
derive instance Ord RuleVar
instance Show RuleVar where show = genericShow

applyRuleVarSubst :: forall a. Pretty a => RuleVarSubst a -> RuleVar -> a
applyRuleVarSubst (RuleVarSubst m) ruleVar = case Map.lookup ruleVar m of
  Nothing -> bug $ newlines
    [ "[applyRuleVarSubst] Attempted to substitute an unhandled RuleVar."
    , bullets
        [ "ruleVar = " <> pretty ruleVar
        , "m = " <> pretty m ] ]
  Just a -> a