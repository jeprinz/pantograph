module Pantograph.Generic.Language.Common where

import Data.Tree
import Prelude
import Util

import Bug (bug)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Prim.Row (class Union)
import Text.Pretty (class Pretty, ticks)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- Sort

data SortNode (sn :: Type)
  = SortNode sn

derive instance Generic (SortNode sn) _
derive instance Eq sn => Eq (SortNode sn)
instance Show sn => Show (SortNode sn) where show = genericShow
derive instance Functor SortNode

instance TreeNode sn => TreeNode (SortNode sn) where
  kidsCount (SortNode node) = kidsCount node

instance PrettyTreeNode sn => PrettyTreeNode (SortNode sn) where
  prettyTreeNode (SortNode node) kids = prettyTreeNode node kids

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
  applyRuleSortVarSubst sigma (Shift {sign, tooth, kid}) = Shift {sign, tooth: fromConstRuleSortNode "invalid Inject" <$> tooth, kid: applyRuleSortVarSubst sigma kid}
  applyRuleSortVarSubst sigma (Replace {old, new}) = Replace {old: applyRuleSortVarSubst sigma old, new: applyRuleSortVarSubst sigma new}
  applyRuleSortVarSubst sigma (InjectChange {node, kids}) = InjectChange {node: fromConstRuleSortNode "invalid InjectChange" node, kids: applyRuleSortVarSubst sigma <$> kids}

-- AnnExpr

type AnnExprNodeRow (sn :: Type) (el :: Type) (er :: Row Type) = (label :: el, sigma :: RuleSortVarSubst sn | er)
newtype AnnExprNode (sn :: Type) (el :: Type) (er :: Row Type) = AnnExprNode (Record (AnnExprNodeRow sn el er))
derive instance Newtype (AnnExprNode sn el er) _
derive newtype instance (Eq (Record (AnnExprNodeRow sn el er))) => Eq (AnnExprNode sn el er)
derive newtype instance (Show (Record (AnnExprNodeRow sn el er))) => Show (AnnExprNode sn el er)

instance TreeNode el => TreeNode (AnnExprNode sn el er) where
  kidsCount (AnnExprNode {label}) = kidsCount label

instance PrettyTreeNode el => PrettyTreeNode (AnnExprNode sn el er) where
  prettyTreeNode (AnnExprNode {label}) prettiedKids = prettyTreeNode label prettiedKids

type AnnExpr sn el er = Tree (AnnExprNode sn el er)
type AnnExprTooth sn el er = Tooth (AnnExprNode sn el er)
type AnnExprPath sn el er = Path (AnnExprNode sn el er)
type AnnExprNonEmptyPath sn el er = NonEmptyPath (AnnExprNode sn el er)
type AnnExprCursor sn el er = Cursor (AnnExprNode sn el er)
type AnnExprSelect sn el er = Select (AnnExprNode sn el er)
type AnnExprGyro sn el er = Gyro (AnnExprNode sn el er)
type AnnExprEdit sn el er = Edit (SortNode sn) (AnnExprNode sn el er)

-- Expr (no annotation)

type Expr sn el = AnnExpr sn el ()
type ExprNode sn el = AnnExprNode sn el ()
type ExprTooth sn el = AnnExprTooth sn el ()
type ExprPath sn el = AnnExprPath sn el ()
type ExprNonEmptyPath sn el = AnnExprNonEmptyPath sn el ()
type ExprCursor sn el = AnnExprCursor sn el ()
type ExprSelect sn el = AnnExprSelect sn el ()
type ExprGyro sn el = AnnExprGyro sn el ()
type ExprEdit sn el = AnnExprEdit sn el ()

-- erase annotations without mapping
shrinkAnnExprNode :: forall sn el er er_ er'. Union er' er_ er => AnnExprNode sn el er -> AnnExprNode sn el er'
shrinkAnnExprNode = unsafeCoerce
shrinkAnnExpr :: forall sn el er er_ er'. Union er' er_ er => AnnExpr sn el er -> AnnExpr sn el er'
shrinkAnnExpr = unsafeCoerce
shrinkAnnExprTooth :: forall sn el er er_ er'. Union er' er_ er => AnnExprTooth sn el er -> AnnExprTooth sn el er'
shrinkAnnExprTooth = unsafeCoerce
shrinkAnnExprPath :: forall sn el er er_ er'. Union er' er_ er => AnnExprPath sn el er -> AnnExprPath sn el er'
shrinkAnnExprPath = unsafeCoerce
shrinkAnnExprNonEmptyPath :: forall sn el er er_ er'. Union er' er_ er => AnnExprNonEmptyPath sn el er -> AnnExprNonEmptyPath sn el er'
shrinkAnnExprNonEmptyPath = unsafeCoerce
shrinkAnnExprCursor :: forall sn el er er_ er'. Union er' er_ er => AnnExprCursor sn el er -> AnnExprCursor sn el er'
shrinkAnnExprCursor = unsafeCoerce
shrinkAnnExprSelect :: forall sn el er er_ er'. Union er' er_ er => AnnExprSelect sn el er -> AnnExprSelect sn el er'
shrinkAnnExprSelect = unsafeCoerce
shrinkAnnExprGyro :: forall sn el er er_ er'. Union er' er_ er => AnnExprGyro sn el er -> AnnExprGyro sn el er'
shrinkAnnExprGyro = unsafeCoerce
shrinkAnnExprGyro' :: forall sn el er er_ er'. Union er_ er' er => Proxy er_ -> AnnExprGyro sn el er -> AnnExprGyro sn el er'
shrinkAnnExprGyro' _ = unsafeCoerce

-- StepExpr

data StepExpr sn el
  = Boundary {direction :: Direction, change :: SortChange sn, kid :: StepExpr sn el}
  | InjectStepExpr {node :: ExprNode sn el, maybeMarker :: Maybe Marker, kids :: Array (StepExpr sn el)}

derive instance Generic (StepExpr sn el) _
instance (Show sn, Show el) => Show (StepExpr sn el) where show x = genericShow x
instance (Eq sn, Eq el) => Eq (StepExpr sn el) where eq x y = genericEq x y

-- Direction

data Direction = Up | Down

derive instance Generic Direction _
instance Show Direction where show x = genericShow x
instance Eq Direction where eq x y = genericEq x y
instance Ord Direction where compare x y = genericCompare x y
instance Pretty Direction where
  pretty Up = "↑"
  pretty Down = "↓"

-- Marker

data Marker = CursorMarker Orientation

derive instance Generic Marker _
instance Show Marker where show x = genericShow x
instance Eq Marker where eq x = genericEq x

-- Language

newtype Language sn el = Language
  { name :: String
  , getSortingRule :: el -> SortingRule sn
  , getChangingRule :: el -> ChangingRule sn
  , topSort :: Sort sn
  , getDefaultExpr :: Sort sn -> Maybe (Expr sn el)
  , getEdits :: Sort sn -> Orientation -> Array (NonEmptyArray (ExprEdit sn el))
  , validGyro :: forall er. AnnExprGyro sn el er -> Boolean 
  , steppingRules :: Array (SteppingRule sn el)
  , matchingSyntax :: MatchingSyntax sn el
  }

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule sn = SortingRule
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSort sn) 
  , parent :: RuleSort sn }

derive instance Newtype (SortingRule sn) _

-- | A `ChangeRule` specifies the changes from the prent to eahc kid of a
-- | corresponding `SortingRule`.
newtype ChangingRule sn = ChangingRule 
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSortChange sn) }

derive instance Newtype (ChangingRule sn) _

newtype SteppingRule sn el = SteppingRule
  (StepExpr sn el -> Maybe (StepExpr sn el))

derive instance Newtype (SteppingRule sn el) _

applySteppingRule :: forall sn el. SteppingRule sn el -> StepExpr sn el -> Maybe (StepExpr sn el)
applySteppingRule = unwrap

newtype MatchingSyntax sn el = MatchingSyntax
  { parseSortNode :: String -> Maybe sn
  , parseExprLabel :: String -> Maybe el }

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