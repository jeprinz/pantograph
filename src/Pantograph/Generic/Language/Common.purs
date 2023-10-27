module Pantograph.Generic.Language.Common where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude
import Util

import Bug (bug)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldM, traverse_)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.SearchableArray (SearchableArray)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.StringQuery (StringQuery)
import Data.Subtype (class Subtype, inject, project)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Union)
import Text.Pretty (class Pretty, inner, outer, pretty, ticks, (<+>))
import Todo (todo)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- infixes

infix 5 StepExpr as %.
infix 5 Boundary as %.|

-- Sort

data SortNode (sn :: Type)
  = SortNode sn
  | VarSortNode SortVar

derive instance Generic (SortNode sn) _
derive instance Eq sn => Eq (SortNode sn)
instance Show sn => Show (SortNode sn) where show = genericShow
derive instance Functor SortNode

-- type SortNodePattern sn = EqPattern (SortNode sn)

instance TreeNode sn => TreeNode (SortNode sn) where
  kidsCount (SortNode node) = kidsCount node
  kidsCount (VarSortNode _) = 0

instance (Show sn, PrettyTreeNode sn) => PrettyTreeNode (SortNode sn) where
  prettyTreeNode sn =
    let ass = assertValidTreeKids "prettyTreeNode" sn in
    case sn of
      SortNode node -> ass \kids -> prettyTreeNode node kids
      VarSortNode var -> ass \[] -> pretty var

type Sort sn = Tree (SortNode sn)
type SortTooth sn = Tooth (SortNode sn)
type SortPath sn = Path (SortNode sn)

-- RuleSort

data RuleSortNode sn
  = InjectRuleSortNode (SortNode sn)
  | VarRuleSortNode RuleSortVar

derive instance Generic (RuleSortNode sn) _
instance Show sn => Show (RuleSortNode sn) where show = genericShow
instance Eq sn => Eq (RuleSortNode sn) where eq = genericEq

instance TreeNode sn => TreeNode (RuleSortNode sn) where
  kidsCount (InjectRuleSortNode node) = kidsCount node
  kidsCount (VarRuleSortNode _) = 0

instance (Show sn, PrettyTreeNode sn) => PrettyTreeNode (RuleSortNode sn) where
  prettyTreeNode rsn =
    let ass = assertValidTreeKids "prettyTreeNode" rsn in
    case rsn of
      InjectRuleSortNode node -> ass \kids -> prettyTreeNode node kids
      VarRuleSortNode x -> ass \[] -> pretty x

makeVarRuleSort :: forall sn. RuleSortVar -> Tree (RuleSortNode sn)
makeVarRuleSort x = Tree (VarRuleSortNode x) []

makeSort :: forall sn. sn -> Array (Sort sn) -> Sort sn
makeSort sn kids = Tree (SortNode sn) kids

makeVarSort :: forall sn. SortVar -> Sort sn
makeVarSort x = Tree (VarSortNode x) []

freshVarSort :: forall sn. String -> Tree (SortNode sn)
freshVarSort label = Tree (VarSortNode (SortVar {label, uuid: unsafePerformEffect UUID.genUUID})) []

injectRuleSort :: forall sn. Sort sn -> RuleSort sn
injectRuleSort = map inject

projectRuleSort :: forall sn. RuleSort sn -> Maybe (Sort sn)
projectRuleSort = traverse project

instance Subtype (SortNode sn) (RuleSortNode sn) where
  inject = InjectRuleSortNode
  project = case _ of
    InjectRuleSortNode sn -> Just sn
    VarRuleSortNode _ -> Nothing

type RuleSort sn = Tree (RuleSortNode sn)
type RuleSortTooth sn = Tooth (RuleSortNode sn)
type RuleSortPath sn = Path (RuleSortNode sn)

-- SortChange

type SortChange sn = Change (SortNode sn)

-- RuleSortChange

type RuleSortChange sn = Change (RuleSortNode sn)

-- AnnExpr

data AnnExprNode (sn :: Type) (el :: Type) (er :: Row Type) = ExprNode el (RuleSortVarSubst sn) (Record er)
derive instance Generic (AnnExprNode sn el er) _
derive instance (Eq sn, Eq el, Eq (Record er)) => Eq (AnnExprNode sn el er)
instance (Show el, Show sn, Show (Record er)) => Show (AnnExprNode sn el er) where show = genericShow

annExprAnn :: forall sn el er. Tree (AnnExprNode sn el er) -> Record er
annExprAnn (Tree node _) = annExprNodeAnn node

annExprNodeAnn :: forall sn el er. AnnExprNode sn el er -> Record er
annExprNodeAnn (ExprNode _ _ er) = er

instance TreeNode el => TreeNode (AnnExprNode sn el er) where
  kidsCount (ExprNode label _ _) = kidsCount label

instance PrettyTreeNode el => PrettyTreeNode (AnnExprNode sn el er) where
  prettyTreeNode (ExprNode label _ _) prettiedKids = prettyTreeNode label prettiedKids

type AnnExpr sn el er = Tree (AnnExprNode sn el er)
type AnnExprTooth sn el er = Tooth (AnnExprNode sn el er)
type AnnExprPath sn el er = Path (AnnExprNode sn el er)
type AnnExprNonEmptyPath sn el er = NonEmptyPath (AnnExprNode sn el er)
type AnnExprCursor sn el er = Cursor (AnnExprNode sn el er)
type AnnExprSelect sn el er = Select (AnnExprNode sn el er)
type AnnExprGyro sn el er = Gyro (AnnExprNode sn el er)

-- Expr (no annotation)

type Expr sn el = AnnExpr sn el ()
type ExprNode sn el = AnnExprNode sn el ()
type ExprTooth sn el = AnnExprTooth sn el ()
type ExprPath sn el = AnnExprPath sn el ()
type ExprNonEmptyPath sn el = AnnExprNonEmptyPath sn el ()
type ExprCursor sn el = AnnExprCursor sn el ()
type ExprSelect sn el = AnnExprSelect sn el ()
type ExprGyro sn el = AnnExprGyro sn el ()

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
  = StepExpr (Maybe Marker /\ ExprNode sn el) (Array (StepExpr sn el))
  | Boundary (Direction /\ SortChange sn) (StepExpr sn el)

derive instance Generic (StepExpr sn el) _
instance (Show sn, Show el) => Show (StepExpr sn el) where show x = genericShow x
instance (Eq sn, Eq el) => Eq (StepExpr sn el) where eq x y = genericEq x y

instance (Show sn, PrettyTreeNode el, PrettyTreeNode sn) => Pretty (StepExpr sn el) where
  pretty = case _ of
    Boundary (dir /\ ch) e -> "{{ " <> pretty dir <> " | " <> pretty ch <> " | " <> pretty e <> " }}"
    StepExpr (Nothing /\ node) kids -> prettyTreeNode node (pretty <$> kids)
    StepExpr (Just (CursorMarker Outside) /\ node) kids -> outer $ prettyTreeNode node (pretty <$> kids)
    StepExpr (Just (CursorMarker Inside) /\ node) kids -> inner $ prettyTreeNode node (pretty <$> kids)

instance Subtype (AnnExpr sn el ()) (StepExpr sn el) where
  inject (Tree node kids) = StepExpr (Nothing /\ node) (inject <$> kids)
  project (StepExpr (Nothing /\ node) kids) = Tree node <$> project `traverse` kids
  project _ = Nothing

-- | Erases markers in `StepExpr`
fromStepExprToExpr :: forall sn el. StepExpr sn el -> Expr sn el
fromStepExprToExpr (StepExpr (_ /\ node) kids) = Tree node (fromStepExprToExpr <$> kids)
fromStepExprToExpr (Boundary _ e) = fromStepExprToExpr e

-- Direction

data Direction = Up | Down

derive instance Generic Direction _
instance Show Direction where show = genericShow
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

class 
    (Eq sn, Show sn, PrettyTreeNode sn, Eq el, Show el, PrettyTreeNode el) <=
    Language sn el | sn -> el, el -> sn
  where
  getSortingRule :: el -> SortingRule sn
  getChangingRule :: el -> ChangingRule sn
  topSort :: Sort sn
  getDefaultExpr :: Sort sn -> Maybe (Expr sn el)

  steppingRules :: Array (SteppingRule sn el)

  getEdits :: Sort sn -> Orientation -> Edits sn el
  
  specialEdits ::
    { deleteCursor :: Sort sn -> Maybe (Edit sn el)
    , deleteSelect :: SortChange sn -> Maybe (Edit sn el)
    , enter :: Unit -> Maybe (Edit sn el)
    , tab :: Unit -> Maybe (Edit sn el) }

  validGyro :: forall er. AnnExprGyro sn el er -> Boolean

newtype Edits sn el = Edits (StringQuery (String /\ NonEmptyArray (Edit sn el)) Fuzzy.Distance)
  -- = SearchableEdits (SearchableArray (String /\ NonEmptyArray (Edit sn el)) Fuzzy.Distance)
  -- | StringEdits (String -> Array (NonEmptyArray (Edit sn el)))

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule sn = SortingRule
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSort sn) 
  , parent :: RuleSort sn }

derive instance Newtype (SortingRule sn) _

-- | A `ChangeRule` specifies the changes from each kid to the parent of a
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

-- Edit

newtype Edit sn el = Edit 
  { outerChange :: Maybe (SortChange sn)
  , middle :: Maybe (ExprNonEmptyPath sn el)
  , innerChange :: Maybe (SortChange sn)
  , inside :: Maybe (Expr sn el) }

derive newtype instance (Show sn, Show el) => Show (Edit sn el)
derive newtype instance (Eq sn, Eq el) => Eq (Edit sn el)

-- RuleSortVar

newtype RuleSortVar = MakeRuleSortVar String

derive newtype instance Show RuleSortVar
derive instance Eq RuleSortVar
derive instance Ord RuleSortVar

instance Pretty RuleSortVar where pretty (MakeRuleSortVar str) = "$" <> str

newtype RuleSortVarSubst sn = RuleSortVarSubst (Map.Map RuleSortVar (Sort sn))
derive newtype instance Eq sn => Eq (RuleSortVarSubst sn)
derive newtype instance Show sn => Show (RuleSortVarSubst sn)
derive instance Functor RuleSortVarSubst

instance Language sn el => Pretty (RuleSortVarSubst sn) where
  pretty (RuleSortVarSubst m) = "{" <> Array.intercalate ", " (items <#> \(x /\ s) -> pretty x <+> ":=" <+> pretty s) <> "}"
    where
    items = Map.toUnfoldable m :: Array _

class ApplyRuleSortVarSubst sn a b | a -> b where
  applyRuleSortVarSubst :: RuleSortVarSubst sn -> a -> b

instance Language sn el => ApplyRuleSortVarSubst sn RuleSortVar (Sort sn) where
  applyRuleSortVarSubst sigma@(RuleSortVarSubst m) x = case Map.lookup x m of
    Nothing -> bug $ "Could not substitute RuleSortVar: " <> show x <> "; sigma = " <> pretty sigma <> "; x = " <> pretty x
    Just s -> s

instance Language sn el => ApplyRuleSortVarSubst sn (RuleSort sn) (Sort sn) where
  applyRuleSortVarSubst sigma (Tree (InjectRuleSortNode sn) kids) = Tree sn (applyRuleSortVarSubst sigma <$> kids)
  applyRuleSortVarSubst sigma (Tree (VarRuleSortNode x) _) = applyRuleSortVarSubst sigma x

instance Language sn el => ApplyRuleSortVarSubst sn (RuleSortChange sn) (SortChange sn) where
  applyRuleSortVarSubst sigma (Shift (sign /\ tooth) kid) = Shift (sign /\ (fromJust <<< project <$> tooth)) (applyRuleSortVarSubst sigma kid)
  applyRuleSortVarSubst sigma (Replace old new) = Replace (applyRuleSortVarSubst sigma old) (applyRuleSortVarSubst sigma new)
  applyRuleSortVarSubst sigma (InjectChange node kids) = InjectChange (fromJust $ project node) (applyRuleSortVarSubst sigma <$> kids)

-- SortVar

newtype SortVar = SortVar {label :: String, uuid :: UUID}

derive instance Generic SortVar _
derive newtype instance Show SortVar
derive newtype instance Eq SortVar
derive newtype instance Ord SortVar

instance Pretty SortVar where
  pretty (SortVar {label, uuid}) = "?" <> label <> "#" <> String.take 2 (UUID.toString uuid)

newtype SortVarSubst sn = SortVarSubst (Map.Map SortVar (Sort sn))
derive newtype instance Eq sn => Eq (SortVarSubst sn)
derive newtype instance Show sn => Show (SortVarSubst sn)
derive instance Functor SortVarSubst

-- | `SortVarSubst` forms a `Semigroup` via composition.
instance Language sn el => Semigroup (SortVarSubst sn) where
  append (SortVarSubst m1) sigma2@(SortVarSubst m2) = SortVarSubst (Map.union m1 (m2 <#> applySortVarSubst sigma2))

instance Language sn el => Monoid (SortVarSubst sn) where
  mempty = SortVarSubst Map.empty

instance Language sn el => Pretty (SortVarSubst sn) where
  pretty (SortVarSubst m) = "{" <> Array.intercalate ", " (items <#> \(x /\ s) -> pretty x <+> ":=" <+> pretty s) <> "}"
    where
    items = Map.toUnfoldable m :: Array _

class ApplySortVarSubst sn a b | a -> b where
  applySortVarSubst :: SortVarSubst sn -> a -> b

instance Language sn el => ApplySortVarSubst sn SortVar (Sort sn) where
  applySortVarSubst (SortVarSubst m) x = case Map.lookup x m of
    Nothing -> makeVarSort x
    Just s -> s

instance Language sn el => ApplySortVarSubst sn (Sort sn) (Sort sn) where
  applySortVarSubst sigma (Tree sn@(SortNode _) kids) = Tree sn (applySortVarSubst sigma <$> kids)
  applySortVarSubst sigma (Tree (VarSortNode x) _) = applySortVarSubst sigma x

instance Language sn el => ApplySortVarSubst sn (SortChange sn) (SortChange sn) where
  applySortVarSubst sigma (Shift (sign /\ tooth) kid) = Shift (sign /\ tooth) (applySortVarSubst sigma kid)
  applySortVarSubst sigma (Replace old new) = Replace (applySortVarSubst sigma old) (applySortVarSubst sigma new)
  applySortVarSubst sigma (InjectChange node kids) = InjectChange node (applySortVarSubst sigma <$> kids)

instance Language sn el => ApplySortVarSubst sn (SortVarSubst sn) (SortVarSubst sn) where
  applySortVarSubst = append

instance Language sn el => ApplySortVarSubst sn (RuleSortVarSubst sn) (RuleSortVarSubst sn) where
  applySortVarSubst sigma (RuleSortVarSubst m) = RuleSortVarSubst $ m <#> applySortVarSubst sigma

instance Language sn el => ApplySortVarSubst sn (AnnExprNode sn el er) (AnnExprNode sn el er) where
  applySortVarSubst sigma (ExprNode label sigma' er) = ExprNode label (applySortVarSubst sigma sigma') er

instance Language sn el => ApplySortVarSubst sn (AnnExpr sn el er) (AnnExpr sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprTooth sn el er) (AnnExprTooth sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprPath sn el er) (AnnExprPath sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprNonEmptyPath sn el er) (AnnExprNonEmptyPath sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)
