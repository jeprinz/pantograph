module Pantograph.Generic.Language.Common where

import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq.Generic (genericEq)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.StringQuery (StringQuery)
import Data.Subtype (class Subtype, inject, project)
import Data.Traversable (traverse)
import Data.Tree (class PrettyTreeNode, class TreeNode, Change(..), Cursor, Gyro, NonEmptyPath, Orientation(..), Path, Select, Tooth, Tree(..), assertValidTreeKids, kidsCount, lub', prettyTreeNode)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Union)
import Text.Pretty (class Pretty, braces2, inner, outer, pretty, (<+>))
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Util (fromJust)

-- infixes

infix 5 StepExpr as %.
infix 5 Boundary as %.|

-- Sort

data SortNode (sn :: Type)
  = SN sn
  | VarSN SortVar

derive instance Generic (SortNode sn) _
derive instance Eq sn => Eq (SortNode sn)
instance Show sn => Show (SortNode sn) where show = genericShow
derive instance Functor SortNode

-- type SortNodePattern sn = EqPattern (SortNode sn)

instance TreeNode sn => TreeNode (SortNode sn) where
  kidsCount (SN node) = kidsCount node
  kidsCount (VarSN _) = 0

instance (Show sn, PrettyTreeNode sn) => PrettyTreeNode (SortNode sn) where
  prettyTreeNode sn =
    let ass = assertValidTreeKids "prettyTreeNode" sn in
    case sn of
      SN node -> ass \kids -> prettyTreeNode node kids
      VarSN var -> ass \[] -> pretty var

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
makeSort sn kids = Tree (SN sn) kids

makeVarSort :: forall sn. SortVar -> Sort sn
makeVarSort x = Tree (VarSN x) []

freshVarSort :: forall sn. String -> Tree (SortNode sn)
freshVarSort label = Tree (VarSN (SortVar {label, uuid: unsafePerformEffect UUID.genUUID})) []

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

data AnnExprNode (sn :: Type) (el :: Type) (er :: Row Type) = EN el (RuleSortVarSubst (Sort sn)) (Record er)
derive instance Generic (AnnExprNode sn el er) _
derive instance (Eq sn, Eq el, Eq (Record er)) => Eq (AnnExprNode sn el er)
instance (Show el, Show sn, Show (Record er)) => Show (AnnExprNode sn el er) where show = genericShow

annExprAnn :: forall sn el er. Tree (AnnExprNode sn el er) -> Record er
annExprAnn (Tree node _) = annExprNodeAnn node

annExprNodeAnn :: forall sn el er. AnnExprNode sn el er -> Record er
annExprNodeAnn (EN _ _ er) = er

instance TreeNode el => TreeNode (AnnExprNode sn el er) where
  kidsCount (EN label _ _) = kidsCount label

instance PrettyTreeNode el => PrettyTreeNode (AnnExprNode sn el er) where
  prettyTreeNode (EN label _ _) prettiedKids = prettyTreeNode label prettiedKids

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
  = StepExpr (ExprNode sn el) (Array (StepExpr sn el))
  | Boundary (Direction /\ SortChange sn) (StepExpr sn el)
  | Marker (StepExpr sn el)

derive instance Generic (StepExpr sn el) _
instance (Show sn, Show el) => Show (StepExpr sn el) where show x = genericShow x
instance (Eq sn, Eq el) => Eq (StepExpr sn el) where eq x y = genericEq x y

instance (Show sn, PrettyTreeNode el, PrettyTreeNode sn) => Pretty (StepExpr sn el) where
  pretty = case _ of
    StepExpr node kids -> prettyTreeNode node (pretty <$> kids)
    Boundary (dir /\ ch) e -> "{{ " <> pretty dir <> " | " <> pretty ch <> " | " <> pretty e <> " }}"
    Marker kid -> braces2 $ pretty kid

instance Subtype (AnnExpr sn el ()) (StepExpr sn el) where
  inject (Tree node kids) = StepExpr node (inject <$> kids)
  project (StepExpr node kids) = Tree node <$> project `traverse` kids
  project _ = Nothing

-- | Erases markers in `StepExpr`
fromStepExprToExpr :: forall sn el. StepExpr sn el -> Expr sn el
fromStepExprToExpr (StepExpr node kids) = Tree node (fromStepExprToExpr <$> kids)
fromStepExprToExpr (Boundary _ e) = fromStepExprToExpr e
fromStepExprToExpr (Marker e) = fromStepExprToExpr e

-- Direction

data Direction = Up | Down

derive instance Generic Direction _
instance Show Direction where show = genericShow
instance Eq Direction where eq x y = genericEq x y
instance Ord Direction where compare x y = genericCompare x y
instance Pretty Direction where
  pretty Up = "↑"
  pretty Down = "↓"

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

  getEditsAtSort :: Sort sn -> Orientation -> Edits sn el
  specialEdits :: SpecialEdits sn el

  validGyro :: forall er. AnnExprGyro sn el er -> Boolean

-- | A collection of (optional) special edits that are applied in different
-- | situations.
type SpecialEdits sn el =
  { -- delete an expr (this is applied to the cursor in order to delete)
    deleteExpr :: Sort sn -> Maybe (Edit sn el)
    -- copy an expr (this is applied to the expr in the clipboard)
  , copyExpr :: Sort sn -> Maybe (Edit sn el)
    -- delete a path (this is applied to the select in order to delete)
  , deletePath :: SortChange sn -> Maybe (Edit sn el)
    -- copy a path (this is applied to the path in the clipboard)
  , copyPath :: SortChange sn -> Maybe (Edit sn el)
  -- 'enter' key
  , enter :: Unit -> Maybe (Edit sn el)
  -- 'tab' key
  , tab :: Unit -> Maybe (Edit sn el) }

newtype Edits sn el = Edits (StringQuery (String /\ NonEmptyArray (Edit sn el)) Fuzzy.Distance)

-- | A `SortingRule` specifies the relationship between the sorts of the parent
-- | an kids of a production.
newtype SortingRule sn = SortingRule
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSort sn) 
  , parent :: RuleSort sn }

derive instance Newtype (SortingRule sn) _

-- | A `ChangeRule` specifies the changes from each kid to the parent of a
-- | corresponding `SortingRule`. The `parent` is probably just the injection of
-- | the the parent's sort, using `RuleSortVar`s appropriately. This is useful
-- | for computing how changes in the kids' sorts are reflected as changes in
-- | the parent's sort.
newtype ChangingRule sn = ChangingRule 
  { parameters :: Set.Set RuleSortVar
  , kids :: Array (RuleSortChange sn)
  , parent :: RuleSortChange sn }

derive instance Newtype (ChangingRule sn) _

newtype SteppingRule sn el = SteppingRule
  (Language sn el => StepExpr sn el -> Maybe (StepExpr sn el))

applySteppingRule :: forall sn el. Language sn el => SteppingRule sn el -> StepExpr sn el -> Maybe (StepExpr sn el)
applySteppingRule (SteppingRule f) = f

-- Edit

-- | An `Edit` is described by the changes, inserted path, and replacing expr
-- | that are applied at the cursor to perform the `Edit`.
newtype Edit sn el = Edit 
  { outerChange :: Maybe (SortChange sn)
  , middle :: Maybe (ExprNonEmptyPath sn el)
  , innerChange :: Maybe (SortChange sn)
  , inside :: Maybe (Expr sn el) }

derive instance Newtype (Edit sn el) _
derive newtype instance (Show sn, Show el) => Show (Edit sn el)
derive newtype instance (Eq sn, Eq el) => Eq (Edit sn el)

-- RuleSortVar

newtype RuleSortVar = MakeRuleSortVar String

derive newtype instance Show RuleSortVar
derive instance Eq RuleSortVar
derive instance Ord RuleSortVar

instance Pretty RuleSortVar where pretty (MakeRuleSortVar str) = "$" <> str

newtype RuleSortVarSubst v = RuleSortVarSubst (Map.Map RuleSortVar v)
derive newtype instance (Eq v) => Eq (RuleSortVarSubst v)
derive newtype instance (Show v) => Show (RuleSortVarSubst v)
derive instance Functor (RuleSortVarSubst)

-- | `RuleSortVarSubst (SortChange sn)` forms a `Monoid` since we can `lub` the
-- | multiple assignments of a single `RuleSortVar`.
instance (Eq sn, Show sn, PrettyTreeNode sn) => Semigroup (RuleSortVarSubst (SortChange sn)) where
  append (RuleSortVarSubst m1) (RuleSortVarSubst m2) = RuleSortVarSubst (Map.unionWith lub' m1 m2)

instance (Eq sn, Show sn, PrettyTreeNode sn) => Monoid (RuleSortVarSubst (SortChange sn)) where
  mempty = emptyRuleSortVarSubst

emptyRuleSortVarSubst :: forall a. RuleSortVarSubst a
emptyRuleSortVarSubst = RuleSortVarSubst Map.empty

singletonRuleSortVarSubst :: forall a. RuleSortVar -> a -> RuleSortVarSubst a
singletonRuleSortVarSubst k v = RuleSortVarSubst $ Map.singleton k v

instance (Pretty v) => Pretty (RuleSortVarSubst v) where
  pretty (RuleSortVarSubst m) = "{" <> Array.intercalate ", " (items <#> \(x /\ s) -> pretty x <+> ":=" <+> pretty s) <> "}"
    where
    items = Map.toUnfoldable m :: Array _

lookupRuleSortVarSubst :: forall sn v. Show sn => PrettyTreeNode sn => Pretty v => RuleSortVar -> RuleSortVarSubst v -> v
lookupRuleSortVarSubst x sigma@(RuleSortVarSubst m) = case Map.lookup x m of
  Nothing -> bug $ "Could not find RuleSortVar " <> show x <> " in RuleSortVarSubst " <> pretty sigma
  Just v -> v

class ApplyRuleSortVarSubst v a b | v a -> b where
  applyRuleSortVarSubst :: RuleSortVarSubst v -> a -> b

instance (Show sn, PrettyTreeNode sn) => ApplyRuleSortVarSubst (Sort sn) RuleSortVar (Sort sn) where
  applyRuleSortVarSubst sigma@(RuleSortVarSubst m) x = case Map.lookup x m of
    Nothing -> bug $ "Could not find RuleSortVar " <> show x <> " in RuleSortVarSubst " <> pretty sigma
    Just sr -> sr

instance (Show sn, PrettyTreeNode sn) => ApplyRuleSortVarSubst (Sort sn) String (Sort sn) where
  applyRuleSortVarSubst sigma string = applyRuleSortVarSubst sigma (MakeRuleSortVar string)

instance (Show sn, PrettyTreeNode sn) => ApplyRuleSortVarSubst (Sort sn) (RuleSort sn) (Sort sn) where
  applyRuleSortVarSubst sigma (Tree (InjectRuleSortNode sn) kids) = Tree sn (applyRuleSortVarSubst sigma <$> kids)
  applyRuleSortVarSubst sigma (Tree (VarRuleSortNode x) _) = applyRuleSortVarSubst sigma x

instance (Show sn, PrettyTreeNode sn) => ApplyRuleSortVarSubst (Sort sn) (RuleSortChange sn) (SortChange sn) where
  applyRuleSortVarSubst sigma (Shift (sign /\ tooth) kid) = Shift (sign /\ (fromJust <<< project <$> tooth)) (applyRuleSortVarSubst sigma kid)
  applyRuleSortVarSubst sigma (Replace old new) = Replace (applyRuleSortVarSubst sigma old) (applyRuleSortVarSubst sigma new)
  applyRuleSortVarSubst sigma (InjectChange node kids) = InjectChange (fromJust $ project node) (applyRuleSortVarSubst sigma <$> kids)

instance (Show sn, PrettyTreeNode sn) => ApplyRuleSortVarSubst (SortChange sn) (RuleSortChange sn) (SortChange sn) where
  applyRuleSortVarSubst sigma (Shift (sign /\ tooth) kid) = Shift (sign /\ (fromJust <<< project <$> tooth)) (applyRuleSortVarSubst sigma kid)
  applyRuleSortVarSubst _ (Replace old new) = Replace (fromJust $ project old) (fromJust $ project new)
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
  applySortVarSubst sigma (Tree sn@(SN _) kids) = Tree sn (applySortVarSubst sigma <$> kids)
  applySortVarSubst sigma (Tree (VarSN x) _) = applySortVarSubst sigma x

instance Language sn el => ApplySortVarSubst sn (SortChange sn) (SortChange sn) where
  applySortVarSubst sigma (Shift (sign /\ tooth) kid) = Shift (sign /\ tooth) (applySortVarSubst sigma kid)
  applySortVarSubst sigma (Replace old new) = Replace (applySortVarSubst sigma old) (applySortVarSubst sigma new)
  applySortVarSubst sigma (InjectChange node kids) = InjectChange node (applySortVarSubst sigma <$> kids)

instance Language sn el => ApplySortVarSubst sn (SortVarSubst sn) (SortVarSubst sn) where
  applySortVarSubst = append

instance Language sn el => ApplySortVarSubst sn (RuleSortVarSubst (Sort sn)) (RuleSortVarSubst (Sort sn)) where
  applySortVarSubst sigma (RuleSortVarSubst m) = RuleSortVarSubst $ m <#> applySortVarSubst sigma

instance Language sn el => ApplySortVarSubst sn (AnnExprNode sn el er) (AnnExprNode sn el er) where
  applySortVarSubst sigma (EN label sigma' er) = EN label (applySortVarSubst sigma sigma') er

instance Language sn el => ApplySortVarSubst sn (AnnExpr sn el er) (AnnExpr sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprTooth sn el er) (AnnExprTooth sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprPath sn el er) (AnnExprPath sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)

instance Language sn el => ApplySortVarSubst sn (AnnExprNonEmptyPath sn el er) (AnnExprNonEmptyPath sn el er) where
  applySortVarSubst sigma = map (applySortVarSubst sigma)
