-- | Language features handled generically:
-- |   - holes
-- |   - symbol terms, with their value reflected in their sort
module Pantograph.Generic.Language where

import Prelude

import Bug (bug)
import Data.Derivative (class Derivative, differentiate, integrate)
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Safe.Coerce (coerce)
import Text.Pretty (class Pretty, class Pretty1, class PrettyS1, angles, braces2, pretty, pretty1, prettyS1, quotes)
import Type.Proxy (Proxy(..))

-- TODO: sophisticate
diff :: forall rule joint tooth. IsLanguage rule joint tooth => HoleSort joint -> HoleSort joint -> HoleChange joint tooth
diff sort1 sort2 = Fix $ Replace sort1 sort2

newtype RuleVarSubst a = RuleVarSubst (Map.Map RuleVar a)

substRuleSort :: forall joint. IsJoint joint => RuleVarSubst (HoleSort joint) -> RuleSort joint -> HoleSort joint
substRuleSort sigma (Fix (InjectSortJoint (RuleVar ruleVar))) = case substRuleVar sigma ruleVar of
  Nothing -> bug $ "[substRuleSort] A RuleVar was not substituted: " <> pretty ruleVar
  Just sort -> sort
substRuleSort sigma (Fix (InjectSortJoint (InjectRuleJoint j))) = Fix (InjectSortJoint (InjectHoleJoint (substRuleSort sigma <$> j)))
substRuleSort sigma (Fix (SomeSymbol sort)) = Fix (SomeSymbol (substRuleSort sigma sort))
substRuleSort _sigma (Fix (Symbol str)) = Fix (Symbol str)

substRuleVar :: forall a. RuleVarSubst a -> RuleVar -> Maybe a
substRuleVar (RuleVarSubst sigma) ruleVar = Map.lookup ruleVar sigma

defaultChangeRule :: forall (rule ∷ Type) (tooth ∷ Type -> Type) (joint ∷ Type -> Type). IsLanguage rule joint tooth ⇒ rule -> ChangeRule joint tooth
defaultChangeRule rule = 
  let ProductionRule production = productionRule rule in
  ChangeRule
    { parameters: Set.empty
    , kidChanges: production.kidSorts <#> \kidSort ->
        let sigma = freshRuleVarSubst production.parameters in
        diff (substRuleSort sigma kidSort) (substRuleSort sigma production.parentSort) }

-- Fix

-- | The fixpoint of a 1-argument type constructor.
data Fix (f :: Type -> Type) = Fix (f (Fix f))

-- | Maps over each layer of the fixpoint.
mapFix :: forall f f'. Functor f => (forall a. f a -> f' a) -> Fix f -> Fix f'
mapFix f (Fix ff) = Fix (f (map (mapFix f) ff))

-- | Traverses over each layer of the fixpoint.
traverseFix :: forall f g m. Traversable f => Monad m => (forall a. f a -> m (g a)) -> Fix f -> m (Fix g)
traverseFix f (Fix ff) = Fix <$> join (f <$> (traverse (traverseFix f) ff))

-- | Unwrap the `Fix` wrapper.
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix ff) = ff

instance Pretty1 f => Pretty (Fix f) where
  pretty (Fix f) = pretty1 f

-- IsJoint, IsTooth

class 
  ( Functor joint, Foldable joint, Traversable joint, Pretty1 joint )
  <= IsJoint joint

class
  ( Functor tooth, Foldable tooth, Traversable tooth, PrettyS1 tooth )
  <= IsTooth tooth

-- HoleJoint

data HoleVar = MakeHoleVar String UUID

derive instance Generic HoleVar _
instance Show HoleVar where show = genericShow
instance Pretty HoleVar where pretty (MakeHoleVar label uuid) = label <> "@" <> String.take 2 (UUID.toString uuid)

freshHoleVar :: String -> HoleVar
freshHoleVar label = unsafePerformEffect $
  MakeHoleVar label <$> UUID.genUUID

data HoleJoint (joint :: Type -> Type) a 
  = Hole HoleVar
  | InjectHoleJoint (joint a)

data HoleTooth (tooth :: Type -> Type) a
  = InjectHoleTooth (tooth a)

instance Subtype (joint a) (HoleJoint joint a) where inject = InjectHoleJoint
instance Subtype (tooth a) (HoleTooth tooth a) where inject = InjectHoleTooth

instance Pretty1 joint => Pretty1 (HoleJoint joint) where
  pretty1 (Hole holeVar) = pretty holeVar
  pretty1 (InjectHoleJoint j) = pretty1 j
derive instance Functor joint => Functor (HoleJoint joint)
derive instance Foldable joint => Foldable (HoleJoint joint)
derive instance Traversable joint => Traversable (HoleJoint joint)

instance PrettyS1 j => PrettyS1 (HoleTooth j) where
  prettyS1 (InjectHoleTooth j) = prettyS1 j
instance PrettyS1 tooth => Pretty1 (HoleTooth tooth) where pretty1 j = prettyS1 j "⌶"
derive instance Functor tooth => Functor (HoleTooth tooth)
derive instance Foldable tooth => Foldable (HoleTooth tooth)
derive instance Traversable tooth => Traversable (HoleTooth tooth)

instance Derivative joint tooth => Derivative (HoleJoint joint) (HoleTooth tooth) where
  differentiate (Hole holeVar) = Hole holeVar
  differentiate (InjectHoleJoint j) = InjectHoleJoint $ map (map inject) $ differentiate j

  integrate a (InjectHoleTooth j) = InjectHoleJoint $ integrate a j

-- RuleJoint

newtype RuleVar = MakeRuleVar String

derive newtype instance Eq RuleVar
derive newtype instance Ord RuleVar

instance Pretty RuleVar where
  pretty (MakeRuleVar str) = angles str

data RuleJoint (joint :: Type -> Type) a 
  = RuleVar RuleVar
  | InjectRuleJoint (joint a)

data RuleTooth (tooth :: Type -> Type) a
  = InjectRuleTooth (tooth a)

instance Subtype (joint a) (RuleJoint joint a) where inject = InjectRuleJoint
instance Subtype (tooth a) (RuleTooth tooth a) where inject = InjectRuleTooth

instance Pretty1 joint => Pretty1 (RuleJoint joint) where
  pretty1 (RuleVar ruleVar) = pretty ruleVar
  pretty1 (InjectRuleJoint j) = pretty1 j
derive instance Functor joint => Functor (RuleJoint joint)
derive instance Foldable joint => Foldable (RuleJoint joint)
derive instance Traversable joint => Traversable (RuleJoint joint)
instance IsJoint joint => IsJoint (RuleJoint joint)

instance Pretty1 tooth => Pretty1 (RuleTooth tooth) where
  pretty1 (InjectRuleTooth j) = pretty1 j
derive instance Functor tooth => Functor (RuleTooth tooth)
derive instance Foldable tooth => Foldable (RuleTooth tooth)
derive instance Traversable tooth => Traversable (RuleTooth tooth)

instance Derivative joint tooth => Derivative (RuleJoint joint) (RuleTooth tooth) where
  differentiate (RuleVar ruleVar) = RuleVar ruleVar
  differentiate (InjectRuleJoint j) = InjectRuleJoint $ map (map inject) $ differentiate j
  
  integrate a (InjectRuleTooth j) = InjectRuleJoint $ integrate a j

-- | A `Sort` is basically a type for expressions in the language. `Sort` also
-- | supports __name__ and __string__ sorts, where `Name` expects one child that
-- | is a `String <String>`, which reflects the literal name into the sort
-- | system.

type Sort joint = Fix (SortJoint joint)
type HoleSort joint = Sort (HoleJoint joint)
type RuleSort joint = Sort (RuleJoint joint)

data SortJoint joint a
  = InjectSortJoint (joint a)
  | SomeSymbol a
  | Symbol String

data SortTooth (tooth :: Type -> Type) a
  = InjectSortTooth (tooth a)
  | SomeSymbol'

instance Subtype (joint a) (SortJoint joint a) where inject = InjectSortJoint
instance Subtype (joint a) (SortTooth joint a) where inject = InjectSortTooth

instance Pretty1 joint => Pretty1 (SortJoint joint) where
  pretty1 (InjectSortJoint j) = pretty1 j
  pretty1 (SomeSymbol a) = "Symbol:" <> pretty a
  pretty1 (Symbol str) = quotes str
derive instance Functor joint => Functor (SortJoint joint)
derive instance Foldable joint => Foldable (SortJoint joint)
derive instance Traversable joint => Traversable (SortJoint joint)
instance IsJoint joint => IsJoint (SortJoint joint)

instance PrettyS1 tooth => PrettyS1 (SortTooth tooth) where
  prettyS1 (InjectSortTooth j) str = prettyS1 j str
  prettyS1 SomeSymbol' str = "Symbol:" <> str
instance PrettyS1 tooth => Pretty1 (SortTooth tooth) where pretty1 a = prettyS1 a "⌶"
derive instance Functor tooth => Functor (SortTooth tooth)
derive instance Foldable tooth => Foldable (SortTooth tooth)
derive instance Traversable tooth => Traversable (SortTooth tooth)
instance IsTooth tooth => IsTooth (SortTooth tooth)

instance Derivative joint tooth => Derivative (SortJoint joint) (SortTooth tooth) where
  differentiate (InjectSortJoint j) = InjectSortJoint $ map (map inject) $ differentiate j
  differentiate (SomeSymbol a) = SomeSymbol (a /\ SomeSymbol')
  differentiate (Symbol str) = Symbol str

  integrate a (InjectSortTooth j) = InjectSortJoint $ integrate a j
  integrate a SomeSymbol' = SomeSymbol a

-- Path

data PathDir = UpPathDir | DownPathDir

type UpPathDir = "UpPathDir"
type DownPathDir = "DownPathDir"

class ReflectPathDir (dir :: Symbol) where reflectPathDir :: Proxy dir -> PathDir
instance ReflectPathDir UpPathDir where reflectPathDir _ = UpPathDir
instance ReflectPathDir DownPathDir where reflectPathDir _ = DownPathDir

class ReversePathDir (dir :: Symbol) (dir' :: Symbol)
instance ReversePathDir UpPathDir DownPathDir
instance ReversePathDir DownPathDir UpPathDir

newtype Path (dir :: Symbol) (tooth :: Type -> Type) a = Path (List (tooth a))

type HoleExprPath dir rule joint tooth = Path dir (HoleExprTooth rule joint tooth) (HoleExpr rule joint)

instance PrettyS1 tooth => PrettyS1 (Path UpPathDir tooth) where prettyS1 path = prettyS1 $ toDownPath path
instance PrettyS1 tooth => Pretty1 (Path UpPathDir tooth) where pretty1 path = prettyS1 path "⌶"

instance PrettyS1 tooth => PrettyS1 (Path DownPathDir tooth) where
  prettyS1 (Path ths) = case ths of
    Nil -> identity
    Cons th ths' -> prettyS1 th >>> prettyS1 (Path ths' :: Path DownPathDir tooth _)
instance PrettyS1 tooth => Pretty1 (Path DownPathDir tooth) where pretty1 path = prettyS1 path "⌶"

derive instance Foldable tooth => Foldable (Path dir tooth)
derive instance Functor tooth => Functor (Path dir tooth)
instance Semigroup (Path dir tooth a) where append (Path ths1) (Path ths2) = Path (ths1 <> ths2)
instance Monoid (Path dir tooth a) where mempty = Path mempty

data SomePath tooth a 
  = UpPath (Path UpPathDir tooth a) 
  | DownPath (Path DownPathDir tooth a)

type SomeHoleExprPath rule joint tooth = SomePath (HoleExprTooth rule joint tooth) (HoleExpr rule joint)

instance PrettyS1 tooth => PrettyS1 (SomePath tooth) where
  prettyS1 = case _ of 
    UpPath path -> prettyS1 path
    DownPath path -> prettyS1 path

consPath th (Path ths) = Path (th : ths)

appendPath :: forall dir tooth a. Path dir tooth a -> Path dir tooth a -> Path dir tooth a
appendPath (Path ths1) (Path ths2) = Path (ths1 <> ths2)

pathDir :: forall dir tooth a. Path dir tooth a -> Proxy dir
pathDir _ = Proxy

toUpPath :: forall dir tooth a. ReflectPathDir dir => Path dir tooth a -> Path UpPathDir tooth a
toUpPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> coerce path :: Path UpPathDir tooth a
  DownPathDir -> reversePath (coerce path :: Path DownPathDir tooth a)

toSomeUpPath :: forall tooth a. SomePath tooth a -> Path UpPathDir tooth a
toSomeUpPath = case _ of
  UpPath path -> toUpPath path
  DownPath path -> toUpPath path

toDownPath :: forall dir tooth a. ReflectPathDir dir => Path dir tooth a -> Path DownPathDir tooth a
toDownPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> reversePath (coerce path :: Path UpPathDir tooth a)
  DownPathDir -> coerce path :: Path DownPathDir tooth a

reversePath :: forall dir dir' tooth a. ReflectPathDir dir => ReversePathDir dir dir' => Path dir tooth a -> Path dir' tooth a
reversePath (Path ths) = Path (List.reverse ths)

-- Change

type Change joint tooth = Fix (ChangeJoint joint tooth)
type HoleChange joint tooth = Fix (HoleChangeJoint joint tooth)

type HoleChangeJoint joint tooth = ChangeJoint (HoleJoint joint) (HoleTooth tooth)

data ChangeJoint joint (tooth :: Type -> Type) a
  = Plus (SortTooth tooth (Sort joint)) a
  | Minus (SortTooth tooth (Sort joint)) a
  | Replace (Sort joint) (Sort joint)
  | InjectChangeJoint (SortJoint joint a)

instance Subtype (SortJoint joint a) (ChangeJoint joint tooth a) where inject = InjectChangeJoint
instance (Pretty1 joint, PrettyS1 tooth) => Pretty1 (ChangeJoint joint tooth) where
  pretty1 (Plus j' a) = "+" <> prettyS1 j' (pretty a)
  pretty1 (Minus j' a) = "-" <> prettyS1 j' (pretty a)
  pretty1 (Replace s1 s2) = pretty s1 <> " ~~> " <> pretty s2
  pretty1 (InjectChangeJoint j) = pretty1 j
derive instance (Functor joint, Functor tooth) => Functor (ChangeJoint joint tooth)
derive instance (Foldable joint, Foldable tooth) => Foldable (ChangeJoint joint tooth)
derive instance (Traversable joint, Traversable tooth) => Traversable (ChangeJoint joint tooth)

instance (IsJoint joint, IsTooth tooth) => IsJoint (ChangeJoint joint tooth)

idChange :: forall rule joint tooth. IsLanguage rule joint tooth => Sort joint -> Change joint tooth
idChange = case _ of
  Fix (InjectSortJoint j) -> Fix $ InjectChangeJoint (InjectSortJoint (idChange <$> j))
  Fix (SomeSymbol sort) -> Fix $ InjectChangeJoint (SomeSymbol (idChange sort))
  Fix (Symbol str) -> Fix $ InjectChangeJoint (Symbol str)

invertChange :: forall rule joint tooth. IsLanguage rule joint tooth => Change joint tooth -> Change joint tooth
invertChange = mapFix case _ of
  Plus th a -> Minus th a
  Minus th a -> Plus th a
  Replace a a' -> Replace a' a
  InjectChangeJoint a -> InjectChangeJoint a

changeEndpoints :: forall rule joint tooth. IsLanguage rule joint tooth => Change joint tooth -> {before :: Sort joint, after :: Sort joint}
changeEndpoints = unFix >>> case _ of
  Plus th a -> do
    let {before, after} = changeEndpoints a
    {before, after: Fix (integrate after th)}
  Minus th a -> do
    let {before, after} = changeEndpoints a
    {before: Fix (integrate before th), after}
  Replace before after -> {before, after}
  InjectChangeJoint a -> do
    let zippedKids = map changeEndpoints a
    let beforeKids = _.before <$> zippedKids
    let afterKids = _.after <$> zippedKids
    {before: Fix beforeKids, after: Fix afterKids}

-- Expr

data ExprJoint rule joint a 
  = Expr rule (RuleVarSubst (Sort joint)) (joint a)
  | SymbolExpr String

type Expr rule joint = Fix (ExprJoint rule joint)

type HoleExprJoint rule joint = ExprJoint rule (HoleJoint joint)
type HoleExpr rule joint = Fix (HoleExprJoint rule joint)

data ExprTooth rule joint (tooth :: Type -> Type) a = Expr' rule (RuleVarSubst (Sort joint)) (tooth a)
type HoleExprTooth rule joint tooth = ExprTooth rule (HoleJoint joint) (HoleTooth tooth)

instance Pretty1 joint => Pretty1 (ExprJoint rule joint) where
  pretty1 (Expr _rule _sigma j) = pretty1 j
  pretty1 (SymbolExpr str) = quotes str
derive instance Functor joint => Functor (ExprJoint rule joint)
derive instance Foldable joint => Foldable (ExprJoint rule joint)
derive instance Traversable joint => Traversable (ExprJoint rule joint)
instance IsJoint joint => IsJoint (ExprJoint rule joint)

instance PrettyS1 tooth => PrettyS1 (ExprTooth rule joint tooth) where
  prettyS1 (Expr' _rule _sigma j') str = prettyS1 j' str
instance PrettyS1 tooth => Pretty1 (ExprTooth rule joint tooth) where pretty1 j = prettyS1 j "⌶"
derive instance Functor tooth => Functor (ExprTooth rule joint tooth)
derive instance Foldable tooth => Foldable (ExprTooth rule joint tooth)

instance Derivative joint tooth => Derivative (ExprJoint rule joint) (ExprTooth rule joint tooth) where
  differentiate (Expr rule sigma j) = Expr rule sigma $ map (map (Expr' rule sigma)) $ differentiate j
  differentiate (SymbolExpr str) = SymbolExpr str
  
  integrate a (Expr' rule sigma j') = Expr rule sigma $ integrate a j'

-- Cursor

data Cursor tooth a = Cursor (Path UpPathDir tooth a) a
type HoleExprCursor rule joint tooth = Cursor (HoleExprTooth rule joint tooth) (HoleExpr rule joint)

instance PrettyS1 tooth => Pretty1 (Cursor tooth) where
  pretty1 (Cursor path a) = prettyS1 path $ braces2 $ pretty a

-- Select

data Select tooth a = Select (Path UpPathDir tooth a) (SomePath tooth a) a
type HoleExprSelect rule joint tooth = Select (HoleExprTooth rule joint tooth) (HoleExpr rule joint)

instance PrettyS1 tooth => Pretty1 (Select tooth) where
  pretty1 (Select up mid a) = prettyS1 up $ braces2 $ prettyS1 mid $ braces2 $ pretty a

-- Language

class
    ( Pretty rule
    , IsJoint joint
    , PrettyS1 tooth
    , Derivative joint tooth )
    <= IsLanguage rule joint tooth 
    | joint -> rule tooth, rule -> joint tooth, tooth -> joint rule
  where
  -- | The production rules, indexed by `rule`.
  productionRule :: rule -> ProductionRule joint
  -- | The change rules, indexed by `rule`.
  changeRule :: rule -> ChangeRule joint tooth
  -- | The default term (if any) for a sort.
  defaultExpr :: HoleSort joint -> Maybe (HoleExpr rule joint)
  -- | When a change is yielded at a cursor, split it into a change to propogate
  -- | downwards, and change to propogate upwards, and a new sort at the cursor.
  splitChange :: {change :: Change joint tooth, sort :: Sort joint} -> {down :: Change joint tooth, up :: Change joint tooth, sort :: Sort joint}
  -- | Defines if a cursor is valid as a function of its sort.
  validCursorSort :: HoleSort joint -> Boolean
  -- | Defines if a selection is valid as a function of its top and bot sorts.
  validSelectionSorts :: {top :: HoleSort joint, bot :: HoleSort joint} -> Boolean

  -- TODO: are these necessary?
  -- -- | The change propogated upwards when the term is deleted.
  -- digChange :: HoleSort joint -> Change joint tooth
  -- -- | TODO: DOC
  -- generalize :: HoleSort joint -> Change joint tooth
  -- -- | TODO: DOC
  -- specialize :: {general :: HoleSort joint, special :: HoleSort joint} -> Change joint tooth

newtype ProductionRule joint = ProductionRule
  { parameters :: Set.Set RuleVar
  , kidSorts :: HoleJoint joint (RuleSort joint)
  , parentSort :: RuleSort joint }

freshRuleVarSubst :: forall joint. Set.Set RuleVar -> RuleVarSubst (HoleSort joint)
freshRuleVarSubst = RuleVarSubst <<< mapWithIndex (\(MakeRuleVar str) _ -> Fix $ InjectSortJoint $ Hole $ freshHoleVar str) <<< Set.toMap

-- TODO: DOC
newtype ChangeRule joint tooth = ChangeRule
  { parameters :: Set.Set RuleVar
  , kidChanges :: HoleJoint joint (HoleChange joint tooth) }

class Zippable a where
  zip :: a -> Maybe a
  unzip :: a -> Array a

-- | `Subtype a b` if every term of `a` can be mapped to a unique term of `b`.
class Subtype a b where inject :: a -> b

inject' :: forall a b. Subtype a b => Proxy a -> a -> b
inject' _ = inject
