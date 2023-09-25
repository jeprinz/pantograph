-- | Language features handled generically:
-- |   - holes
-- |   - label terms, with their value reflected as strings in their sort
module Pantograph.Generic.Language where

import Prelude

import Data.Derivative (class Derivative, differentiate, integrate)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultR)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Hole (hole)
import Safe.Coerce (coerce)
import Text.Pretty (class Pretty, class Pretty1)
import Type.Proxy (Proxy(..))

instantiateSort :: forall joint. RuleSort joint -> Sort joint
instantiateSort = hole "TODO: instantiateSort"

diff :: forall rule joint joint'. IsLanguage rule joint joint' => RuleSort joint -> RuleSort joint -> Change joint joint'
diff = hole "TODO: diff"

defaultChangeRule :: forall (rule ∷ Type) (joint' ∷ Type -> Type) (joint ∷ Type -> Type). IsLanguage rule joint joint' ⇒ rule -> ChangeRule joint joint'
defaultChangeRule rule = 
  let ProductionRule production = productionRule rule in
  ChangeRule
    { quantifiers: Set.empty
    , kidChanges: production.kidSorts <#> \kidSort -> diff kidSort production.parentSort }

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

class 
  ( Functor joint, Foldable joint, Traversable joint, Pretty1 joint )
  <= IsJoint joint

-- HoleJoint

data HoleVar = MakeHoleVar String UUID

derive instance Generic HoleVar _
instance Show HoleVar where show = genericShow
instance Pretty HoleVar where pretty (MakeHoleVar label uuid) = label <> "@" <> UUID.toString uuid

freshHoleVar :: String -> HoleVar
freshHoleVar label = unsafePerformEffect $
  MakeHoleVar label <$> UUID.genUUID

data HoleJoint (joint :: Type -> Type) a 
  = Hole HoleVar
  | InjectHoleJoint (joint a)

data HoleJoint' (joint' :: Type -> Type) a
  = InjectHoleJoint' (joint' a)

instance Subtype (joint a) (HoleJoint joint a) where inject = InjectHoleJoint
instance Subtype (joint' a) (HoleJoint' joint' a) where inject = InjectHoleJoint'

instance Pretty1 joint => Pretty1 (HoleJoint joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (HoleJoint joint)
instance FunctorWithIndex Int joint  => FunctorWithIndex Int (HoleJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (HoleJoint joint)
derive instance Traversable joint => Traversable (HoleJoint joint)

instance Pretty1 joint' => Pretty1 (HoleJoint' joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (HoleJoint' joint')
instance FunctorWithIndex Int joint'  => FunctorWithIndex Int (HoleJoint' joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (HoleJoint' joint')
derive instance Traversable joint' => Traversable (HoleJoint' joint')

instance Derivative joint joint' => Derivative (HoleJoint joint) (HoleJoint joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

-- HoleJoint

newtype RuleVar = MakeRuleVar String

derive newtype instance Eq RuleVar
derive newtype instance Ord RuleVar

data RuleVarJoint (joint :: Type -> Type) a 
  = RuleVar RuleVar
  | InjectRuleVarJoint (joint a)

data RuleVarJoint' (joint' :: Type -> Type) a
  = InjectRuleVarJoint' (joint' a)

instance Subtype (joint a) (RuleVarJoint joint a) where inject = InjectRuleVarJoint
instance Subtype (joint' a) (RuleVarJoint' joint' a) where inject = InjectRuleVarJoint'

instance Pretty1 joint => Pretty1 (RuleVarJoint joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (RuleVarJoint joint)
instance FunctorWithIndex Int joint  => FunctorWithIndex Int (RuleVarJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (RuleVarJoint joint)
derive instance Traversable joint => Traversable (RuleVarJoint joint)
instance IsJoint joint => IsJoint (RuleVarJoint joint)

instance Pretty1 joint' => Pretty1 (RuleVarJoint' joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (RuleVarJoint' joint')
instance FunctorWithIndex Int joint'  => FunctorWithIndex Int (RuleVarJoint' joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (RuleVarJoint' joint')
derive instance Traversable joint' => Traversable (RuleVarJoint' joint')

instance Derivative joint joint' => Derivative (RuleVarJoint joint) (RuleVarJoint joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

-- | A `Sort` is basically a type for expressions in the language. `Sort` also
-- | supports __name__ and __string__ sorts, where `Name` expects one child that
-- | is a `String <String>`, which reflects the literal name into the sort
-- | system.

type Sort joint = Fix (SortJoint joint)
type HoleSort joint = Sort (HoleJoint joint)
type RuleSort joint = Sort (RuleVarJoint joint)

data SortJoint joint a
  = InjectSortJoint (joint a)
  | SomeSymbol a
  | Symbol String

data SortJoint' (joint' :: Type -> Type) a
  = InjectSortJoint' (joint' a)

instance Subtype (joint a) (SortJoint joint a) where inject = InjectSortJoint

instance Pretty1 joint => Pretty1 (SortJoint joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (SortJoint joint)
instance FunctorWithIndex Int joint => FunctorWithIndex Int (SortJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (SortJoint joint)
derive instance Traversable joint => Traversable (SortJoint joint)
instance IsJoint joint => IsJoint (SortJoint joint)

instance Pretty1 joint' => Pretty1 (SortJoint' joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (SortJoint' joint')
instance FunctorWithIndex Int joint' => FunctorWithIndex Int (SortJoint' joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (SortJoint' joint')
derive instance Traversable joint' => Traversable (SortJoint' joint')

instance Derivative joint joint' => Derivative (SortJoint joint) (SortJoint' joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

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

newtype Path (dir :: Symbol) (joint' :: Type -> Type) a = Path (List (joint' a))

type HoleExprPath dir rule joint joint' = Path dir (HoleExprJoint' rule joint joint') (HoleExpr rule joint)

derive instance Foldable joint' => Foldable (Path dir joint')
derive instance Functor joint' => Functor (Path dir joint')
instance Semigroup (Path dir joint' a) where append (Path ths1) (Path ths2) = Path (ths1 <> ths2)
instance Monoid (Path dir joint' a) where mempty = Path mempty

data SomePath joint' a 
  = UpPath (Path UpPathDir joint' a) 
  | DownPath (Path DownPathDir joint' a)

type SomeHoleExprPath rule joint joint' = SomePath (HoleExprJoint' rule joint joint') (HoleExpr rule joint)

pathDir :: forall dir joint' a. Path dir joint' a -> Proxy dir
pathDir _ = Proxy

toUpPath :: forall dir joint' a. ReflectPathDir dir => Path dir joint' a -> Path UpPathDir joint' a
toUpPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> coerce path :: Path UpPathDir joint' a
  DownPathDir -> reversePath (coerce path :: Path DownPathDir joint' a)

toDownPath :: forall dir joint' a. ReflectPathDir dir => Path dir joint' a -> Path DownPathDir joint' a
toDownPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> reversePath (coerce path :: Path UpPathDir joint' a)
  DownPathDir -> coerce path :: Path DownPathDir joint' a

reversePath :: forall dir dir' joint' a. ReflectPathDir dir => ReversePathDir dir dir' => Path dir joint' a -> Path dir' joint' a
reversePath (Path ths) = Path (List.reverse ths)

-- Change

type Change joint joint' = Fix (ChangeJoint joint joint')

data ChangeJoint joint (joint' :: Type -> Type) a
  = Plus (SortJoint' joint' (Sort joint)) a
  | Minus (SortJoint' joint' (Sort joint)) a
  | Replace (Sort joint) (Sort joint)
  | InjectChangeJoint (SortJoint joint a)

instance Subtype (SortJoint joint a) (ChangeJoint joint joint' a) where inject = InjectChangeJoint
instance Pretty1 joint => Pretty1 (ChangeJoint joint joint') where pretty1 _ = hole "TODO"
derive instance (Functor joint, Functor joint') => Functor (ChangeJoint joint joint')
instance (FunctorWithIndex Int joint, FunctorWithIndex Int joint') => FunctorWithIndex Int (ChangeJoint joint joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance (Foldable joint, Foldable joint') => Foldable (ChangeJoint joint joint')
derive instance (Traversable joint, Traversable joint') => Traversable (ChangeJoint joint joint')

instance (IsJoint joint, Functor joint', FunctorWithIndex Int joint', Foldable joint', Traversable joint') => IsJoint (ChangeJoint joint joint')

invertChange :: forall rule joint joint'. IsLanguage rule joint joint' => Change joint joint' -> Change joint joint'
invertChange = mapFix case _ of
  Plus th a -> Minus th a
  Minus th a -> Plus th a
  Replace a a' -> Replace a' a
  InjectChangeJoint a -> InjectChangeJoint a

changeEndpoints :: forall rule joint joint'. IsLanguage rule joint joint' => Change joint joint' -> {before :: Sort joint, after :: Sort joint}
changeEndpoints = unFix >>> case _ of
  -- Plus th a -> do
  --   let {before, after} = changeEndpoints a
  --   {before, after: Fix (integrate after th)}
  -- Minus th a -> do
  --   let {before, after} = changeEndpoints a
  --   {before: Fix (integrate before th), after}
  -- Replace before after -> {before, after}
  -- InjectChangeJoint a -> do
  --   let zippedKids = map changeEndpoints a
  --   let beforeKids = _.before <$> zippedKids
  --   let afterKids = _.after <$> zippedKids
  --   {before: Fix beforeKids, after: Fix afterKids}
  _ -> hole "TODO: changeEndpoints"

-- Expr


data ExprJoint rule joint a 
  = Expr rule (Subst RuleVar (Sort joint)) (joint a)
  | SymbolExpr String

type Expr rule joint = Fix (ExprJoint rule joint)

type HoleExprJoint rule joint = ExprJoint rule (HoleJoint joint)
type HoleExpr rule joint = Fix (HoleExprJoint rule joint)

data ExprJoint' rule joint (joint' :: Type -> Type) a = Expr' rule (Subst RuleVar (Sort joint)) (HoleJoint' joint' a)
type HoleExprJoint' rule joint joint' = ExprJoint' rule (HoleJoint joint) (HoleJoint joint')

instance Pretty1 joint => Pretty1 (ExprJoint rule joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (ExprJoint rule joint)
instance FunctorWithIndex Int joint => FunctorWithIndex Int (ExprJoint rule joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (ExprJoint rule joint)
derive instance Traversable joint => Traversable (ExprJoint rule joint)
instance IsJoint joint => IsJoint (ExprJoint rule joint)

instance Pretty1 joint' => Pretty1 (ExprJoint' rule joint joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (ExprJoint' rule joint joint')
instance FunctorWithIndex Int joint' => FunctorWithIndex Int (ExprJoint' rule joint joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (ExprJoint' rule joint joint')

instance Derivative joint joint' => Derivative (ExprJoint rule joint) (ExprJoint' rule joint joint') where
  differentiate _ = hole "TODO"
  integrate _ = hole "TODO"


-- Cursor

data Cursor joint' a = Cursor (Path UpPathDir joint' a) a
type HoleExprCursor rule joint joint' = Cursor (HoleExprJoint' rule joint joint') (HoleExpr rule joint)

-- Select

data Select joint' a = Select (Path UpPathDir joint' a) (SomePath joint' a) a
type HoleExprSelect rule joint joint' = Select (HoleExprJoint' rule joint joint') (HoleExpr rule joint)

-- Language

class
    ( Pretty rule
    , IsJoint joint
    , Pretty1 joint'
    , Derivative joint joint' )
    <= IsLanguage rule joint joint' 
    | joint -> rule joint', rule -> joint joint', joint' -> joint rule
  where
  -- | The production rules, indexed by `rule`.
  productionRule :: rule -> ProductionRule joint
  -- | The change rules, indexed by `rule`.
  changeRule :: rule -> ChangeRule joint joint'
  -- | The default term (if any) for a sort.
  defaultExpr :: HoleSort joint -> Maybe (HoleExpr rule joint)
  -- | When a change is yielded at a cursor, split it into a change to propogate
  -- | downwards, and change to propogate upwards, and a new sort at the cursor.
  splitChange :: {change :: Change joint joint', sort :: Sort joint} -> {down :: Change joint joint', up :: Change joint joint', sort :: Sort joint}
  -- | Defines if a cursor is valid as a function of its sort.
  validCursorSort :: HoleSort joint -> Boolean
  -- | Defines if a selection is valid as a function of its top and bot sorts.
  validSelectionSorts :: {top :: HoleSort joint, bot :: HoleSort joint} -> Boolean
  -- | The change propogated upwards when the term is deleted.
  digChange :: HoleSort joint -> Change joint joint'
  -- | TODO: DOC
  generalize :: HoleSort joint -> Change joint joint'
  -- | TODO: DOC
  specialize :: {general :: HoleSort joint, special :: HoleSort joint} -> Change joint joint'

newtype ProductionRule joint = ProductionRule
  { quantifiers :: Set.Set RuleVar
  , kidSorts :: joint (RuleSort joint)
  , parentSort :: RuleSort joint }

-- TODO: DOC
newtype ChangeRule joint (joint' :: Type -> Type) = ChangeRule
  { quantifiers :: Set.Set RuleVar
  , kidChanges :: joint (Change joint joint') }

-- Misc

newtype Subst k v = Subst (Map.Map k v)

-- data Nat = Zero | Suc Nat
-- derive instance Eq Nat
-- derive instance Ord Nat

class Zippable a where
  zip :: a -> Maybe a
  unzip :: a -> Array a

-- | `Subtype a b` if every term of `a` can be mapped to a unique term of `b`.
class Subtype a b where inject :: a -> b

inject' :: forall a b. Subtype a b => Proxy a -> a -> b
inject' _ = inject
