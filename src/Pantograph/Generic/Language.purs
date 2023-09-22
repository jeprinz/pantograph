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
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Hole (hole)
import Safe.Coerce (coerce)
import Text.Pretty (class Pretty, class Pretty1)
import Type.Proxy (Proxy(..))

-- Fix

-- | The fixpoint of a 1-argument type constructor.
data Fix (f :: Type -> Type) = Fix (f (Fix f))

-- | Maps over each layer of the fixpoint.
mapFix :: forall f. Functor f => (forall a. f a -> f a) -> Fix f -> Fix f
mapFix f (Fix ff) = Fix (f (map (mapFix f) ff))

-- | Unwrap the `Fix` wrapper.
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix ff) = ff

-- | A language's __joint__ type is the container type of the children of an
-- | expression.
class 
  ( Functor joint, FunctorWithIndex Int joint, Foldable joint, Pretty1 joint )
  <= IsJoint joint

-- MetaVar

-- | A `MetaVar` is a meta-level variable that can appear in `Sort`s.
data MetaVar = MetaVar UUID (Maybe String)

derive instance Generic MetaVar _
instance Show MetaVar where show = genericShow
instance Pretty MetaVar where pretty (MetaVar uuid mb_label) = maybe "" (_ <> "@") mb_label <> UUID.toString uuid

freshMetaVar :: Maybe String -> MetaVar
freshMetaVar label = unsafePerformEffect $
  MetaVar <$> UUID.genUUID <*> pure label

data MetaJoint (joint :: Type -> Type) a 
  = Meta MetaVar
  | InjectMetaJoint (joint a)

data MetaJoint' (joint' :: Type -> Type) a
  = InjectMetaJoint' (joint' a)

instance Subtype (joint a) (MetaJoint joint a) where inject = InjectMetaJoint
instance Subtype (joint' a) (MetaJoint' joint' a) where inject = InjectMetaJoint'

instance Pretty1 joint => Pretty1 (MetaJoint joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (MetaJoint joint)
instance FunctorWithIndex Int joint  => FunctorWithIndex Int (MetaJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (MetaJoint joint)

instance Pretty1 joint' => Pretty1 (MetaJoint' joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (MetaJoint' joint')
instance FunctorWithIndex Int joint'  => FunctorWithIndex Int (MetaJoint' joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (MetaJoint' joint')

instance Derivative joint joint' => Derivative (MetaJoint joint) (MetaJoint joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

-- instance IsJoint joint joint' => IsJoint (MetaJoint joint) (MetaJoint' joint')

-- | A `Sort` is basically a type for expressions in the language. `Sort` also
-- | supports __name__ and __string__ sorts, where `Name` expects one child that
-- | is a `String <String>`, which reflects the literal name into the sort
-- | system.
type Sort joint = Fix (SortJoint joint)

data SortJoint joint a
  = InjectSortJoint (joint a)
  | MetaSort (MetaJoint joint a)
  | Name a
  | String String

data SortJoint' (joint' :: Type -> Type) a
  = InjectSortJoint' (joint' a)

instance Subtype (joint a) (SortJoint joint a) where inject = InjectSortJoint

instance Pretty1 joint => Pretty1 (SortJoint joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (SortJoint joint)
instance FunctorWithIndex Int joint => FunctorWithIndex Int (SortJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (SortJoint joint)

instance Pretty1 joint' => Pretty1 (SortJoint' joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (SortJoint' joint')
instance FunctorWithIndex Int joint' => FunctorWithIndex Int (SortJoint' joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (SortJoint' joint')

instance Derivative joint joint' => Derivative (SortJoint joint) (SortJoint' joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

instance IsJoint joint => IsJoint (SortJoint joint)

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

type TermPath dir rule joint joint' = Path dir (TermJoint' rule joint joint') (Term rule joint)

derive instance Foldable joint' => Foldable (Path dir joint')
derive instance Functor joint' => Functor (Path dir joint')
instance Semigroup (Path dir joint' a) where append (Path ths1) (Path ths2) = Path (ths1 <> ths2)
instance Monoid (Path dir joint' a) where mempty = Path mempty

data SomePath joint' a 
  = UpPath (Path UpPathDir joint' a) 
  | DownPath (Path DownPathDir joint' a)

type SomeTermPath rule joint joint' = SomePath (TermJoint' rule joint joint') (Term rule joint)

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

instance (IsJoint joint, Functor joint', FunctorWithIndex Int joint', Foldable joint') => IsJoint (ChangeJoint joint joint')

invertChange :: forall rule joint joint'. IsLanguage rule joint joint' => Change joint joint' -> Change joint joint'
invertChange = mapFix case _ of
  Plus th a -> Minus th a
  Minus th a -> Plus th a
  Replace a a' -> Replace a' a
  InjectChangeJoint a -> InjectChangeJoint a

changeEndpoints :: forall rule joint joint'. IsLanguage rule joint joint' => Change joint joint' -> {before :: Sort joint, after :: Sort joint}
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

-- Term

type Term rule joint = Fix (TermJoint rule joint)

data TermJoint rule joint a = Term rule (Substitution MetaVar (Sort joint)) (joint a)

data TermJoint' rule joint (joint' :: Type -> Type) a = Term' rule (Substitution MetaVar (Sort joint)) (joint' a)

instance Pretty1 joint => Pretty1 (TermJoint rule joint) where pretty1 _ = hole "TODO"
derive instance Functor joint => Functor (TermJoint rule joint)
instance FunctorWithIndex Int joint => FunctorWithIndex Int (TermJoint rule joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint => Foldable (TermJoint rule joint)

instance Pretty1 joint' => Pretty1 (TermJoint' rule joint joint') where pretty1 _ = hole "TODO"
derive instance Functor joint' => Functor (TermJoint' rule joint joint')
instance FunctorWithIndex Int joint' => FunctorWithIndex Int (TermJoint' rule joint joint') where
  mapWithIndex _ _ = hole "TODO"
derive instance Foldable joint' => Foldable (TermJoint' rule joint joint')

instance Derivative joint joint' => Derivative (TermJoint rule joint) (TermJoint' rule joint joint') where
  differentiate = hole "TODO"
  integrate = hole "TODO"

instance IsJoint joint => IsJoint (TermJoint rule joint)

-- Cursor

data Cursor joint' a = Cursor (Path UpPathDir joint' a) a
type TermCursor rule joint joint' = Cursor (TermJoint' rule joint joint') (Term rule joint)

-- Select

data Select joint' a = Select (Path UpPathDir joint' a) (SomePath joint' a) a
type TermSelect rule joint joint' = Select (TermJoint' rule joint joint') (Term rule joint)

-- Language

class
    ( Pretty rule
    , IsJoint joint
    , Derivative joint joint' )
    <= IsLanguage rule joint joint' 
    | joint -> rule joint', rule -> joint joint', joint' -> joint rule
  where
  -- | The production rules, indexed by `rule`.
  productionRules :: rule -> ProductionRule rule joint
  -- | The change rules, indexed by `rule`.
  changeRules :: rule -> ChangeRule rule joint joint'
  -- | The default term (if any) for a sort.
  defaultTerm :: Sort joint -> Maybe (Term rule joint)
  -- | When a change is yielded at a cursor, split it into a change to propogate
  -- | downwards, and change to propogate upwards, and a new sort at the cursor.
  splitChange :: Change joint joint' -> {down :: Change joint joint', up :: Change joint joint', sort :: Sort joint}
  -- | Defines if a cursor is valid as a function of its sort.
  validCursorSort :: Sort joint -> Boolean
  -- | Defines if a selection is valid as a function of its top and bottom sorts.
  validSelectionSorts :: {top :: Sort joint, bottom :: Sort joint} -> Boolean
  -- | The change propogated upwards when the term is deleted.
  digChange :: Sort joint -> Change joint joint'
  -- | TODO: DOC
  generalize :: Sort joint -> Change joint joint'
  -- | TODO: DOC
  specialize :: {general :: Sort joint, special :: Sort joint} -> Change joint joint'

newtype ProductionRule rule joint = ProductionRule
  { quantifiers :: Set.Set MetaVar
  , kidSorts :: TermJoint rule joint (Sort joint)
  , parentSort :: Sort joint }

-- TODO: DOC
newtype ChangeRule rule joint (joint' :: Type -> Type) = ChangeRule
  { quantifiers :: Set.Set MetaVar
  , kidChanges :: TermJoint rule joint (Fix (ChangeJoint joint joint')) }

-- Misc

newtype Substitution k v = Substitution (Map.Map k v)

-- data Nat = Zero | Suc Nat
-- derive instance Eq Nat
-- derive instance Ord Nat

class Zippable a where
  zip :: a -> Maybe a
  unzip :: a -> Array a

-- | `Subtype a b` if every term of `a` can be mapped to a unique term of `b`.
class Subtype a b where inject :: a -> b
