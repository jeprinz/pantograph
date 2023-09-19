module Pantograph.Generic.Language where

import Prelude

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
class (Functor joint, FunctorWithIndex Int joint, Foldable joint, FoldableWithIndex Int joint, Pretty1 joint) <= IsJoint joint

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

instance Subtype (joint a) (MetaJoint joint a) where inject = InjectMetaJoint

instance IsJoint joint => Pretty1 (MetaJoint joint) where pretty1 _ = hole "TODO"
derive instance IsJoint joint => Functor (MetaJoint joint)
instance IsJoint joint => FunctorWithIndex Int (MetaJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance IsJoint joint => Foldable (MetaJoint joint)
instance IsJoint joint => FoldableWithIndex Int (MetaJoint joint) where
  foldrWithIndex f b joint = snd $ foldr (\a (i /\ b') -> ((i + 1) /\ f i a b')) (0 /\ b) joint
  foldlWithIndex f b joint = snd $ foldl (\(i /\ b') a -> ((i + 1) /\ f i b' a)) (0 /\ b) joint
  foldMapWithIndex f joint = foldMapWithIndexDefaultR f joint
instance IsJoint joint => IsJoint (MetaJoint joint)

-- | A `Sort` is basically a type for expressions in the language. `Sort` also
-- | supports __name__ and __string__ sorts, where `Name` expects one child that
-- | is a `String <String>`, which reflects the literal name into the sort
-- | system.
type Sort joint = Fix (SortJoint (MetaJoint joint))

data SortJoint joint a
  = InjectSortJoint (joint a)
  | Name a
  | String String

instance Subtype (joint a) (SortJoint joint a) where inject = InjectSortJoint

instance IsJoint joint => Pretty1 (SortJoint joint) where pretty1 _ = hole "TODO"
derive instance IsJoint joint => Functor (SortJoint joint)
instance IsJoint joint => FunctorWithIndex Int (SortJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance IsJoint joint => Foldable (SortJoint joint)
instance IsJoint joint => FoldableWithIndex Int (SortJoint joint) where
  foldrWithIndex _ _ _ = hole "TODO"
  foldlWithIndex _ _ _ = hole "TODO"
  foldMapWithIndex _ _ = hole "TODO"
instance IsJoint joint => IsJoint (SortJoint joint)

-- The derivative of a 1-argument type constructor.
class Derivative f f' | f -> f' where
  differentiate :: forall x. f x -> f (f' x)
  integrate :: forall x. x -> f' x -> f x

-- | A `Tooth` is a `Term` with one kid missing -- exactly one kid should be
-- | `Nothing` and the rest should be `Some`. Unfortunately, this is not
-- | type-enforced.
data Tooth joint a = Tooth (joint (Maybe a))

derive instance IsJoint joint => Foldable (Tooth joint)
derive instance IsJoint joint => Functor (Tooth joint)

tooths :: forall joint a. IsJoint joint => joint a -> joint (Tooth joint a)
tooths joint = mapWithIndex (\i _ -> Tooth (mapWithIndex (\i' a' -> if i == i' then Nothing else Just a') joint)) joint

unTooth :: forall joint a. IsJoint joint => a -> Tooth joint a -> joint a
unTooth a (Tooth joint) = map (maybe a identity) joint

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

newtype Path (dir :: Symbol) joint a = Path (List (Tooth joint a))

derive instance IsJoint joint => Foldable (Path dir joint)
derive instance IsJoint joint => Functor (Path dir joint)
instance Semigroup (Path dir joint a) where append (Path ths1) (Path ths2) = Path (ths1 <> ths2)
instance Monoid (Path dir joint a) where mempty = Path mempty

pathDir :: forall dir joint a. Path dir joint a -> Proxy dir
pathDir _ = Proxy

toUpPath :: forall dir joint a. ReflectPathDir dir => Path dir joint a -> Path UpPathDir joint a
toUpPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> coerce path :: Path UpPathDir joint a
  DownPathDir -> reversePath (coerce path :: Path DownPathDir joint a)

toDownPath :: forall dir joint a. ReflectPathDir dir => Path dir joint a -> Path DownPathDir joint a
toDownPath path = case reflectPathDir (pathDir path) of
  UpPathDir -> reversePath (coerce path :: Path UpPathDir joint a)
  DownPathDir -> coerce path :: Path DownPathDir joint a

reversePath :: forall dir dir' joint a. ReflectPathDir dir => ReversePathDir dir dir' => Path dir joint a -> Path dir' joint a
reversePath (Path ths) = Path (List.reverse ths)

-- Change

type Change joint = Fix (ChangeJoint (SortJoint (MetaJoint joint)))

data ChangeJoint joint a
  = Plus (Tooth joint (Fix joint)) a
  | Minus (Tooth joint (Fix joint)) a
  | Replace (Fix joint) (Fix joint)
  | InjectChangeJoint (joint a)

instance Subtype (joint a) (ChangeJoint joint a) where inject = InjectChangeJoint
instance IsJoint joint => Pretty1 (ChangeJoint joint) where pretty1 _ = hole "TODO"
derive instance IsJoint joint => Functor (ChangeJoint joint)
instance IsJoint joint => FunctorWithIndex Int (ChangeJoint joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance IsJoint joint => Foldable (ChangeJoint joint)
instance IsJoint joint => FoldableWithIndex Int (ChangeJoint joint) where
  foldrWithIndex _ _ _ = hole "TODO"
  foldlWithIndex _ _ _ = hole "TODO"
  foldMapWithIndex _ _ = hole "TODO"
instance IsJoint joint => IsJoint (ChangeJoint joint)

invertChange :: forall ruleName joint. IsLanguage ruleName joint => Change joint -> Change joint
invertChange = mapFix case _ of
  Plus th a -> Minus th a
  Minus th a -> Plus th a
  Replace a a' -> Replace a' a
  InjectChangeJoint a -> InjectChangeJoint a

changeEndpoints :: forall joint. IsJoint joint => Change joint -> {left :: Sort joint, right :: Sort joint}
changeEndpoints = unFix >>> case _ of
  Plus th a -> do
    let {left, right} = changeEndpoints a
    {left, right: Fix (unTooth right th)}
  Minus th a -> do
    let {left, right} = changeEndpoints a
    {left: Fix (unTooth left th), right}
  Replace left right -> {left, right}
  InjectChangeJoint a -> do
    let zippedKids = map changeEndpoints a
    let leftKids = _.left <$> zippedKids
    let rightKids = _.right <$> zippedKids
    {left: Fix leftKids, right: Fix rightKids}

-- Term

type Term ruleName joint = Fix (TermJoint ruleName (MetaJoint joint))

data TermJoint ruleName joint a
  = Term ruleName (Sub MetaVar (Sort joint)) (joint a)
  | Literal String

instance IsJoint joint => Pretty1 (TermJoint ruleName joint) where pretty1 _ = hole "TODO"
derive instance IsJoint joint => Functor (TermJoint ruleName joint)
instance IsJoint joint => FunctorWithIndex Int (TermJoint ruleName joint) where
  mapWithIndex _ _ = hole "TODO"
derive instance IsJoint joint => Foldable (TermJoint ruleName joint)
instance IsJoint joint => FoldableWithIndex Int (TermJoint ruleName joint) where
  foldrWithIndex _ _ _ = hole "TODO"
  foldlWithIndex _ _ _ = hole "TODO"
  foldMapWithIndex _ _ = hole "TODO"
instance IsJoint joint => IsJoint (TermJoint ruleName joint)

-- TODO: does this go in `Editor` since its only relevant to the ui?

-- -- | A `HolyTerm` is a term 
-- type HolyTerm ruleName joint = Fix (HolyTermJoint ruleName (MetaJoint joint))

-- data HolyTermJoint ruleName joint a
--   = InjectHolyTermJoint (TermJoint ruleName joint a)
--   | HoleInterior (Sort joint)

-- instance Subtype (TermJoint ruleName joint a) (HolyTermJoint ruleName joint a) where inject = InjectHolyTermJoint

-- instance IsJoint joint => Pretty1 (HolyTermJoint ruleName joint) where pretty1 _ = hole "TODO"
-- derive instance IsJoint joint => Functor (HolyTermJoint ruleName joint)
-- instance IsJoint joint => FunctorWithIndex Int (HolyTermJoint ruleName joint) where
--   mapWithIndex _ _ = hole "TODO"
-- derive instance IsJoint joint => Foldable (HolyTermJoint ruleName joint)
-- instance IsJoint joint => FoldableWithIndex Int (HolyTermJoint ruleName joint) where
--   foldrWithIndex _ _ _ = hole "TODO"
--   foldlWithIndex _ _ _ = hole "TODO"
--   foldMapWithIndex _ _ = hole "TODO"
-- instance IsJoint joint => IsJoint (HolyTermJoint ruleName joint)

-- Cursor

data Cursor joint a = Cursor (Path UpPathDir joint a) a

-- Select

data Select joint dir a = Select (Path UpPathDir joint a) (Path dir joint a) a

-- Language

class
    ( Pretty ruleName
    , IsJoint joint )
    <= IsLanguage ruleName joint
    | joint -> ruleName
  where
  -- | The production rules, indexed by `ruleName`.
  productionRules :: ruleName -> ProductionRule ruleName joint
  -- | The change rules, indexed by `ruleName`.
  changeRules :: ruleName -> ChangeRule ruleName joint
  -- | The default term (if any) for a sort.
  defaultTerm :: Sort joint -> Maybe (Term ruleName joint)
  -- | When a change is yielded at a cursor, split it into a change to propogate
  -- | downwards, and change to propogate upwards, and a new sort at the cursor.
  splitChange :: Change joint -> {down :: Change joint, up :: Change joint, sort :: Sort joint}
  -- | Defines if a cursor is valid as a function of its sort.
  isValidCursorSort :: Sort joint -> Boolean
  -- | Defines if a selection is valid as a function of its top and bottom sorts.
  isValidSelectionSorts :: {top :: Sort joint, bottom :: Sort joint} -> Boolean
  -- | The change propogated upwards when the term is deleted.
  digChange :: Sort joint -> Change joint
  -- | TODO: DOC
  generalize :: Sort joint -> Change joint
  -- | TODO: DOC
  specialize :: {general :: Sort joint, special :: Sort joint} -> Change joint

newtype ProductionRule ruleName joint = ProductionRule
  { quantifiers :: Set.Set MetaVar
  , kidSorts :: TermJoint ruleName joint (Sort joint)
  , parentSort :: Sort joint 
  }

newtype ChangeRule ruleName joint = ChangeRule
  { quantifiers :: Set.Set MetaVar
  , kidChanges :: TermJoint ruleName joint (Fix (ChangeJoint (MetaJoint joint)))
  }

-- -- example

-- data L = String
-- data J x = Leaf x | Tree x x
-- type G = Term L J

-- Misc

newtype Sub k v = Sub (Map.Map k v)

-- data Nat = Zero | Suc Nat
-- derive instance Eq Nat
-- derive instance Ord Nat

class Zippable a where
  zip :: a -> Maybe a
  unzip :: a -> Array a

-- | `Subtype a b` if every term of `a` can be mapped to a unique term of `b`.
class Subtype a b where inject :: a -> b
