module Data.Unify where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Traversable (class Foldable, class Traversable)
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Unsafe (unsafePerformEffect)
import Text.Pretty (class Pretty, pretty)

-- | MetaVar

newtype MetaVar = MetaVar UUID

derive newtype instance Show MetaVar
derive newtype instance Eq MetaVar
derive newtype instance Ord MetaVar
instance Pretty MetaVar where pretty (MetaVar uuid) = "?" <> String.take 2 (UUID.toString uuid)

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = show str

freshMetaVar :: Unit -> MetaVar
freshMetaVar _ = MetaVar $ unsafePerformEffect (UUID.genUUID)

newtype Meta a = Meta (Either MetaVar a)
derive instance Newtype (Meta a) _
derive newtype instance Show a => Show (Meta a)
derive newtype instance Eq a => Eq (Meta a)
derive newtype instance Ord a => Ord (Meta a)
derive newtype instance Functor Meta
derive newtype instance Apply Meta
derive newtype instance Applicative Meta
derive newtype instance Foldable Meta
derive newtype instance Traversable Meta
instance Pretty a => Pretty (Meta a) where pretty = unwrap >>> either pretty pretty

{-
-- | IsMeta

class IsMeta :: Type -> Type -> Constraint
class IsMeta meta conc | meta -> conc where
  fromMetaVar :: Maybe (MetaVar -> meta)
  fromConcrete :: conc -> meta
  metaEither :: meta -> Either MetaVar conc

instance IsMeta (Meta a) a where
  fromMetaVar = Just (Meta <<< Left)
  fromConcrete = Meta <<< Right
  metaEither (Meta e) = e
-}

{-
-- | UnifySt

class UnifySt :: Type -> Type -> Constraint
class UnifySt st meta where
  lookup :: Proxy meta -> MetaVar -> st -> Maybe meta
  insert :: MetaVar -> meta -> st -> st

-- | UnifyM

type UnifyM st = StateT st Maybe

-- | Unify

class Unify :: Type -> Type -> Type -> Constraint
class (IsMeta meta conc, UnifySt st meta) <= Unify st meta conc | meta -> conc where
  substConcrete :: Proxy meta -> conc -> UnifyM st conc
  unifyConcrete :: Proxy meta -> conc -> conc -> UnifyM st Unit

subst :: forall st meta conc. Unify st meta conc => meta -> UnifyM st meta
subst m = do
  case metaEither m of
    Left mv -> gets (lookup (Proxy :: Proxy meta) mv) >>= case _ of
      Nothing -> pure m
      Just m' -> subst m'
    Right c -> fromConcrete <$> substConcrete (Proxy :: Proxy meta) c

unify :: forall st meta conc. Unify st meta conc => meta -> meta -> UnifyM st Unit 
unify m1 m2 = do
  m1' <- subst m1
  case metaEither m1' of
    Left mv -> modify_ $ insert mv m2
    Right c1 -> do
      m2' <- subst m2
      case metaEither m2' of
        Left mv -> modify_ $ insert mv m1
        Right c2 -> unifyConcrete (Proxy :: Proxy meta) c1 c2

-- | GenericUnify

class GenericUnify :: Type -> Type -> Type -> Constraint
class GenericUnify st meta a where
  genericSubst' :: Proxy meta -> a -> UnifyM st a
  genericUnify' :: Proxy meta -> a -> a -> UnifyM st Unit

instance GenericUnify st meta NoConstructors where
  genericSubst' _ a = pure a
  genericUnify' _ _ _ = pure unit

instance GenericUnify st meta NoArguments where
  genericSubst' _ a = pure a
  genericUnify' _ _ _ = pure unit

instance (GenericUnify st meta a, GenericUnify st meta b) => GenericUnify st meta (Sum a b) where
  genericSubst' p (Inl a) = Inl <$> genericSubst' p a
  genericSubst' p (Inr b) = Inr <$> genericSubst' p b
  genericUnify' p (Inl a1) (Inl a2) = genericUnify' p a1 a2
  genericUnify' p (Inr b1) (Inr b2) = genericUnify' p b1 b2
  genericUnify' _ _ _ = empty

instance (GenericUnify st meta a, GenericUnify st meta b) => GenericUnify st meta (Product a b) where
  genericSubst' p (Product a b) = Product <$> genericSubst' p a <*> genericSubst' p b
  genericUnify' p (Product a1 b1) (Product a2 b2) = do
    genericUnify' p a1 a2
    genericUnify' p b1 b2

instance GenericUnify st meta a => GenericUnify st meta (Constructor name a) where
  genericSubst' p (Constructor a) = Constructor <$> genericSubst' p a
  genericUnify' p (Constructor a1) (Constructor a2) = genericUnify' p a1 a2

instance Unify st meta' conc' => GenericUnify st meta (Argument meta') where
  genericSubst' _ (Argument a) = Argument <$> subst a
  genericUnify' _ (Argument a1) (Argument a2) = unify a1 a2

genericSubst :: forall st meta a rep.
  Generic a rep => GenericUnify st meta rep => 
  Proxy meta -> a -> UnifyM st a
genericSubst _ a = to <$> genericSubst' (Proxy :: Proxy meta) (from a)

genericUnify :: forall st meta a rep. 
  Generic a rep => GenericUnify st meta rep =>
  Proxy meta -> a -> a -> UnifyM st Unit
genericUnify _ a1 a2 = genericUnify' (Proxy :: Proxy meta) (from a1) (from a2)

-- instances

instance IsMeta meta conc => IsMeta (Maybe meta) (Maybe conc) where
  fromMetaVar = fromMetaVar <#> \f mv -> f mv >>= Just
  fromConcrete = map fromConcrete
  metaEither m = sequence $ metaEither <$> m

-- instance Unify st meta conc => Unify st (Maybe meta) (Maybe conc) where
--   substConcrete = ?a
--   unifyConcrete = ?a

-- example

newtype T = T (Meta (Tuple String (Maybe T)))
type T' = Tuple String (Maybe T)
derive instance Generic T _

instance IsMeta String String where
  fromMetaVar = Nothing
  fromConcrete = identity
  metaEither = Right

newtype MapT = MapT (Map.Map MetaVar T)
derive instance Newtype MapT _

-- for T

instance IsMeta T T' where
  fromMetaVar = fromMetaVar <#> \f mv -> T (f mv)
  fromConcrete = T <<< fromConcrete
  metaEither (T m) = metaEither m

instance UnifySt MapT T where
  lookup _ x = unwrap >>> Map.lookup x
  insert mv x = over MapT $ Map.insert mv x

instance Unify MapT T T' where
  substConcrete p c = genericSubst p c
  unifyConcrete p c1 c2 = genericUnify p c1 c2

-- instances for String

instance UnifySt MapT String where
  lookup _ _ _ = Nothing
  insert _ _ = identity

instance Unify MapT String String where
  substConcrete _ = pure
  unifyConcrete _ _ _ = pure unit
-}