module Data.Gram where

import Prelude

import Control.Monad.Maybe.Trans (class MonadTrans, MaybeT, lift)
import Control.Monad.State (class MonadState, StateT, modify, modify_)
import Control.Plus (empty)
import Control.Plus as Plus
import Data.Array as Array
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldr, foldrDefault, intercalate, sequence_)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.Zip as ListZip
import Data.List.Zip as Zip
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as Newtype
import Data.Show.Generic (class GenericShow, genericShow)
import Data.String as String
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), curry, fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Data.Unfoldable (class Unfoldable)
import Data.Unify (class Unify, unify)
import Data.Unify.Generic (genericUnify)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Type.Direction as Dir
import Type.Proxy (Proxy(..))

-- | A `Gram` is a generalization of a tree (and a specialization of a
-- | fixpoint), where each node of the `Gram` has a label of type `l`, and the kids are 
data Gram j l = Gram (NodeG j l (Gram j l))

derive instance Generic (Gram j l) _
derive instance Functor j => Functor (Gram j)

instance Foldable j => Foldable (Gram j) where
  foldMap f (Gram (l /\ j)) = f l <> foldMap (foldMap f) j
  foldr f b (Gram (l /\ j)) = foldr (\g  b' -> foldr f b' g) (f l b) j
  foldl f b (Gram (l /\ j)) = foldl (\b' g  -> foldl f b' g) (f b l) j

instance (Functor j, Traversable j) => Traversable (Gram j) where
  traverse f (Gram (l /\ j)) = Gram <$> (Tuple <$> f l <*> traverse (traverse f) j)
  sequence fa = sequenceDefault fa

instance (Show l, Functor j, Foldable j) => Show (Gram j l) where
  show = foldMapGram \(l /\ j) ->
    if Foldable.null j 
      then show l
      else "(" <> show l <> " " <> intercalate " " j <> ")"

-- | In order to show any `Gram`, first map each label to `unit`, and then show.
-- | This will only show the structure of the `Gram` and nothing about the
-- | labels -- useful for debug in the generic code. (Alternatively, could
-- | include `Show l` typeclass constraint everywhere, but that's annoying).
showGramStructure :: forall j l. Functor j => Foldable j => Gram j l -> String
showGramStructure = foldMapGram \(_ /\ j) -> "(" <> intercalate " " j <> ")"

-- | MetaVar

newtype MetaVar = MetaVar UUID

instance Show MetaVar where show (MetaVar uuid) = "?" <> String.take 2 (UUID.toString uuid)
derive newtype instance Eq MetaVar
derive newtype instance Ord MetaVar

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = show str

freshMetaVar :: Unit -> MetaVar
freshMetaVar _ = MetaVar $ unsafePerformEffect (UUID.genUUID)

newtype Meta l = Meta (MetaVar \/ l)

derive instance Newtype (Meta l) _
instance Show l => Show (Meta l) where show = unwrap >>> either show show
derive newtype instance Eq l => Eq (Meta l)

class IsMeta meta concrete | meta -> concrete where
  fromMetaVar :: MetaVar -> meta
  fromConcrete :: concrete -> meta
  matchMeta :: forall a. meta -> {meta :: MetaVar -> a, concrete :: concrete -> a} -> a

instance IsMeta (Meta l) l where
  fromMetaVar = Meta <<< Left
  fromConcrete = Meta <<< Right
  matchMeta (Meta (Left mv)) f = f.meta mv
  matchMeta (Meta (Right l)) f = f.concrete l

-- instance (IsMeta ml l, Foldable j, Plus.Plus j) => IsMeta (Gram j ml) (Node j l ml) where
--   fromMetaVar mv = ?a -- Gram (fromMetaVar mv /\ empty)
--   fromConcrete = ?a -- map fromConcrete
--   matchMeta = ?a
--   -- matchMeta (l /\ j) f = matchMeta l 
--   --   { meta: \mv -> if Foldable.null j
--   --       then f.meta mv
--   --       else unsafeThrow "metavariable case of a Gram should not have any children"
--   --   , concrete: \l -> f.concrete (Gram (l /\ (?a <$> j)))
--   --   }

_unifySub = Proxy :: Proxy "unifySub"
newtype UnifyT meta m a = UnifyT (StateT (UnifySt meta m) (MaybeT m) a)
type UnifySt meta m = {unifySub :: Map.Map MetaVar (UnifyT meta m meta)}

unUnifyT (UnifyT m) = m

instance MonadTrans (UnifyT meta) where lift = UnifyT <<< lift <<< lift
instance Functor m => Functor (UnifyT meta m) where map f (UnifyT ma) = UnifyT (f <$> ma)
instance Monad m => Apply (UnifyT meta m) where apply (UnifyT mf) (UnifyT ma) = UnifyT (mf <*> ma)
instance Monad m => Bind (UnifyT meta m) where bind (UnifyT ma) k = UnifyT (ma >>= k >>> unUnifyT)
instance Monad m => Applicative (UnifyT meta m) where pure a = UnifyT (pure a)

-- ml is meta label
-- cl is concrete label
instance (Monad m, IsMeta ml l) => Unify (UnifyT (Gram j ml) m) (Gram j ml) where
  -- unify = unsafeThrow "TODO"
  unify (Gram (ml1 /\ j1)) g2@(Gram (ml2 /\ j2)) = matchMeta ml1
    { meta: \mv1 -> do
        lift (modify_ (Record.modify _unifySub (Map.insert mv1 ?g2)))
        pure ?a
      -- modify_ (Record.modify _unifySub (Map.insert mv1 ?g2)) >>= \_ -> pure ?g2
    , concrete: \l1 -> matchMeta ml2 
      { meta: ?a 
      , concrete: ?a
      }
    }

-- instance (Unify m l, MonadState {unifySub :: Map.Map MetaVar (Meta l) | r} m) => Unify m (Meta l) where
--   unify (Meta (Left mv1)) m2 = modify_ (Record.modify _unifySub (Map.insert mv1 m2)) >>= \_ -> pure m2
--   unify m1 (Meta (Left mv2)) = modify_ (Record.modify _unifySub (Map.insert mv2 m1)) >>= \_ -> pure m1
--   unify (Meta (Right l1)) (Meta (Right l2)) = Meta <<< Right <$> unify l1 l2

{-
instance (Applicative m, Plus.Plus m, Unify m l, Functor j, Foldable j, Unfoldable j, Traversable j) => Unify m (Gram j l) where
  unify (Gram (l1 /\ j1)) (Gram (l2 /\ j2)) = 
    curry Gram
      <$> (unify l1 l2)
      <*> (Array.toUnfoldable <$> traverse (uncurry unify) (Array.zip (Array.fromFoldable j1) (Array.fromFoldable j2)))
-}

-- | `Node` of `Gram` parametrized by type of kids.
type NodeG :: forall k. (k -> Type) -> Type -> k -> Type
type NodeG j l g = l /\ j g

-- | Alias for `NodeG` with `Gram j l'` kids.
type Node j l l' = NodeG j l (Gram j l')
-- | Alias for `NodeG` with `m (Gram j l')` kids.
type NodeM j m l l' = NodeG j l (m (Gram j l'))

type NodeMG :: forall k1 k2. (k1 -> Type) -> (k2 -> k1) -> Type -> k2 -> Type
type NodeMG j m l g = NodeG j l (m g)

showNodeUnit :: forall j l g. Functor j => Foldable j => NodeG j l g -> String
showNodeUnit (_l /\ j) = "(" <> show unit <> " /\\ " <> "[" <> intercalate ", " (show <<< const unit <$> j) <> "]" <> ")"

mapNodeG_g :: forall j l g g'. Functor j => (g -> g') -> NodeG j l g -> NodeG j l g'
mapNodeG_g f = rmap (map f)

mapNodeG_l :: forall j l l' g. Functor j => (l -> l') -> NodeG j l g -> NodeG j l' g
mapNodeG_l f = lmap f

bimapNodeG :: forall j l l' g g'. Functor j => (l -> l') -> (g -> g') -> NodeG j l g -> NodeG j l' g'
bimapNodeG f_l f_g = bimap f_l (map f_g)

unwrapGram (Gram g) = g
overGram f = unwrapGram >>> f >>> Gram
overMGram k = unwrapGram >>> k >>> map Gram
labelGram = unwrapGram >>> fst
jointGram = unwrapGram >>> snd

-- | Map a function `f` over the labels in a `Gram`, taking into account `Node`
-- | structure. This is implemented by `travereGram`ing via the trivial
-- | applicative `Identity`.
-- |
-- | This should actually just serve as a spec, since `Gram` is automatically
-- | derived to be a functor.
mapGram :: forall j l l'. Functor j => (Node j l l' -> Node j l' l') -> Gram j l -> Gram j l'
mapGram f = Newtype.unwrap <<< traverseGram (Identity <<< f <<< mapNodeG_g Newtype.unwrap)

-- | Fold-map a function `f` over `Gram` taking into account `Node` structure.
-- | This is implemented by `traverseGram`ing via the applicative `Const m`,
-- | where `m` is the monoid we are fold-mapping into, which just accumulates
-- | the monoid value and discards the `Gram`-value of the children.
foldMapGram :: forall j l m. Functor j => (NodeG j l m -> m) -> Gram j l -> m
foldMapGram f = Newtype.unwrap <<< traverseGram (Const <<< f <<< mapNodeG_g Newtype.unwrap)

-- | Traverse a function `f` over the labels of a `Gram`, taking into account
-- | `Node` structure. This is the most general function over `Gram`, which
-- | `mapGram`, `foldMapGram`, etc are built out of.
-- |
-- | Observe that `f`'s type specifies that `f` assumes that the children of
-- | each `Node` it traverses has already had its children traversed. In this
-- | way, you don't have to manually call `traverseGram` on the children. If you
-- | want the traversal over the children to use data computed while traversing
-- | the parent, then `m` should implement `MonadReader r` where `r` is whatever
-- | data the children can use, and `f` should use `Reader.local` in order to
-- | provide that data. See (!TODO link to example here) for an example of this
-- | pattern.
traverseGram :: forall j l l' m. Functor j => Functor m => (NodeM j m l l' -> m (Node j l' l')) -> Gram j l -> m (Gram j l')
traverseGram f (Gram (l /\ j)) = Gram <$> f (l /\ (traverseGram f <$> j))

traverseGram_ :: forall j l m a. Functor j => Functor m => (NodeMG j m l a -> m a) -> Gram j l -> m a
traverseGram_ f (Gram (l /\ j)) = f (l /\ (traverseGram_ f <$> j))

logGramString :: forall j. Functor j => Traversable j => Gram j String -> Effect Unit
logGramString = traverseGram_ \(l /\ j) -> do
  log l 
  sequence_ j

-- | Expr
type Expr = Gram Array

expr l kids = Gram (l /\ kids)

-- | MetaExpr
type MetaExpr l = Gram Array (Meta l)

-- | Each node of a path is labeled. `Path1` is populated (has at least one
-- | element). Use `Path` for a path that could be empty.
type Path1 :: Type -> Type
type Path1 l = Gram Maybe (Tooth l)

type Path1Node    l l' = Node  Maybe   (Tooth l) (Tooth l')
type Path1NodeG   l l' = NodeG Maybe   (Tooth l) (Path1 l')
type Path1NodeM m l l' = NodeM Maybe m (Tooth l) (Tooth l')

type Tooth l = l /\ ListZip.Path (Expr l)

foldMapTooth :: forall l m. Monoid m => (l -> m) -> Tooth l -> m
foldMapTooth f (l /\ p) = f l <> foldMap (foldMap f) p

foldlTooth :: forall l a b. (b -> a -> b) -> b -> Tooth a -> b
foldlTooth f b (l /\ p) = ZipList.foldlAround (foldl f) b (flip f l) p

foldrTooth :: forall l a b. (a -> b -> b) -> b -> Tooth a -> b
foldrTooth f b (l /\ p) = ZipList.foldrAround (flip (foldr f)) b (f l) p

traverseTooth :: forall l l' m. Applicative m => (l -> m l') -> Tooth l -> m (Tooth l')
traverseTooth f (l /\ p) = Tuple <$> f l <*> traverse (traverse f) p

unTooth (l /\ p) g = expr l (Array.fromFoldable (ListZip.unpathAround g p))

showTooth (l /\ p) = show l <> " " <> intercalate " " (ListZip.unpathAround "{}" (show <$> p))

newtype Path (dir :: Symbol) l = Path (Maybe (Path1 l))
derive instance Newtype (Path dir l) _

lastPath1 :: forall l. Tooth l -> Path1 l
lastPath1 th = Gram (th /\ Nothing)

lastPath :: forall dir l. Tooth l -> Path dir l
lastPath th = Newtype.wrap <<< Just $ lastPath1 th

stepPath1 :: forall l. Tooth l -> Maybe (Path1 l) -> Path1 l
stepPath1 th p = Gram (th /\ p)

stepPath :: forall dir l. Tooth l -> Path dir l -> Path dir l
stepPath th p = Newtype.wrap <<< Just $ stepPath1 th (Newtype.unwrap p)

traverseDownPath :: forall dir l l' m. Applicative m => (Path1NodeM m l l' -> m (Path1Node l' l')) -> Path dir l -> m (Path dir l')
traverseDownPath f = Newtype.unwrap >>> case _ of 
  Nothing -> Newtype.wrap <$> pure Nothing
  Just p -> Newtype.wrap <$> (Just <$> traverseGram f p)

-- | Basically, traverse the path in reverse. The implementation uses
-- | tail-recursion to be efficient.
traverseUpPath :: forall dir l l' m. Applicative m => (Path1NodeM m l l' -> m (Path1Node l' l')) -> Path dir l -> m (Path dir l')
traverseUpPath f = map Path <<< go Nothing <<< Newtype.unwrap
  where
  go :: Maybe (m (Path1 l')) -> Maybe (Path1 l) -> m (Maybe (Path1 l'))
  go m_pBelow Nothing = sequence m_pBelow
  go m_pBelow (Just p1Above) = go (Just $ Gram <$> f (labelGram p1Above /\ m_pBelow)) (jointGram p1Above)

-- if = down, then p1 goes below
-- if = up, then p1 goes above
appendPath :: forall dir l. Path dir l -> Path dir l -> Path dir l
appendPath p1 = Newtype.unwrap >>> case _ of
  Nothing -> p1
  Just p2 -> stepPath (labelGram p2) (appendPath p1 (Newtype.wrap (jointGram p2)))

reversePath :: forall dir dir' l. Dir.Rev dir dir' => Path dir l -> Path dir' l
reversePath = Newtype.over Path (go Nothing)
  where
  go p' Nothing = p'
  go p' (Just p) = go (Just (stepPath1 (labelGram p) p')) (jointGram p)

-- | Change
type Change l = Expr (ChangeLabel l)
data ChangeLabel l
  = Plus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Minus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Expr l {-same number of kids that l has-}
  | Replace (Expr l) (Expr l) {-zero kids?-}

derive instance Generic (ChangeLabel l) _

instance Show l => Show (ChangeLabel l) where
  show (Plus th) = "(+ " <> showTooth th <> ")"
  show (Minus th) = "(- " <> showTooth th <> ")"
  show (Expr e) = show e
  show (Replace e1 e2) = "(" <> show e1 <> "~~> " <> show e2 <> ")"

derive instance Functor ChangeLabel

-- -- instance (Applicative m, Plus.Plus m, Unify m l) => Unify m (ChangeLabel l) where unify x y = genericUnify x y
-- instance IsMeta ml l => IsMeta (ChangeLabel ml) (ChangeLabel l) where
--   fromMetaVar = ?a 
--   fromConcrete = ?a 
--   matchMeta = ?a 

instance Foldable ChangeLabel where
  foldMap f = case _ of
    Plus th -> foldMapTooth f th
    Minus th -> foldMapTooth f th
    Expr l -> f l
    Replace e1 e2 -> foldMap f e1 <> foldMap f e2
  foldl f b = case _ of
    Plus th -> foldlTooth f b th
    Minus th -> foldlTooth f b th
    Expr l -> f b l
    Replace e1 e2 -> foldl f (foldl f b e1) e2
  foldr f b = case _ of 
    Plus th -> foldrTooth f b th
    Minus th -> foldrTooth f b th
    Expr l -> f l b
    Replace e1 e2 -> foldr f (foldr f b e2) e1

instance Traversable ChangeLabel where
  traverse f = case _ of
    Plus th -> Plus <$> traverseTooth f th
    Minus th -> Minus <$> traverseTooth f th
    Expr l -> Expr <$> f l
    Replace e1 e2 -> Replace <$> traverse f e1 <*> traverse f e2
  sequence fa = sequenceDefault fa

type MetaChange l = Change (Meta l)

plusChange :: forall l. Tooth l -> Change l -> Change l
plusChange th g = Gram (Plus th /\ [g])

minusChange :: forall l. Tooth l -> Change l -> Change l
minusChange th g = Gram (Minus th /\ [g])

exprChange :: forall l. l -> Array (Change l) -> Change l
exprChange l g = Gram (Expr l /\ g)

replaceChange :: forall l. Expr l -> Expr l -> Change l
replaceChange e1 e2 = Gram (Replace e1 e2 /\ [])

matchChangeNode :: forall l g a.
  NodeG Array (ChangeLabel l) g ->
  { plus :: Tooth l -> g -> a
  , minus :: Tooth l -> g -> a
  , expr :: l -> Array g -> a
  , replace :: Expr l -> Expr l -> a
  } -> a
matchChangeNode = flip \f -> case _ of
  (Plus th /\ [kid]) -> f.plus th kid
  (Minus th /\ [kid]) -> f.minus th kid
  (Expr l /\ kids) -> f.expr l kids
  (Replace e1 e2 /\ []) -> f.replace e1 e2
  g -> unsafeCrashWith $ "invalid change: " <> showNodeUnit g

matchChange (Gram n) = matchChangeNode n

-- Name (placeholder)

newtype Name = Name String

derive instance Generic Name _
derive newtype instance Eq Name
derive newtype instance Ord Name
instance Show Name where show (Name str) = str
-- instance (Functor m, Plus.Plus m, Applicative m) => Unify m Name where unify x y = genericUnify x y
