module Data.Gram where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldr, foldrDefault, intercalate, sequence_)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.Zip as ListZip
import Data.List.Zip as Zip
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Show.Generic (class GenericShow, genericShow)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Type.Direction as Dir

-- | MetaVar

newtype MetaVar = MetaVar UUID

derive newtype instance Show MetaVar

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = show str

type Meta l = MetaVar \/ l

-- | A `Gram` is a generalization of a tree (and a specialization of a
-- | fixpoint), where each node of the `Gram` has a label of type `l`, and the kids are 
newtype Gram j l = Gram (NodeG j l (Gram j l))

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
    "(Gram (" <> show l <> " /\\ " <> "[" <> intercalate ", " j <> "]" <> "))"

-- | In order to show any `Gram`, first map each label to `unit`, and then show.
-- | This will only show the structure of the `Gram` and nothing about the
-- | labels -- useful for debug in the generic code. (Alternatively, could
-- | include `Show l` typeclass constraint everywhere, but that's annoying).
showGramStructure :: forall j l. Functor j => Foldable j => Gram j l -> String
showGramStructure = foldMapGram \(_ /\ j) -> "(" <> intercalate " " j <> ")"

-- showGramGeneric :: forall j l l_rep. Functor j => Foldable j => GenericShow l_rep => Generic l l_rep => Gram j l -> String
-- showGramGeneric = show <<< map (\x -> genericShow x)

-- | `Node` of `Gram` parametrized by type of kids.
-- type NodeG j l g = {l :: l, j :: j g}
type NodeG j l g = l /\ j g

-- | Alias for `NodeG` with `Gram j l'` kids.
type Node j l l' = NodeG j l (Gram j l')
-- | Alias for `NodeG` with `m (Gram j l')` kids.
type NodeM j m l l' = NodeG j l (m (Gram j l'))

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

-- | Each node of a path is labeled. `Path1` is populated. Use `Path` for a path
-- | that could be empty.
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

newtype Path (dir :: Symbol) l = Path (Maybe (Path1 l))
derive instance Newtype (Path dir l) _

lastPath1 :: forall l. Tooth l -> Path1 l
lastPath1 th = Gram (th /\ Nothing)

lastPath :: forall dir l. Tooth l -> Path dir l
lastPath th = Newtype.wrap <<< Just $ lastPath1 th

stepPath1 :: forall l. Tooth l -> Maybe (Path1 l) -> Path1 l
-- stepPath1 th p = wrapGram (p /\ th)
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

reverse :: forall dir dir' l. Dir.Rev dir dir' => Path dir l -> Path dir' l
reverse = Newtype.over Path (go Nothing)
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
instance Show (ChangeLabel String) where show x = genericShow x
derive instance Functor ChangeLabel

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
