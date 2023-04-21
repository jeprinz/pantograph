module Data.Gram where

import Prelude

import Data.Array as Array
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, rmap)
import Data.Bitraversable (class Bitraversable)
import Data.Const (Const(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, intercalate)
import Data.Identity (Identity(..))
import Data.List.Zip as Zip
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Partial.Unsafe (unsafeCrashWith)
import Type.Direction as Dir
import Utility (map2)

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
derive instance Foldable j => Foldable (Gram j)
derive instance Traversable j => Traversable (Gram j)

wrapGram = Gram
unwrapGram (Gram g) = g
overGram f = unwrapGram >>> f >>> wrapGram
overMGram k = unwrapGram >>> k >>> map wrapGram
label = unwrapGram >>> _.l
joint = unwrapGram >>> _.j

-- | `Node` of `Gram` parametrized by type of kids.
type NodeG :: (Type -> Type) -> Type -> Type -> Type
type NodeG j l g = {l :: l, j :: j g}

-- | Alias for `NodeG` with `Gram j l'` kids.
type Node j l l' = NodeG j l (Gram j l')
-- | Alias for `NodeG` with `m (Gram j l')` kids.
type NodeM j m l l' = NodeG j l (m (Gram j l'))

mapNodeG_g :: forall j l g n. Functor j => (g -> n) -> NodeG j l g -> NodeG j l n
mapNodeG_g f n = n {j = f <$> n.j}

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
foldMapGram :: forall j l m. Functor j => Monoid m => (NodeG j l m -> m) -> Gram j l -> m
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
traverseGram :: forall j l l' m. Functor j => Applicative m => (NodeM j m l l' -> m (Node j l' l')) -> Gram j l -> m (Gram j l')
traverseGram f = overMGram \{l, j} -> f {l, j: traverseGram f <$> j}

-- | Expr
type Expr = Gram Array

-- | MetaExpr
type MetaExpr l = Gram Array (Meta l)

-- | Each node of a path is labeled. `Path1` is populated. Use `Path` for a path
-- | that could be empty.
type Path1 :: Type -> Type
type Path1 l = Gram Maybe (Tooth l)

type Path1Node    l l' = Node  Maybe   (Tooth l) (Tooth l')
type Path1NodeG   l l' = NodeG Maybe   (Tooth l) (Path1 l')
type Path1NodeM m l l' = NodeM Maybe m (Tooth l) (Tooth l')

-- newtype Tooth l = Tooth {l :: l, p :: Zip.Path (Expr l)}
-- derive instance Functor Tooth

type Tooth l = {l :: l, p :: Zip.Path (Expr l)}

newtype Path (dir :: Symbol) l = Path (Maybe (Path1 l))
derive instance Newtype (Path dir l) _

lastPath1 :: forall l. Tooth l -> Path1 l
lastPath1 th = wrapGram {l: th, j: Nothing}

lastPath :: forall dir l. Tooth l -> Path dir l
lastPath th = Newtype.wrap <<< Just $ lastPath1 th

stepPath1 :: forall l. Tooth l -> Maybe (Path1 l) -> Path1 l
stepPath1 th mb_p1 = wrapGram {l: th, j: mb_p1}

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
  go m_pBelow (Just p1Above) = go (Just $ wrapGram <$> f {l: label p1Above, j: m_pBelow}) (joint p1Above)

-- if = down, then p1 goes below
-- if = up, then p1 goes above
appendPath :: forall dir l. Path dir l -> Path dir l -> Path dir l
appendPath p1 = Newtype.unwrap >>> case _ of
  Nothing -> p1
  Just p2 -> stepPath (label p2) (appendPath p1 (Newtype.wrap (joint p2)))

reverse :: forall dir dir' l. Dir.Rev dir dir' => Path dir l -> Path dir' l
reverse = Newtype.over Path (go Nothing)
  where
  go p' Nothing = p'
  go p' (Just p) = go (Just (stepPath1 (label p) p')) (joint p)

-- | Change
type Change l = Expr (ChangeLabel l)
data ChangeLabel l
  = Plus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Minus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Expr l {-same number of kids that l has-}
  | Replace (Expr l) (Expr l) {-zero kids?-}

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

--
-- utilities
--

showGram show_l = foldMapGram \{l, j} ->
 let kids = Array.fromFoldable j
     kids_str = if Array.null kids then "" else " " <> intercalate " " kids
 in
 "(" <> show l <> kids_str <> ")"

