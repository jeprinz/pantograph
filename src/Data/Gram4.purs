module Data.Gram4 where

import Prelude

import Data.Const (Const(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, intercalate)
import Data.List.Zip as Zip
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Type.Direction as Dir
import Utility (map2)

-- | MetaVar

newtype MetaVar = MetaVar String

derive newtype instance Show MetaVar

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = str

type Meta l = MetaVar \/ l

-- | Gram

newtype Gram j l = Gram (Gram' j l (Gram j l))

type Gram' :: (Type -> Type) -> Type -> Type -> Type
type Gram' j l g = l /\ j g

derive instance Newtype (Gram j l) _
derive instance Functor j => Functor (Gram j)
derive instance Foldable j => Foldable (Gram j)
derive instance Traversable j => Traversable (Gram j)

foldMapGram f = unwrap <<< traverseGram (Const <<< f <<< map2 unwrap)

traverseGram :: forall j l l' m.
  Functor j => Applicative m =>
  (Gram' j l (m (Gram j l')) -> m (Gram' j l' (Gram j l'))) ->
  Gram j l -> m (Gram j l')
traverseGram f (Gram (l /\ j)) = Gram <$> f (l /\ (traverseGram f <$> j))

-- | Expr
type Expr = Gram Array

-- | MetaExpr
type MetaExpr l = Gram Array (Meta l)

-- | DownPath
type Path :: Symbol -> Type -> Type
type Path dir l = Gram Maybe (Maybe (Tooth l))

type Tooth l = Zip.Path (Expr l) /\ l

endPath :: forall dir l. Path dir l
endPath = wrap (Nothing /\ Nothing)

matchPath :: forall dir l a.
  { end :: a
  , step :: Tooth l /\ Path dir l -> a
  } -> Path dir l -> a
matchPath f = unwrap >>> case _ of
  -- if dir = down, then at bottom of path
  -- if dir = up, then at top of path
  Nothing /\ Nothing -> f.end
  Just l /\ Just p2 -> f.step $ l /\ p2
  _ -> unsafeCrashWith "invalid path"

-- if dir = down, then p1 goes below
-- if dir = up, then p1 goes above
appendPath :: forall dir l. Path dir l -> Path dir l -> Path dir l
appendPath p1 = matchPath
  { end: p1
  , step: \(th /\ p2) -> wrap (Just th /\ Just (appendPath p1 p2))
  }

reverse :: forall dir dir' l. Dir.Rev dir dir' => Path dir l -> Path dir' l
reverse = go endPath
  where
  go :: Path dir' l -> Path dir l -> Path dir' l
  go p1 = matchPath 
    { end: p1
    , step: \(th /\ p2) -> go (wrap (Just th /\ Just p1)) p2
    }

-- | Change
type Change l = Gram Array (ChangeLabel l)
data ChangeLabel l 
  = Plus (Tooth l)
  | Minus (Tooth l)
  | Expr l
  | Replace (Expr l) (Expr l)

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

--
-- utilities
--

showGram show_l = foldMapGram \(l /\ j) -> "(" <> show l <> " " <> intercalate " " j <> ")"

