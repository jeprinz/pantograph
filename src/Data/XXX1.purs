module Data.XXX1 where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.List.Reversed (ReversedList)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)

-- | The generic type of grammatical structures.
newtype Gram proto (joint :: Type -> Type) (link :: Type -> Type) (var :: Type) = Gram (Gram' proto joint link var) -- fixpoint of GramF
type Gram' proto joint link var = GramF proto joint link var (Gram proto joint link var) -- fixpoint unfolded once
type GramF proto joint (link :: Type -> Type) (var :: Type) (rec :: Type) = { proto :: proto var , joint :: joint (link rec) } -- base function of fixpoint Gram

derive instance Newtype (Gram proto join link var) _

-- -- !TODO though its possible to derive this, shouldn't because we want to map
-- -- over variables while being aware of their bindings, not just blindly map every location a-locally
-- derive instance (Functor proto, Functor joint, Functor link) => Functor (Gram proto joint link)

mapJointLink :: forall proto joint link var rec rec'. 
  Functor joint => Functor link => 
  (rec -> rec') -> 
  GramF proto joint link var rec -> GramF proto joint link var rec'
mapJointLink f gf@{ joint } = gf { joint = ((f <$> _) <$> _) joint }

-- !TODO necessary?
-- traverseJointLink :: forall proto joint link var m rec rec'.
--   Traversable joint => Traversable link => Applicative m =>
--   (rec -> m rec') -> 
--   GramF proto joint link var rec -> m (GramF proto joint link var rec')
-- traverseJointLink f gf@{ joint } = (gf { joint = _ }) <$> (f `traverse` _) `traverse` joint

mapGram :: forall proto joint link x y.
  Functor joint => Functor link =>
  (forall rec. GramF proto joint link x rec -> GramF proto joint link y rec) ->
  Gram proto joint link x -> Gram proto joint link y
mapGram f (Gram gf) = Gram (f <<< (mapGram f `mapJointLink` _) $ gf)

foldMapGram :: forall proto joint link x m.
  Functor joint => Functor link => Monoid m =>
  (GramF proto joint link x m -> m) ->
  Gram proto joint link x -> m
foldMapGram f (Gram gf) = f (foldMapGram f `mapJointLink` gf)

traverseGram :: forall m proto joint link x y.
  Monad m => Traversable joint => Traversable link =>
  (forall rec. GramF proto joint link x (m rec) -> m (GramF proto joint link y rec)) ->
  Gram proto joint link x -> m (Gram proto joint link y)
traverseGram f (Gram gf) = Gram <$> f ((f `traverseGram` _) `mapJointLink` gf)

renameGram :: forall proto joint link x y. 
  Traversable joint => Traversable link => Ord x =>
  (forall rec. GramF proto joint link x (Reader (Map.Map x y) rec) -> Reader (Map.Map x y) (GramF proto joint link y rec)) ->
  Gram proto joint link x -> Gram proto joint link y
renameGram f g = runReader (f `traverseGram` g) Map.empty

-- | The generic type of expressions, defined in terms of `Gram`. `ExprJoin`
-- | uses an `Array` of kids because it should be a constant number of kids
-- | (which `Array`s encode more efficiently than `List`s).
type Expr proto link var = Gram proto ExprJoint link var
type ExprF proto link var rec = GramF proto ExprJoint link var rec
data ExprJoint expr = ExprJoint (Array expr)

derive instance Functor ExprJoint
derive instance Foldable ExprJoint
derive instance Traversable ExprJoint

-- | The generic type of paths, defined in terms of `Gram`.
type Path proto link var = Gram proto (PathJoint proto link var) link var
data PathJoint proto link var (path :: Type) = PathJoint (List (Tooth proto link var))
type Tooth proto link var = { lefts :: ReversedList (Expr proto link var), rights :: List (Expr proto link var) }

derive instance Functor (PathJoint proto link var)
derive instance Foldable (PathJoint proto link var)
derive instance Traversable (PathJoint proto link var)

-- -- The generic type of changes, defined in terms of `Gram`.
type Change proto link var metavar = Gram proto (ChangeJoint proto link var metavar) link var
data ChangeJoint proto link var metavar change
  = Plus { change :: Change proto link var metavar, tooth :: Tooth proto link var }
  | Minus { change :: Change proto link var metavar, tooth :: Tooth proto link var }
  | Expr (ExprJoint (Change proto link var metavar))
  | Meta metavar

derive instance Functor (ChangeJoint proto link var metavar)
derive instance Foldable (ChangeJoint proto link var metavar)
derive instance Traversable (ChangeJoint proto link var metavar)

