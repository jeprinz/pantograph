module Data.Gram3 where

import Data.Tuple.Nested (type (/\))
import Prelude
import Data.Bifunctor (class Bifunctor, rmap)
import Data.Either (Either)
import Data.List.Zip as Zip
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

-- | MetaVar

newtype MetaVar = MetaVar String

derive newtype instance Show MetaVar

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = str

class IsCore :: forall k. k -> Constraint
class IsCore c

-- | Gram
newtype Gram :: forall k. (Type -> k -> Type) -> (Type -> k -> Type -> Type) -> Type -> k -> Type
newtype Gram j1 j2 l x = Gram (Gram' j1 j2 l x (Gram j1 j2 l x))
newtype Gram' :: forall k1 k2. (Type -> k1 -> Type) -> (Type -> k1 -> k2 -> Type) -> Type -> k1 -> k2 -> Type
newtype Gram' j1 j2 l x g = Gram' (j1 (l /\ j2 l x g) x)

derive instance (Bifunctor j1, Bifunctor (j2 l)) => Functor (Gram j1 j2 l)
derive instance (Bifunctor j1, Bifunctor (j2 l)) => Bifunctor (Gram' j1 j2 l)

traverseGram :: forall j1 j2 l x x' m.
  Applicative m => Bifunctor j1 => Bifunctor (j2 l) =>
  (Gram' j1 j2 l x (m (Gram j1 j2 l x')) -> m (Gram' j1 j2 l x' (Gram j1 j2 l x'))) ->
  Gram j1 j2 l x -> m (Gram j1 j2 l x')
traverseGram f (Gram g') = Gram <$> f (traverseGram f `rmap` g')

-- | Expr
type Expr = Gram ExprJoint1 ExprJoint2
newtype ExprJoint1 :: forall k. k -> Type -> Type
newtype ExprJoint1 x g = ExprJoint1 g
newtype ExprJoint2 :: forall k1 k2. k1 -> k2 -> Type -> Type
newtype ExprJoint2 l x g = ExprJoint2 (Array g)

derive instance Newtype (ExprJoint1 x   g) _
derive instance Newtype (ExprJoint2 l x g) _

-- | MetaExpr
type MetaExpr = Gram MetaExprJoint1 MetaExprJoint2
newtype MetaExprJoint1 x g = MetaExprJoint1 (Either x g)
newtype MetaExprJoint2 :: forall k1 k2. k1 -> k2 -> Type -> Type
newtype MetaExprJoint2 l x g = MetaExprJoint2 (Array g)

derive instance Newtype (MetaExprJoint1 x   g) _
derive instance Newtype (MetaExprJoint2 l x g) _

-- | DownPath
type DownPath = Gram PathJoint1 PathJoint2
newtype PathJoint1 :: forall k. k -> Type -> Type
newtype PathJoint1 x g = PathJoint1 (Maybe g)
newtype PathJoint2 l x g = PathJoint2 (Zip.Path (Expr l x) /\ g)

derive instance Newtype (PathJoint1 x   g) _
derive instance Newtype (PathJoint2 l x g) _

newtype UpPath l x = UpPath (DownPath l x)
type Tooth l x = PathJoint2 l x Void

-- | Change
type Change = Gram ChangeJoint1 ChangeJoint2
newtype ChangeJoint1 :: forall k. k -> Type -> Type
newtype ChangeJoint1 x g = ChangeJoint1 (Either MetaVar g)
data ChangeJoint2 l x g 
  = Plus (Zip.Path (MetaExpr l x) /\ g)
  | Minus (Zip.Path (MetaExpr l x) /\ g)
  | Expr (Array g)
  | Replace (MetaExpr l x /\ MetaExpr l x)

derive instance Newtype (ChangeJoint1 x   g) _

