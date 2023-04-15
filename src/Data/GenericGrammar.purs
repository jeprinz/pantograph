module Data.GenericGrammar where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Const (Const)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Reversed (ReversedList)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Type.Proxy (Proxy(..))

{-

The cleat is the fundamental connective component for derived structures
relevant to the grammar. A cleat has left, right, and tangent components, which
are interpreted with respect to the particular structure the cleat is used to
build.
-}
{-

expression:

  unit -- pre -- exprs
           |
         unit


expression change:

  left changes -- pre -- right changes
                   |
                  unit


plus/minus change:

  left exprs -- pre -- right exprs
                 | 
               change


up path:

                path
                 |
  left exprs -- pre -- right exprs


down path:

  left exprs -- pre -- right exprs
                 |
                path

-}

-- | The type of cleats in terms of
-- |  - `Annotation` ann
-- |  - type of functors over the left kids `left`
-- |  - type of functors over the right kids `left`
-- |  - type of pre-expressions `pre`
-- |  - type of tangents `tan`
-- |  - type of kids `kid`
type Cleat :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Type
type Cleat ann left right pre tan kid = { pre :: pre, tan :: tan, left :: left (ann kid), right :: right (ann kid) }

-- | The type of expression tooth cleats, which is `Cleat` with trivial left
-- | kids and list of right kids.
type ExprToothCleat :: (Type -> Type) -> Type -> Type -> Type -> Type
type ExprToothCleat ann pre tan kid = Cleat ann (Const Unit) List pre tan kid

-- | The type of path tooth cleats, which is `Cleat` with reversed list of left
-- | kids and list of right kids.
type PathToothCleat :: (Type -> Type) -> Type -> Type -> Type -> Type
type PathToothCleat ann pre tan kid = Cleat ann ReversedList List pre tan kid

-- | The type of expressions is a fixpoint over `ExprTooth`, which is an
-- | `ExprToothCleat` with trivial tangent.
newtype Expr ann pre = Expr (ExprTooth ann pre (Expr ann pre))
type ExprTooth :: (Type -> Type) -> Type -> Type -> Type
type ExprTooth ann pre expr = ExprToothCleat ann pre Unit expr

derive instance Generic (Expr ann pre) _
derive instance Newtype (Expr ann pre) _
derive instance Functor ann => Functor (Expr ann) -- over pre
derive instance Foldable ann => Foldable (Expr ann) -- over pre
derive instance Traversable ann => Traversable (Expr ann) -- over pre

matchExpr :: forall ann pre x. Functor ann => 
  ({ pre :: pre, kids :: List (ann (Expr ann pre)) } -> x) -> 
  Expr ann pre -> x
matchExpr f (Expr {pre, right}) = f {pre, kids: right}

-- | The type of changes is a fixpoint over a sum of expression tooth, two
-- | polarities of path tooth, and a meta case.
data Change ann pre meta
  = ExprChange (ExprToothCleat ann pre Unit (Change ann pre meta))
  | PlusChange (PathToothCleat ann pre (Change ann pre meta) (Expr ann pre))
  | MinusChange (PathToothCleat ann pre (Change ann pre meta) (Expr ann pre))
  | MetaChange meta

derive instance Generic (Change ann pre meta) _
derive instance Functor ann => Functor (Change ann pre) -- over meta
derive instance Foldable ann => Foldable (Change ann pre) -- over meta
derive instance Traversable ann => Traversable (Change ann pre) -- over meta
derive instance Functor ann => Bifunctor (Change ann) -- over pre and meta
derive instance Foldable ann => Bifoldable (Change ann) -- over pre and meta
derive instance Traversable ann => Bitraversable (Change ann) -- over pre and meta

matchChange :: forall x ann meta pre.
  { expr :: { pre :: pre, right :: List (ann (Change ann pre meta)) } -> x
  , minus :: { left :: ReversedList (ann (Expr ann pre)), pre :: pre, right :: List (ann (Expr ann pre)), tan :: Change ann pre meta } -> x
  , plus :: { left :: ReversedList (ann (Expr ann pre)), pre :: pre, right :: List (ann (Expr ann pre)), tan :: Change ann pre meta } -> x
  , meta :: meta -> x
  }
  -> Change ann pre meta -> x
matchChange f (ExprChange {pre, right}) = f.expr {pre, right}
matchChange f (PlusChange {pre, tan, left, right}) = f.plus {pre, tan, left, right}
matchChange f (MinusChange {pre, tan, left, right}) = f.minus {pre, tan, left, right}
matchChange f (MetaChange meta) = f.meta meta

-- | The types of paths, up and down, are fixpoints over `PathTooth`, which is
-- | an `PathToothCleat` with trivial tangent.
data Path :: Symbol -> (Type -> Type) -> Type -> Type
data Path dir ann pre = ConsPath (PathTooth ann pre (Path dir ann pre)) | NilPath
type PathTooth :: (Type -> Type) -> Type -> Type -> Type
type PathTooth ann pre path = PathToothCleat ann pre Unit path

derive instance Generic (Path dir ann pre) _
derive instance Functor ann => Functor (Path dir ann) -- over meta
derive instance Foldable ann => Foldable (Path dir ann) -- over meta
derive instance Traversable ann => Traversable (Path dir ann) -- over meta

type UpPath = Path "up"
type DownPath = Path "down"

-- matchPath f (ConsPath th) = ?a 
-- matchPath f NilPath = ?a