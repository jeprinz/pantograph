module Data.XXX1 where

import Data.Variant
import Prelude

import Control.Monad.Reader (Reader, runReader)
import Data.Bifunctor (class Bifunctor)
import Data.Either.Nested (type (\/))
import Data.Map as Map
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))

-- -- | A veriable mention, which can either be a binding mention of a variable or
-- -- | a use mention of a variable (either bound or free).
-- type Mention x = Variant ( bind :: x, use :: x )

-- | The generic type of grammatical structures.
-- |
-- | - `proto` is "proto-exprotossion"
-- | - `cleat` is "cleat" (viz
-- |   https://surreysteels.com/product/fabrication/notch-and-cleat-connection/)
newtype Gram proto cleat (var :: Type) = Gram (Gram' proto cleat var) -- fixpoint of GramF
type Gram' proto cleat var = GramF proto var (cleat (Gram proto cleat var)) -- fixpoint unfolded once
type GramF proto (var :: Type) rec = { proto :: proto var , cleat :: rec } -- base function of fixpoint Gram

derive instance (Functor cleat, Functor proto) => Functor (Gram proto cleat)

overCleat :: forall proto var rec rec'. (rec -> rec') -> GramF proto var rec -> GramF proto var rec'
overCleat f {proto, cleat} = {proto, cleat: f cleat}

traverseGram :: forall m proto cleat x y.
  Monad m => Traversable cleat =>
  (forall rec. GramF proto x (m rec) -> m (GramF proto y rec)) ->
  Gram proto cleat x -> m (Gram proto cleat y)
traverseGram f (Gram gf) = Gram <$> f (((f `traverseGram` _) `traverse` _) `overCleat` gf)

renameGram :: forall proto cleat x y. 
  Traversable cleat => Ord x =>
  (forall rec. GramF proto x (Reader (Map.Map x y) rec) -> Reader (Map.Map x y) (GramF proto y rec)) ->
  Gram proto cleat x -> Gram proto cleat y
renameGram f g = runReader (traverseGram f g) Map.empty

-- | The generic type of expressions, defined in terms of `Gram`.
type Expr proto var = Gram proto ExprCleat var
newtype ExprCleat expr = ExprCleat { kids :: Array expr }

-- The generic type of paths, defined in terms of `Gram`.
type Path proto var = Gram proto PathCleat var 
data PathCleat path = PathCons path | PathNil

-- -- The generic type of changes, defined in terms of `Gram`.
-- type Change proto var = Gram proto (ChangeCleat proto var) var
-- data ChangeCleat proto var change =


-- example

data Proto x = Var x | Lam x | App 
