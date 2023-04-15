module Data.Binding where

import Prelude
import Control.Monad.Reader (Reader)
import Control.Monad.State (State)
import Data.Set as Set

type BindingInfo x
  = { intros :: Set.Set x
    , uses :: Set.Set x
    }

class BindSite f where
  getBindingInfo :: forall x. f x -> BindingInfo x

-- getKids :: forall x r. f x r -> 
-- rename ::
--   forall c f x y env.
--   BindSite f =>
--   (c (f x) -> State env (c (f x))) ->
--   (x -> y) -> c (f x) -> c (f y)
-- rename xxx f = ?a

-- rename :: forall f x y. Binding f => (x -> y) -> f x  -> f y
-- rename f a = ?a
