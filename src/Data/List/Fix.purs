module Data.List.Fix where

import Prelude

import Data.Fix (Fix(..), mapFix)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type ListFix a = Fix (Compose Maybe (Tuple a))

nil :: forall a. ListFix a
nil = Fix (Compose Nothing)

cons :: forall a. a -> ListFix a -> ListFix a
cons x xs = Fix (Compose (Just (Tuple x xs)))

mapListFix :: forall a b. (a -> b) -> ListFix a -> ListFix b
mapListFix _ (Fix (Compose Nothing)) = Fix (Compose Nothing)
mapListFix f (Fix (Compose (Just (Tuple x xs)))) = Fix (Compose (Just (Tuple (f x) (mapListFix f xs))))
