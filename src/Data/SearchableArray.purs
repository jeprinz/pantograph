module Data.SearchableArray where

import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.Tuple (fst)

newtype SearchableArray a = SearchableArray
  { items :: Array a
  , priority :: String -> a -> Int }

annotatePriorities :: forall a. String -> SearchableArray a -> Array (a /\ Int)
annotatePriorities query (SearchableArray sa) = sa.items <#> \item -> item /\ sa.priority query item

prioritizedItems :: forall a. String -> SearchableArray a -> Array a
prioritizedItems query (SearchableArray sa) = 
  let annItems = annotatePriorities query (SearchableArray sa) in
  let sortedAnnItems = Array.sortBy (\(_ /\ p1) (_ /\ p2) -> compare p1 p2) annItems in
  sortedAnnItems <#> fst
