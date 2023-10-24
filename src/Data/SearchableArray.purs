module Data.SearchableArray where

import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Fuzzy as Fuzzy
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (fst)

newtype SearchableArray a p = SearchableArray
  { items :: Array a
  , penalty :: String -> a -> p
  , maxPenalty :: p }

derive instance Newtype (SearchableArray a p) _

unprioritizedItems :: forall a p. SearchableArray a p -> Array a
unprioritizedItems = unwrap >>> _.items

annotatePenalties :: forall a p. String -> SearchableArray a p -> Array (a /\ p)
annotatePenalties query (SearchableArray sa) = sa.items <#> \item -> item /\ sa.penalty query item

prioritizedItems :: forall a p. Ord p => String -> SearchableArray a p -> Array a
prioritizedItems query (SearchableArray sa) = (SearchableArray sa) #
  annotatePenalties query >>> 
  -- sort by increasing penalty
  Array.sortBy (\(_ /\ p1) (_ /\ p2) -> compare p1 p2) >>>
  -- truncate items with too high penalty
  Array.takeWhile (\(_ /\ p) -> p <= sa.maxPenalty) >>>
  -- erase penalties
  map fst

fuzzy :: forall a. Fuzzy.Distance -> (a -> String) -> Array a -> SearchableArray a Fuzzy.Distance
fuzzy maxPenalty toString items = SearchableArray 
  { items
  , penalty: \input a -> 
      let Fuzzy.FuzzyStr fuzzyStr = Fuzzy.matchStr true input (toString a) in 
      fuzzyStr.distance
  , maxPenalty }
