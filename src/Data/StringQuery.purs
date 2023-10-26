module Data.StringQuery where

import Prelude

import Data.Array as Array
import Data.Fuzzy as Fuzzy
import Data.Newtype (class Newtype)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))

newtype StringQuery a p = StringQuery
  { getItems :: String -> Array a
  , penalty :: String -> a -> p
  , maxPenalty :: p }

derive instance Newtype (StringQuery a p) _

getUnprioritizedItems str (StringQuery q) = q.getItems str

getAnnotatedItems str (StringQuery q) = q.getItems str <#> \item -> item /\ q.penalty str item

getPrioritizedItems str (StringQuery q) = StringQuery q #
  getAnnotatedItems str >>>
  -- sort by increasing penanty
  Array.sortBy (\(_ /\ p1) (_ /\ p2) -> compare p1 p2) >>>
  -- truncate items with too high penalty
  Array.takeWhile (\(_ /\ p) -> p <= q.maxPenalty) >>>
  -- erase penalties
  map fst

fuzzy {maxPenalty, toString, getItems} = StringQuery
  { getItems
  , penalty: \str a -> 
      let Fuzzy.FuzzyStr fuzzyStr = Fuzzy.matchStr true str (toString a) in
      fuzzyStr.distance
  , maxPenalty }
