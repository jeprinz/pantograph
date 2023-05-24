module Utility where

import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))

map2 f = map (map f)
infixl 4 map2 as <$$>

map3 f = map (map (map f))
infixl 4 map3 as <$$$>

stripSuffix :: forall a. Eq a => List.Pattern a -> List a -> Maybe (List a)
stripSuffix (List.Pattern Nil) xs = Just xs
stripSuffix (List.Pattern suf) xs0 = go Nil xs0
  where
  go _ Nil = Nothing
  go ys (x : xs)
    | suf == xs = Just (List.reverse (x : ys))
    | otherwise = go (x : ys) xs
