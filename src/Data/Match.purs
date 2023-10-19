module Data.Match where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (WriterT, execWriterT, mapWriterT, tell)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Text.Pretty (class Pretty, class PrettyS, pretty)
import Util (fromJust, fromJust')

-- Match

-- | The type of matching functions such that "`a` is matched by `TreePattern`s with
-- | contructor `a`, with resulting matches of type `m`".
type Match a pat m = pat -> a -> MatchM m
type MatchM m = WriterT (Array m) Maybe Unit

matches :: forall a pat m b. Match a pat m -> Array (pat /\ (Array m -> Maybe b)) -> a -> b
matches match cases a = case Array.uncons cases of
  Nothing -> bug $ "matches: no patterns matched"
  Just {head: pat /\ k, tail: cases'} -> case execWriterT $ match pat a of
    Nothing -> matches match cases' a
    Just m -> case k m of
      Nothing -> matches match cases' a
      Just b -> b

noMatch :: forall m. MatchM m
noMatch = throwError unit

mapMatches :: forall m m'. (m -> m') -> MatchM m -> MatchM m'
mapMatches f = mapWriterT (map (map (map f)))

-- Matchable

class Matchable a pat m | pat -> a m where
  match :: Match a pat m

-- EqPattern

newtype EqPattern a = EqPattern a

derive instance Generic (EqPattern a) _
derive newtype instance Show a => Show (EqPattern a)

instance Eq a => Matchable a (EqPattern a) m where
  match (EqPattern a) a' = unless (a == a') $ noMatch

-- WildPattern

newtype WildPattern a = WildPattern (Maybe a)

derive instance Generic (WildPattern a) _
derive newtype instance Show a => Show (WildPattern a)

instance Matchable a a' m => Matchable a (WildPattern a') m where
  match (WildPattern Nothing) _ = pure unit
  match (WildPattern (Just a)) a' = match a a'

-- Matchable Tree

data TreePattern a
  = TreePattern a (Array (TreePattern a))
  | VarTreePattern

derive instance Generic (TreePattern a) _
instance Show a => Show (TreePattern a) where show x = genericShow x

instance PrettyTreeNode a => Pretty (TreePattern a) where
  pretty = case _ of
    TreePattern a kids -> prettyTreeNode a (pretty <$> kids)
    VarTreePattern -> "var"

instance Matchable a a' m => Matchable (Tree a) (TreePattern a') (Tree a \/ m) where
  match (TreePattern a kids) (Tree a' kids') = do
    mapMatches Right $ match a a'
    uncurry match `traverse_` Array.zip kids kids'
  match VarTreePattern t = tell [Left t]

-- Matchable Tooth

data ToothPattern a
  = ToothPattern a (EqPattern Int) (Array (TreePattern a))

derive instance Generic (ToothPattern a) _
instance Show a => Show (ToothPattern a) where show x = genericShow x

instance PrettyTreeNode a => PrettyS (ToothPattern a) where
  prettyS (ToothPattern a (EqPattern i) kids) str = prettyTreeNode a (fromJust $ Array.insertAt i str $ pretty <$> kids)

instance Matchable a a' m => Matchable (Tooth a) (ToothPattern a') (Tree a \/ m) where
  match (ToothPattern a i kids) (Tooth a' i' kids') = do
    mapMatches Right $ match a a'
    match i i'
    uncurry match `traverse_` Array.zip kids kids'

-- matchChange

data ChangePattern a
  = ShiftPattern (EqPattern ShiftSign) (ToothPattern a) (ChangePattern a)
  | ReplacePattern (TreePattern a) (TreePattern a)
  | ChangePattern a (Array (ChangePattern a))

derive instance Generic (ChangePattern a) _
instance Show a => Show (ChangePattern a) where show x = genericShow x

instance Matchable a a' m => Matchable (Change a) (ChangePattern a') (Tree a \/ m) where
  match (ShiftPattern s th c) (Shift s' th' c') = do
    match s s'
    match th th'
    match c c'
  match (ReplacePattern c1 c2) (Replace c1' c2') = do
    match c1 c1'
    match c2 c2'
  match (ChangePattern a cs) (Change a' cs') = do
    mapMatches Right $ match a a'
    uncurry match `traverse_` Array.zip cs cs'
  match _ _ = noMatch