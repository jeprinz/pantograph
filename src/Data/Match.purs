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
import Data.Tuple (uncurry)
import Text.Pretty (class Pretty, class PrettyS, pretty)
import Type.Proxy (Proxy)
import Util (fromJust)

-- Match

-- | The type of matching functions such that "`a` is matched by `TreePattern`s with
-- | contructor `a`, with resulting matches of type `m`".
type Match a pat m = pat -> a -> MatchM m
type MatchM m = WriterT (Array m) Maybe Unit

case_ :: forall a pat m b. Matchable a pat m => Array (pat /\ (Array m -> Maybe b)) -> a -> b
case_ cases a = case Array.uncons cases of
  Nothing -> bug $ "matches: no patterns matched"
  Just {head: pat /\ k, tail: cases'} -> case execWriterT $ match pat a of
    Nothing -> case_ cases' a
    Just m -> case k m of
      Nothing -> case_ cases' a
      Just b -> b

noMatch :: forall m. MatchM m
noMatch = throwError unit

addMatch :: forall m. m -> MatchM m
addMatch x = tell [x]

mapMatches :: forall m m'. (m -> m') -> MatchM m -> MatchM m'
mapMatches f = mapWriterT (map (map (map f)))

-- Matchable

class Matchable a pat m | pat -> a m where
  match :: Match a pat m

-- EqPattern

newtype EqPattern a = EqPattern a

derive instance Generic (EqPattern a) _
derive newtype instance Show a => Show (EqPattern a)
derive instance Functor EqPattern
instance Apply EqPattern where apply (EqPattern f) (EqPattern a) = EqPattern (f a)
instance Applicative EqPattern where pure = EqPattern

instance Eq a => Matchable a (EqPattern a) m where
  match (EqPattern a) a' = unless (a == a') $ noMatch

-- WildPattern

newtype WildPattern a = WildPattern (Maybe a)

derive instance Generic (WildPattern a) _
derive newtype instance Show a => Show (WildPattern a)
derive instance Functor WildPattern
instance Apply WildPattern where apply (WildPattern f) (WildPattern a) = WildPattern (f <*> a)
instance Applicative WildPattern where pure = WildPattern <<< pure

instance Matchable a a' m => Matchable a (WildPattern a') m where
  match (WildPattern Nothing) _ = pure unit
  match (WildPattern (Just a)) a' = match a a'

class HasWildPattern pat where wild :: pat
instance HasWildPattern (WildPattern a)  where wild = WildPattern Nothing

-- VarPattern

newtype VarPattern (a :: Type) = VarPattern (Maybe a)

derive instance Generic (VarPattern a) _
derive newtype instance Show a => Show (VarPattern a)
derive instance Functor VarPattern
instance Apply VarPattern where apply (VarPattern f) (VarPattern a) = VarPattern (f <*> a)
instance Applicative VarPattern where pure = VarPattern <<< pure

instance Matchable a (VarPattern a') (a \/ m) where
  match (VarPattern _) a = addMatch (Left a)

class HasVarPattern pat where var :: pat
instance HasVarPattern (VarPattern a) where var = VarPattern Nothing

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

type TreeMatch a m = Tree a \/ m

instance Matchable a a' m => Matchable (Tree a) (TreePattern a') (TreeMatch a m) where
  match (TreePattern a kids) (Tree a' kids') = do
    mapMatches Right $ match a a'
    uncurry match `traverse_` Array.zip kids kids'
  match VarTreePattern t = addMatch (Left t)

-- Matchable Tooth

data ToothPattern a
  = ToothPattern a (EqPattern Int) (Array (TreePattern a))

derive instance Generic (ToothPattern a) _
instance Show a => Show (ToothPattern a) where show x = genericShow x

instance PrettyTreeNode a => PrettyS (ToothPattern a) where
  prettyS (ToothPattern a (EqPattern i) kids) str = prettyTreeNode a (fromJust $ Array.insertAt i str $ pretty <$> kids)

type ToothMatch a m = Tree a \/ m

instance Matchable a a' m => Matchable (Tooth a) (ToothPattern a') (ToothMatch a m) where
  match (ToothPattern a i kids) (Tooth a' i' kids') = do
    mapMatches Right $ match a a'
    match i i'
    uncurry match `traverse_` Array.zip kids kids'

-- matchChange

data ChangePattern a
  = ShiftPattern (EqPattern ShiftSign) (ToothPattern a) (ChangePattern a)
  | ReplacePattern (TreePattern a) (TreePattern a)
  | ChangePattern a (Array (ChangePattern a))
  | VarChangePattern
  | WildChangePattern

derive instance Generic (ChangePattern a) _
instance Show a => Show (ChangePattern a) where show x = genericShow x

type ChangeMatch a m = Change a \/ Tree a \/ m

instance Matchable a a' m => Matchable (Change a) (ChangePattern a') (ChangeMatch a m) where
  match (ShiftPattern s th c) (Shift s' th' c') = do
    match s s'
    mapMatches pure $ match th th'
    match c c'
  match (ReplacePattern c1 c2) (Replace c1' c2') = do
    mapMatches pure $ match c1 c1'
    mapMatches pure $ match c2 c2'
  match (ChangePattern a cs) (Change a' cs') = do
    mapMatches (pure >>> pure) $ match a a'
    uncurry match `traverse_` Array.zip cs cs'
  match VarChangePattern c = addMatch (in1 c)
  match _ _ = noMatch

instance HasVarPattern (ChangePattern a) where var = VarChangePattern
instance HasWildPattern (ChangePattern a) where wild = WildChangePattern