module Data.Tree.Common where

import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Foldable (class Foldable, and)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (fromFoldable, snoc, unsnoc) as NonEmptyList
import Data.List.Types (NonEmptyList(..))
import Data.List.Types (nelCons) as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Hole (hole)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, class PrettyS, parens, pretty, prettyS, (<+>))
import Text.Pretty as Pretty
import Util (fromJust, fromJust', indexDeleteAt)

-- Tree

data Tree a = Tree a (Array (Tree a))
derive instance Generic (Tree a) _
instance Show a => Show (Tree a) where show x = genericShow x
derive instance Eq a => Eq (Tree a)
derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

treeNode :: forall a. Tree a -> a
treeNode (Tree a _) = a

-- Tooth

data Tooth a = Tooth a Int (Array (Tree a))
derive instance Generic (Tooth a) _
instance Show a => Show (Tooth a) where show x = genericShow x
derive instance Eq a => Eq (Tooth a)
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

toothNode :: forall a. Tooth a -> a
toothNode (Tooth a _ _) = a

tooths :: forall a. Tree a -> Array (Tooth a /\ Tree a)
tooths (Tree a kids) = kids # Array.mapWithIndex \i inside -> (Tooth a i (fromJust' "tooths" $ Array.deleteAt i kids) /\ inside)

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth a i kids) kid = Tree a (fromJust' "unTooth" $ Array.insertAt i kid kids)

-- Path

newtype Path a = Path (List (Tooth a))
derive instance Generic (Path a) _
instance Show a => Show (Path a) where show x = genericShow x
derive instance Eq a => Eq (Path a)
derive instance Functor Path
derive instance Foldable Path
derive instance Traversable Path
instance Semigroup (Path a) where append (Path ts1) (Path ts2) = Path (ts2 <> ts1)
derive newtype instance Monoid (Path a)

consPath :: forall a. Path a -> Tooth a -> Path a
consPath (Path as) a = Path (Cons a as)

unPath :: forall a. Path a -> Tree a -> Tree a
unPath (Path Nil) tree = tree
unPath (Path (Cons t ts)) tree = unPath (Path ts) (unTooth t tree)

unconsPath :: forall a. Path a -> Maybe {outer :: Path a, inner :: Tooth a}
unconsPath (Path Nil) = Nothing
unconsPath (Path (Cons t ts)) = Just {outer: Path ts, inner: t}

unsnocPath :: forall a. Path a -> Maybe {outer :: Tooth a, inner :: Path a}
unsnocPath (Path ts) = do
  {init, last} <- List.unsnoc ts
  pure {outer: last, inner: Path init}

-- NonEmptyPath

newtype NonEmptyPath a = NonEmptyPath (NonEmptyList (Tooth a))
derive instance Generic (NonEmptyPath a) _
instance Show a => Show (NonEmptyPath a) where show x = genericShow x
derive instance Eq a => Eq (NonEmptyPath a)
derive instance Functor NonEmptyPath
derive instance Foldable NonEmptyPath
derive instance Traversable NonEmptyPath
instance Semigroup (NonEmptyPath a) where append (NonEmptyPath ts1) (NonEmptyPath ts2) = NonEmptyPath (ts2 <> ts1)

toPath :: forall a. NonEmptyPath a -> Path a
toPath (NonEmptyPath ts) = Path (List.fromFoldable ts)

fromPath :: forall a. String -> Path a -> NonEmptyPath a
fromPath msg (Path Nil) = bug $ "[fromPath] null path: " <> msg
fromPath _ (Path (Cons t ts)) = NonEmptyPath (NonEmptyList (t NonEmpty.:| ts))

fromPathMaybe :: forall a. Path a -> Maybe (NonEmptyPath a)
fromPathMaybe (Path ts) = NonEmptyPath <$> NonEmptyList.fromFoldable ts

unconsNonEmptyPath :: forall a. NonEmptyPath a -> {outer :: Maybe (NonEmptyPath a), inner :: Tooth a}
unconsNonEmptyPath (NonEmptyPath (NonEmptyList ts)) = case ts of
  NonEmpty t Nil -> {outer: Nothing, inner: t}
  NonEmpty t (Cons t' ts') -> {outer: Just $ NonEmptyPath $ NonEmptyList (NonEmpty t' ts'), inner: t}

unsnocNonEmptyPath :: forall a. NonEmptyPath a -> {outer :: Tooth a, inner :: Maybe (NonEmptyPath a)}
unsnocNonEmptyPath (NonEmptyPath ts) =
  let {init, last} = NonEmptyList.unsnoc ts in
  {outer: last, inner: NonEmptyPath <$> NonEmptyList.fromFoldable init}

consNonEmptyPath :: forall a. NonEmptyPath a -> Tooth a -> NonEmptyPath a
consNonEmptyPath (NonEmptyPath ts) t = NonEmptyPath (NonEmptyList.nelCons t ts)

snocNonEmptyPath :: forall a. Tooth a -> NonEmptyPath a -> NonEmptyPath a
snocNonEmptyPath t (NonEmptyPath ts) = NonEmptyPath (NonEmptyList.snoc ts t)

singletonNonEmptyPath :: forall a. Tooth a -> NonEmptyPath a
singletonNonEmptyPath tooth = NonEmptyPath (NonEmptyList (tooth NonEmpty.:| Nil))

nonEmptyPathOuterNode :: forall a. NonEmptyPath a -> a
nonEmptyPathOuterNode p = toothNode (unsnocNonEmptyPath p).outer

nonEmptyPathInnerNode :: forall a. NonEmptyPath a -> a
nonEmptyPathInnerNode p = toothNode (unconsNonEmptyPath p).inner

-- Cursor

newtype Cursor a = Cursor {outside :: Path a, inside :: Tree a, orientation :: Orientation}
derive instance Generic (Cursor a) _
instance Show a => Show (Cursor a) where show x = genericShow x
derive instance Eq a => Eq (Cursor a)
derive instance Functor Cursor

-- Select

newtype Select a = Select {outside :: Path a, middle :: NonEmptyPath a, inside :: Tree a, orientation :: Orientation}
derive instance Generic (Select a) _
instance Show a => Show (Select a) where show x = genericShow x
derive instance Eq a => Eq (Select a)
derive instance Functor Select

-- Orientation

data Orientation = Outside | Inside
derive instance Generic Orientation _
instance Show Orientation where show = genericShow
derive instance Eq Orientation
derive instance Ord Orientation

-- Gyro

data Gyro a = RootGyro (Tree a) | CursorGyro (Cursor a) | SelectGyro (Select a)
derive instance Generic (Gyro a) _
instance Show a => Show (Gyro a) where show x = genericShow x
derive instance Eq a => Eq (Gyro a)
derive instance Functor Gyro

gyroNode :: forall a. Gyro a -> a
gyroNode (RootGyro (Tree a _)) = a
gyroNode (CursorGyro (Cursor {inside: Tree a _})) = a
gyroNode (SelectGyro (Select {middle, orientation: Outside})) | {inner: Tooth a _ _} <- unconsNonEmptyPath middle = a
gyroNode (SelectGyro (Select {inside: Tree a _, orientation: Inside})) = a

-- Change

data Change a
  = Shift ShiftSign (Tooth a) (Change a)
  | Replace (Tree a) (Tree a)
  | Change a (Array (Change a))
derive instance Generic (Change a) _
instance Show a => Show (Change a) where show x = genericShow x
derive instance Eq a => Eq (Change a)
derive instance Functor Change

-- `Change` forms a semigroup under composition.
instance Eq a => Semigroup (Change a) where
  append (Shift Plus th c) (Shift Minus th' c') | th == th' =
    c <> c'
  append (Shift Minus th@(Tooth a i ts) c) (Shift Plus th' c') | th == th' =
    Change a $ fromJust $ Array.insertAt i (c <> c') $ map injectChange ts
  append c (Shift Plus th c') = Shift Plus th (c <> c')
  append (Shift Minus th c) c' = Shift Minus th (c <> c')
  append (Shift Plus th@(Tooth a i ts) c) (Change a' _cs') 
    | a == a'
    , cs' /\ c' <- fromJust $ indexDeleteAt i _cs'
    , and $ map (uncurry eq) $ Array.zip (injectChange <$> ts) cs'
    = Shift Plus th (c <> c')
  append (Change a _cs) (Shift Minus th@(Tooth a' i' ts') c')
    | a == a'
    , cs /\ c <- fromJust $ indexDeleteAt i' _cs
    , and $ map (uncurry eq) $ Array.zip cs (injectChange <$> ts')
    = Shift Plus th (c <> c')
  append (Change a cs) (Change a' cs') | a == a' = Change a (Array.zipWith append cs cs')
  append c1 c2 = Replace (endpoints c1).left (endpoints c2).right

injectChange :: forall a. Tree a -> Change a
injectChange (Tree a kids) = Change a (injectChange <$> kids)

endpoints :: forall a. Change a -> {left :: Tree a, right :: Tree a}
endpoints (Shift Plus tooth kid) =
  let {left, right} = endpoints kid in
  {left, right: unTooth tooth right}
endpoints (Shift Minus tooth kid) =
  let {left, right} = endpoints kid in
  {left: unTooth tooth left, right}
endpoints (Replace old new) = {left: old, right: new}
endpoints (Change a kids) = 
  let kids' = endpoints <$> kids in
  let leftKids /\ rightKids = Array.unzip $ map (\{left, right} -> left /\ right) kids' in
  {left: Tree a leftKids, right: Tree a rightKids}

data ShiftSign = Plus | Minus
derive instance Generic ShiftSign _
instance Show ShiftSign where show = genericShow
derive instance Eq ShiftSign
derive instance Ord ShiftSign

instance Pretty ShiftSign where
  pretty Plus = "+"
  pretty Minus = "-"

plusChange th ch = Shift Plus th ch
minusChange th ch = Shift Minus th ch
treeChange a kids = Change a kids
replaceChange old new = Replace old new

-- Edit

data Edit a b
  = InsertEdit {outerChange :: Change a, middle :: NonEmptyPath b, innerChange :: Change a}
  | ReplaceEdit {outerChange :: Change a, inside :: Tree b}
derive instance Generic (Edit a b) _
instance (Show a, Show b) => Show (Edit a b) where show = genericShow
derive instance (Eq a, Eq b) => Eq (Edit a b)
derive instance Bifunctor Edit

-- TreeNode

class TreeNode a where
  kidsCount :: a -> Int

assertValidTreeKids :: forall a b c. TreeNode a => String -> a -> (Partial => Array b -> c) -> Array b -> c
assertValidTreeKids msg a k bs = unsafePartial
  if kidsCount a == Array.length bs
    then k bs
    else bug $ "invalid tree kids: " <> msg

-- Pretty

class TreeNode a <= PrettyTreeNode a where
  prettyTreeNode :: a -> Array String -> String

instance PrettyTreeNode a => Pretty (Tree a) where
  pretty (Tree a kids) = prettyTreeNode a (pretty <$> kids)

instance PrettyTreeNode a => PrettyS (Tooth a) where
  prettyS (Tooth a i kids) str = prettyTreeNode a (fromJust' "(PrettyS (Tooth a)).prettyS" $ Array.insertAt i str (pretty <$> kids))

instance PrettyTreeNode a => Pretty (Tooth a) where
  pretty tooth = prettyS tooth Pretty.cursor

instance PrettyTreeNode a => PrettyS (Path a) where
  prettyS (Path ts) = go ts
    where
    go Nil str = str
    go (Cons t ts') str = go ts' (prettyS t str)

instance PrettyTreeNode a => Pretty (Path a) where
  pretty path = prettyS path Pretty.cursor

instance PrettyTreeNode a => Pretty (NonEmptyPath a) where
  pretty nonEmptyPath = prettyS (toPath nonEmptyPath) Pretty.cursor

instance PrettyTreeNode a => Pretty (Cursor a) where
  pretty (Cursor {outside, inside, orientation}) = case orientation of
    Outside -> prettyS outside $ Pretty.outer $ pretty inside
    Inside -> prettyS outside $ Pretty.inner $ pretty inside

instance PrettyTreeNode a => Pretty (Select a) where
  pretty (Select {outside, middle, inside, orientation: Outside}) =
    prettyS outside $ Pretty.outerActive $ prettyS (toPath middle) $ Pretty.inner $ pretty inside
  pretty (Select {outside, middle, inside, orientation: Inside}) =
    prettyS outside $ Pretty.outer $ prettyS (toPath middle) $ Pretty.innerActive $ pretty inside

instance PrettyTreeNode a => Pretty (Gyro a) where
  pretty (RootGyro tree) = pretty tree
  pretty (CursorGyro cursor) = pretty cursor
  pretty (SelectGyro select) = pretty select

instance PrettyTreeNode a => Pretty (Change a) where
  pretty (Shift sign tooth kid) = pretty sign <> (Pretty.outer (prettyS tooth (Pretty.inner (pretty kid))))
  pretty (Replace old new) = parens (pretty old <+> "~~>" <+> pretty new)
  pretty (Change a kids) = prettyTreeNode a (pretty <$> kids)
