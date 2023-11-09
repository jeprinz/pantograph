module Data.Tree.Common where

import Prelude

import Data.Either.Nested
import Bug (bug)
import Data.Array as Array
import Data.Display (class Display, class DisplayS, Html, display, displayS)
import Data.Foldable (class Foldable, and)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty (fromFoldable, snoc, unsnoc) as NonEmptyList
import Data.List.Types (NonEmptyList(..))
import Data.List.Types (nelCons) as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Subtype (class Subtype)
import Data.Subtype as Subtype
import Data.Supertype (class Supertype)
import Data.Supertype as Supertype
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Halogen.Elements as El
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, class PrettyS, parens, pretty, prettyS, (<+>))
import Text.Pretty as Pretty
import Todo (todo)
import Util (fromJust, fromJust', extractAt)

-- infixes

infix 5 Tree as %
infix 5 Tooth as %-
infix 5 InjectChange as %!
infix 5 Shift as %!/
infix 5 Replace as %!~>

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

treeKids :: forall a. Tree a -> Array (Tree a)
treeKids (Tree _ kids) = kids

-- Tooth

data Tooth a = Tooth a (Int /\ (Array (Tree a)))
derive instance Generic (Tooth a) _
instance Show a => Show (Tooth a) where show x = genericShow x
derive instance Eq a => Eq (Tooth a)
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

toothNode :: forall a. Tooth a -> a
toothNode (Tooth a _) = a

toothIndex :: forall a. Tooth a -> Int
toothIndex (Tooth _ (i /\ _)) = i

toothKids :: forall a. Tooth a -> Array (Tree a)
toothKids (Tooth _ (_ /\ kids)) = kids

buildTooth :: forall a. a -> Array (Tree a) -> Array (Tree a) -> Tooth a
buildTooth a leftKids rightKids = Tooth a (Array.length leftKids /\ (leftKids <> rightKids))

tooths :: forall a. Tree a -> Array (Tooth a /\ Tree a)
tooths (Tree a kids) = kids # Array.mapWithIndex \i inside -> (Tooth a (i /\ (fromJust' "tooths" $ Array.deleteAt i kids)) /\ inside)

unTooth :: forall a. Tooth a -> Tree a -> Tree a
unTooth (Tooth a (i /\ kids)) kid = Tree a (fromJust' "unTooth" $ Array.insertAt i kid kids)

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

snocPath :: forall a. Tooth a -> Path a -> Path a
snocPath a (Path as) = Path (List.snoc as a)

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

-- Vertibra

data Vertibra a = Vertibra a Int

derive instance Generic (Vertibra a) _
instance Show a => Show (Vertibra a) where show x = genericShow x
derive instance Eq a => Eq (Vertibra a)
derive instance Functor Vertibra
derive instance Foldable Vertibra
derive instance Traversable Vertibra

-- Spine

newtype Spine a = Spine (List (Vertibra a))

derive instance Generic (Spine a) _
instance Show a => Show (Spine a) where show x = genericShow x
derive instance Eq a => Eq (Spine a)
derive instance Functor Spine
derive instance Foldable Spine
derive instance Traversable Spine
instance Semigroup (Spine a) where append (Spine vs1) (Spine vs2) = Spine (vs2 <> vs1)
derive newtype instance Monoid (Spine a)

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

fromOrientationToCursorClassName :: Orientation -> El.ClassName
fromOrientationToCursorClassName Outside = El.OutsideCursor
fromOrientationToCursorClassName Inside = El.InsideCursor

-- Gyro

data Gyro a = CursorGyro (Cursor a) | SelectGyro (Select a)
derive instance Generic (Gyro a) _
instance Show a => Show (Gyro a) where show x = genericShow x
derive instance Eq a => Eq (Gyro a)
derive instance Functor Gyro

gyroNode :: forall a. Gyro a -> a
gyroNode (CursorGyro (Cursor {inside: Tree a _})) = a
gyroNode (SelectGyro (Select {middle, orientation: Outside})) | {inner: Tooth a _} <- unconsNonEmptyPath middle = a
gyroNode (SelectGyro (Select {inside: Tree a _, orientation: Inside})) = a

-- If `ensureGyroIsCursor gyro == Nothing` then then `gyro` is already a
-- `Cursor`.
ensureGyroIsCursor :: forall a. PrettyTreeNode a => Gyro a -> Maybe (Gyro a)
ensureGyroIsCursor (CursorGyro _) = Nothing
ensureGyroIsCursor (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

escapeGyro :: forall a. Gyro a -> Maybe (Gyro a)
escapeGyro (CursorGyro _) = Nothing
escapeGyro (SelectGyro select) = Just $ CursorGyro (escapeSelect select)

escapeCursor :: forall a. Cursor a -> Tree a
escapeCursor (Cursor {outside, inside}) = unPath outside inside

escapeSelect :: forall a. Select a -> Cursor a
escapeSelect (Select {outside, middle, inside, orientation}) =
  case orientation of
    Outside -> Cursor {outside, inside: unPath (toPath middle) inside, orientation: Outside}
    Inside -> Cursor {outside: outside <> toPath middle, inside, orientation: Outside}

-- Change

data Change a
  = Shift (ShiftSign /\ Tooth a) (Change a)
  | Replace (Tree a) (Tree a)
  | InjectChange a (Array (Change a))
derive instance Generic (Change a) _
instance Show a => Show (Change a) where show x = genericShow x
derive instance Eq a => Eq (Change a)
derive instance Functor Change
derive instance Foldable Change
derive instance Traversable Change

-- `Change` forms a semigroup under composition.
instance Eq a => Semigroup (Change a) where
  append (Shift (Plus /\ th) c) (Shift (Minus /\ th') c') | th == th' =
    c <> c'
  append (Shift (Minus /\ th@(Tooth a (i /\ ts))) c) (Shift (Plus /\ th') c') | th == th' =
    InjectChange a $ fromJust $ Array.insertAt i (c <> c') $ map Supertype.inject ts
  append c (Shift (Plus /\ th) c') = Shift (Plus /\ th) (c <> c')
  append (Shift (Minus /\ th) c) c' = Shift (Minus /\ th) (c <> c')
  append (Shift (Plus /\ th@(Tooth a (i /\ ts))) c) (InjectChange a' _cs')
    | a == a'
    , cs' /\ c' <- fromJust $ extractAt i _cs'
    , and $ map (uncurry eq) $ Array.zip (Supertype.inject <$> ts) cs'
    = Shift (Plus /\ th) (c <> c')
  append (InjectChange a _cs) (Shift (Minus /\ th@(Tooth a' (i' /\ ts'))) c')
    | a == a'
    , cs /\ c <- fromJust $ extractAt i' _cs
    , and $ map (uncurry eq) $ Array.zip cs (Supertype.inject <$> ts')
    = Shift (Plus /\ th) (c <> c')
  append (InjectChange a cs) (InjectChange a' cs') | a == a' = InjectChange a (Array.zipWith append cs cs')
  append c1 c2 = Replace (endpoints c1).left (endpoints c2).right

instance Supertype (Change a) (Tree a) where
  inject (Tree a kids) = InjectChange a (Supertype.inject <$> kids)
  project = case _ of
    InjectChange a kids -> Tree a <$> Supertype.project `traverse` kids
    _ -> Nothing

-- Supertype.inject :: forall a. Tree a -> Change a
-- Supertype.inject (Tree a kids) = InjectChange a (Supertype.inject <$> kids)

-- projectTreeIntoChange :: forall a. Change a -> Maybe (Tree a)
-- projectTreeIntoChange = case _ of
--   InjectChange a kids -> Tree a <$> projectTreeIntoChange `traverse` kids
--   _ -> Nothing

endpoints :: forall a. Change a -> {left :: Tree a, right :: Tree a}
endpoints (Shift (Plus /\ tooth) kid) =
  let {left, right} = endpoints kid in
  {left, right: unTooth tooth right}
endpoints (Shift (Minus /\ tooth) kid) =
  let {left, right} = endpoints kid in
  {left: unTooth tooth left, right}
endpoints (Replace old new) = {left: old, right: new}
endpoints (InjectChange a kids) = 
  let kids' = endpoints <$> kids in
  let leftKids /\ rightKids = Array.unzip $ map (\{left, right} -> left /\ right) kids' in
  {left: Tree a leftKids, right: Tree a rightKids}

epL :: forall a. Change a -> Tree a
epL ch = (endpoints ch).left

epR :: forall a. Change a -> Tree a
epR ch = (endpoints ch).right

data ShiftSign = Plus | Minus
derive instance Generic ShiftSign _
instance Show ShiftSign where show = genericShow
derive instance Eq ShiftSign
derive instance Ord ShiftSign

instance Pretty ShiftSign where
  pretty Plus = "+"
  pretty Minus = "-"

-- TreeNode

class TreeNode a where
  validKidsCount :: a -> Int -> Boolean

assertValidTreeKids :: forall a b c. Show a => TreeNode a => String -> a -> (Partial => Array b -> c) -> Array b -> c
assertValidTreeKids msg a k bs = unsafePartial
  if validKidsCount a (Array.length bs)
    then k bs
    else bug $ 
      "assertValidTreeKids: " <> msg <>
      "; a = " <> show a <> 
      "; Array.length bs = " <> show (Array.length bs)

assertValidToothKids :: forall a b c. Show a => TreeNode a => String -> a -> Int -> (Partial => Array b -> c) -> Array b -> c
assertValidToothKids msg a i k bs = unsafePartial
  if validKidsCount a (Array.length bs + 1)
    then k bs
    else bug $
      "assertValidToothKids: " <> msg <>
      "; a = " <> show a <> 
      "; i = " <> show i <>
      "; Array.length bs = " <> show (Array.length bs)

-- Pretty

class TreeNode a <= PrettyTreeNode a where
  prettyTreeNode :: a -> Array String -> String

instance PrettyTreeNode a => Pretty (Tree a) where
  pretty (Tree a kids) = prettyTreeNode a (pretty <$> kids)

instance PrettyTreeNode a => PrettyS (Tooth a) where
  prettyS (Tooth a (i /\ kids)) str = prettyTreeNode a (fromJust' "(PrettyS (Tooth a)).prettyS" $ Array.insertAt i str (pretty <$> kids))

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
  pretty (CursorGyro cursor) = pretty cursor
  pretty (SelectGyro select) = pretty select

instance PrettyTreeNode a => Pretty (Change a) where
  pretty (Shift (sign /\ tooth) kid) = pretty sign <> (Pretty.outer (prettyS tooth (Pretty.inner (pretty kid))))
  pretty (Replace old new) = parens (pretty old <+> "~~>" <+> pretty new)
  pretty (InjectChange a kids) = prettyTreeNode a (pretty <$> kids)

class TreeNode a <= DisplayTreeNode a where
  displayTreeNode :: a -> Array (Maybe (Tree a) /\ Html) -> Html

instance DisplayTreeNode a => Display (Tree a) where
  display (Tree a kids) = displayTreeNode a (kids <#> \kid -> Just kid /\ display kid)

instance DisplayTreeNode a => DisplayS (Tooth a) where
  displayS (Tooth a (i /\ kids)) = \inner -> displayTreeNode a (fromJust $ Array.insertAt i (Nothing /\ inner) (kids <#> \kid -> Just kid /\ display kid))

instance DisplayTreeNode a => Display (Change a) where
  display (Shift (sh /\ th) ch) = 
    El.ℓ [El.Classes [El.ShiftChange]]
      [El.π $ pretty sh, displayS th $ El.ℓ [El.Classes [El.ShiftChangeInner]] [display ch]]
  display (Replace s1 s2) = 
    El.ℓ [El.Classes [El.ReplaceChange]]
      [ El.π "("
      , El.ℓ [El.Classes [El.ReplaceChangeLeft]] [display s1]
      , El.π " ~> "
      , El.ℓ [El.Classes [El.ReplaceChangeRight]] [display s2]
      , El.π ")" ]
  display (InjectChange a kids) =
    El.ℓ [El.Classes [El.InjectChange]]
      [displayTreeNode a (kids <#> \kid -> Nothing /\ display kid)]

-- | # Utilities

fromSelectGyroToCursor :: Gyro ~> Cursor
fromSelectGyroToCursor (CursorGyro cursor) = cursor
fromSelectGyroToCursor (SelectGyro select) = escapeSelect select

getPathInnerToothIndex :: forall a. Path a -> Maybe Int
getPathInnerToothIndex = unconsPath >=> \{inner: Tooth _ (i /\ _)} -> pure i

getTreeKidsLength :: forall a. Tree a -> Int
getTreeKidsLength (Tree _ kids) = Array.length kids

fromTreeToCursor :: Tree ~> Cursor
fromTreeToCursor tree = Cursor {outside: mempty, inside: tree, orientation: Outside}

fromTreeToGyro :: Tree ~> Gyro
fromTreeToGyro = fromTreeToCursor >>> CursorGyro
