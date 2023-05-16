module Data.Expr where

import Data.Either
import Data.Either.Nested
import Prelude
import Type.Direction

import Bug (bug)
import Bug.Assertion (Assertion(..), assert, assertInput_, assertM, assert_)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const(..))
import Data.Enum (enumFromTo)
import Data.Eq (class Eq1, eq1)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, intercalate, sequence_)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Ord (class Ord1, compare1)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.TotalMap (hasKey)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Data.Variant (case_, on)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Text.Pretty (class Pretty, braces, braces2, parens, pretty, quotes)
import Text.Pretty as Pretty
import Type.Direction as Dir
import Type.Proxy (Proxy(..))

-- | Expr

data Expr l = Expr l (Array (Expr l))
type ExprF l kid = l /\ Array kid

infixl 7 Expr as %

toExprF :: forall l. Expr l -> ExprF l (Expr l)
toExprF (Expr l es) = l /\ es

fromExprF :: forall l. ExprF l (Expr l) -> Expr l
fromExprF (l /\ es) = Expr l es

derive instance Generic (Expr l) _
instance Show l => Show (Expr l) where show x = genericShow x
instance Eq l => Eq (Expr l) where eq x y = genericEq x y
instance Ord l => Ord (Expr l) where compare x y = genericCompare x y
derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr

class (Eq l, Ord l, Show l, Pretty l) <= IsExprLabel l where
  prettyExprF'_unsafe :: Partial => ExprF l String -> String
  expectedKidsCount :: l -> Int

wellformedExprF :: forall l kid. String -> (kid -> String) -> IsExprLabel l => ExprF l kid -> Assertion Unit
wellformedExprF source showKid (l /\ kids) = Assertion
  { name: "wellformedExprF"
  , source
  , result:
      if expectedKidsCount l == Array.length kids 
        then Right unit
        else Left $ "An expression with ExprLabel " <> quotes (show l) <> " is expected to have " <> show (expectedKidsCount l) <> " kids, but an instance of it actually has " <> show (Foldable.length kids :: Int) <> " kids:" <> Pretty.bullets (showKid <$> kids)
  }

wellformedExpr :: forall l. IsExprLabel l => String -> Expr l -> Assertion Unit
wellformedExpr source (Expr l kids) = wellformedExprF source pretty (l /\ kids)

prettyExprF :: forall l. IsExprLabel l => ExprF l String -> String
prettyExprF e@(l /\ es) = assert (wellformedExprF "prettyExprF" identity e) \_ ->
  prettyExprF'_unsafe (l /\ (pretty <$> es))

instance IsExprLabel l => Pretty (Expr l) where
  pretty (Expr l es) = prettyExprF (l /\ (pretty <$> es))

-- | MetaVar

newtype MetaVar = MetaVar (Maybe String /\ UUID)

derive newtype instance Show MetaVar
derive newtype instance Eq MetaVar
derive newtype instance Ord MetaVar
-- instance Pretty MetaVar where pretty (MetaVar uuid) = "?" <> String.take 2 (UUID.toString uuid)
instance Pretty MetaVar where 
  pretty (MetaVar (Nothing /\ uuid)) = "?" <> String.take 2 (UUID.toString uuid)
  pretty (MetaVar (Just str /\ uuid)) = "?" <> str <> "~" <> String.take 2 (UUID.toString uuid)

showMetaVar :: MetaVar -> String
showMetaVar (MetaVar str) = show str

freshMetaVar :: String -> MetaVar
freshMetaVar str = MetaVar (Just str /\ unsafePerformEffect (UUID.genUUID))

freshMetaVar' :: Unit -> MetaVar 
freshMetaVar' _ = MetaVar (Nothing /\ unsafePerformEffect (UUID.genUUID))

newtype Meta a = Meta (MetaVar \/ a)
derive instance Newtype (Meta a) _
derive newtype instance Show a => Show (Meta a)
derive newtype instance Eq a => Eq (Meta a)
derive newtype instance Ord a => Ord (Meta a)
derive newtype instance Functor Meta
derive newtype instance Apply Meta
derive newtype instance Applicative Meta
derive newtype instance Foldable Meta
derive newtype instance Traversable Meta
instance Pretty a => Pretty (Meta a) where pretty = Newtype.unwrap >>> either pretty pretty

instance IsExprLabel l => IsExprLabel (Meta l) where
  prettyExprF'_unsafe ((Meta (Left x)) /\ _kids) = pretty x
  prettyExprF'_unsafe ((Meta (Right l)) /\ kids) = prettyExprF (l /\ kids)

  expectedKidsCount (Meta (Left _)) = 0
  expectedKidsCount (Meta (Right l)) = expectedKidsCount l

type MetaVarSub a = Map.Map MetaVar a

subMetaVars :: forall l. IsExprLabel l => MetaVarSub (Expr l) -> MetaExpr l -> Expr l
subMetaVars sigma = assertInput_ (wellformedExpr "subMetaVars") go
  where
  go :: Partial => _
  go = case _ of
    Meta (Left mx) % [] -> assert (hasKey "subMetaVars" mx sigma) identity
    Meta (Right l) % kids -> l % (go <$> kids)

subSomeMetaVars :: forall l. IsExprLabel l => MetaVarSub (MetaExpr l) -> MetaExpr l -> MetaExpr l
subSomeMetaVars sigma = assertInput_ (wellformedExpr "subSomeMetaVars") go
  where
  go :: Partial => _
  go = case _ of
    Meta (Left mx) % [] -> case Map.lookup mx sigma of
      Nothing -> Meta (Left mx) % []
      Just mexpr -> mexpr
    Meta (Right l) % kids -> Meta (Right l) % (go <$> kids)

-- | MetaExpr

type MetaExpr l = Expr (Meta l)

fromMetaVar :: forall l. MetaVar -> MetaExpr l
fromMetaVar mx = Meta (Left mx) % []

-- | Tooth

data Tooth l = Tooth l (ZipList.Path (Expr l))

infixl 7 Tooth as %<

derive instance Generic (Tooth l) _
instance Show l => Show (Tooth l) where show x = genericShow x
instance Eq l => Eq (Tooth l) where eq x y = genericEq x y
instance Ord l => Ord (Tooth l) where compare x y = genericCompare x y
derive instance Functor Tooth
derive instance Foldable Tooth
derive instance Traversable Tooth

instance IsExprLabel l => Pretty (Tooth l) where
  pretty th = prettyTooth th "⌶"

foldMapTooth :: forall l m. Monoid m => (l -> m) -> Tooth l -> m
foldMapTooth f (l %< p) = f l <> foldMap (foldMap f) p

foldlTooth :: forall l a b. (b -> a -> b) -> b -> Tooth a -> b
foldlTooth f b (l %< p) = ZipList.foldlAround (foldl f) b (flip f l) p

foldrTooth :: forall l a b. (a -> b -> b) -> b -> Tooth a -> b
foldrTooth f b (l %< p) = ZipList.foldrAround (flip (foldr f)) b (f l) p

traverseTooth :: forall l l' m. Applicative m => (l -> m l') -> Tooth l -> m (Tooth l')
traverseTooth f (l %< p) = Tooth <$> f l <*> traverse (traverse f) p

unTooth (l %< p) g = l % Array.fromFoldable (ZipList.unpathAround g p)

tooth :: forall l. Int -> Expr l -> Maybe (Tooth l /\ Expr l)
tooth i (Expr l kids) = do
  kidsZip /\ kid <- ZipList.zipAt i (List.fromFoldable kids)
  Just $ (l %< kidsZip) /\ kid

showTooth (l %< p) = show l <> " " <> intercalate " " (ZipList.unpathAround "{}" (show <$> p))

prettyTooth (l %< p) str = prettyExprF (l /\ Array.fromFoldable (ZipList.unpathAround str (pretty <$> p)))

-- | Path

newtype Path (dir :: Symbol) l = Path (List (Tooth l))

derive instance Newtype (Path dir l) _
derive newtype instance Show l => Show (Path dir l)
derive newtype instance Eq l => Eq (Path dir l)
derive newtype instance (Eq l, Ord l) => Ord (Path dir l)
derive instance Functor (Path dir)
derive instance Foldable (Path dir)
derive instance Traversable (Path dir)

-- For getting the direction of a path as a value, without having to keep a
-- value stored in the path that is maintained to correspond to the type's dir
-- annotation
class ReflectPathDir (dir :: Symbol) where 
  reflectPathDir :: forall l. Path dir l -> Dir.VerticalDir
instance ReflectPathDir Dir.Down where reflectPathDir _ = upDir
instance ReflectPathDir Dir.Up where reflectPathDir _ = downDir

-- | This works on both up and down paths
prettyPath path str = foldMapPath str prettyTooth path

instance (ReflectPathDir dir, IsExprLabel l) => Pretty (Path dir l) where pretty path = prettyPath path "⌶"

stepPath :: forall dir l. Tooth l -> Path dir l -> Path dir l
stepPath th (Path ths) = Path (th : ths)

unstepPath :: forall dir l. Path dir l -> Maybe (Tooth l /\ Path dir l)
unstepPath (Path Nil) = Nothing
unstepPath (Path (th : ths)) = Just (th /\ Path ths)

toUpPath :: forall dir l. ReflectPathDir dir => Path dir l -> Path Up l
toUpPath path@(Path ths) = (_ $ reflectPathDir path) $ case_
  # on _up (\_ -> Path ths)
  # on _down (\_ -> reversePath (Path ths :: Path Down l))

toDownPath :: forall dir l. ReflectPathDir dir => Path dir l -> Path Down l
toDownPath path@(Path ths) = (_ $ reflectPathDir path) $ case_
  # on _up (\_ -> reversePath (Path ths :: Path Up l))
  # on _down (\_ -> Path ths)

unPath :: forall dir l. ReflectPathDir dir => Path dir l -> Expr l -> Expr l
unPath path expr0 = do
  let Path ths = toUpPath path
  go ths expr0
  where
  go Nil expr = expr
  go (th : ths) expr = go ths (unTooth th expr)

foldMapMPath'_down nil cons = go
  where
  go Nil = nil
  go (th : ths) = cons th $ go ths

foldMapMPath'_up nil cons = go nil
  where
  go below Nil = below
  go below (th : ths) = go (cons th below) ths

-- | This works on both up and down paths
foldMapMPath nil cons path@(Path ths) = reflectPathDir path # 
  (case_
    # on _up (\_ -> foldMapMPath'_up nil cons ths)
    # on _down (\_ -> foldMapMPath'_down nil cons ths)
  )

foldMapPath nil cons = Newtype.unwrap <<< foldMapMPath (Identity $ nil) (\th (Identity a) -> Identity $ cons th a)

reversePath :: forall dir dir' l. Dir.Opposite dir dir' => Path dir l -> Path dir' l
reversePath (Path ths) = Path (List.reverse ths)

-- | Depending on direction:
-- |   - If dir == up, then path1 is below path2.
-- |   - If dir == down, then path1 is above path2
instance Semigroup (Path dir l) where
  append (Path ths1) (Path ths2) = Path (ths1 <> ths2)

instance Monoid (Path dir l) where
  mempty = Path Nil

-- | Zipper

newtype Zipper l = Zipper {path :: Path Dir.Up l, expr :: Expr l}

derive instance Newtype (Zipper l) _
derive newtype instance Show l => Show (Zipper l)
derive newtype instance Eq l => Eq (Zipper l)

instance IsExprLabel l => Pretty (Zipper l) where
  pretty (Zipper z) = prettyPath z.path $ pretty z.expr

zipUp :: forall l. Zipper l -> Maybe (Tooth l /\ Zipper l)
zipUp (Zipper z) = case z.path of
  Path Nil -> Nothing
  Path (th : ths) -> Just $ th /\ Zipper {path: Path ths, expr: unTooth th z.expr}

-- | Only zip down the kids in the tooth (not the interior of the tooth).
zipDownsTooth :: forall l. IsExprLabel l => Zipper l -> Tooth l -> ZipList.Path (Zipper l)
zipDownsTooth zipper (_ %< kidsPath) = do
  let ix = ZipList.leftLength kidsPath
  let zs = zipDowns zipper
  case ZipList.zipAt ix (List.fromFoldable zs) of
    Nothing -> bug "[zipDownsTooth] bad index"
    Just (zipsPath /\ _kidZip) -> snd <$> zipsPath

zipDowns :: forall l. Zipper l -> Array (Tooth l /\ Zipper l)
zipDowns (Zipper z) = do
  let l % kids0 = z.expr
  case Array.uncons kids0 of
    Nothing -> []
    Just {head: kid0, tail: kids1} ->
      go
        []
        (l %< ZipList.Path {left: mempty, right: List.fromFoldable kids1})
        kid0
  where
  go :: Array (Tooth l /\ Zipper l) -> Tooth l -> Expr l -> Array (Tooth l /\ Zipper l)
  go zippers th@(_ %< ZipList.Path {right: Nil}) kid =
    Array.reverse $ Array.cons (th /\ Zipper {path: stepPath th z.path, expr: kid}) zippers
  go zippers th@(l' %< ZipList.Path {left, right: Cons kid' kids'}) kid =
    go
      (Array.cons (th /\ Zipper {path: stepPath th z.path, expr: kid}) zippers)
      (l' %< ZipList.Path {left: RevList.snoc left kid, right: kids'})
      kid'

zipLeft :: forall l. Zipper l -> Maybe (Zipper l)
zipLeft (Zipper z) = case z.path of
  Path Nil -> Nothing
  Path (l %< kidsPath : ths) -> do
    expr' /\ kidsPath' <- ZipList.zipLeft (z.expr /\ kidsPath)
    Just $ Zipper {path: Path (l %< kidsPath' : ths), expr: expr'}

zipRight :: forall l. Zipper l -> Maybe (Zipper l)
zipRight (Zipper z) = case z.path of
  Path Nil -> Nothing
  Path (l %< kidsPath : ths) -> do
    expr' /\ kidsPath' <- ZipList.zipRight (z.expr /\ kidsPath)
    Just $ Zipper {path: Path (l %< kidsPath' : ths), expr: expr'}

-- | Zipperp

newtype Zipperp l = Zipperp
  { path :: Path Dir.Up l
  , selection :: Path Dir.Down l \/ Path Dir.Up l
  , expr :: Expr l}

derive instance Generic (Zipperp l) _
derive instance Newtype (Zipperp l) _
instance Show l => Show (Zipperp l) where show x = genericShow x

zipperpTopPath :: forall l. Zipperp l -> Path Up l
zipperpTopPath (Zipperp z') = z'.path

zipperpBottomPath :: forall l. Zipperp l -> Path Up l
zipperpBottomPath (Zipperp z') = either reversePath identity z'.selection <> z'.path

unzipperp :: forall l. Zipperp l -> Zipper l
unzipperp (Zipperp z') = case z'.selection of
  Left downPath -> Zipper
    { path: reversePath downPath <> z'.path
    , expr: z'.expr
    }
  Right upPath -> Zipper
    { path: z'.path
    , expr: unPath upPath z'.expr
    }

-- | Change

type Change l = Expr (ChangeLabel l)

data ChangeLabel l
  = Plus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Minus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Inject l {-same number of kids that l has-}
  | Replace (Expr l) (Expr l) {-zero kids?-}

derive instance Generic (ChangeLabel l) _
derive instance Eq l => Eq (ChangeLabel l)
derive instance Ord l => Ord (ChangeLabel l)
instance Show l => Show (ChangeLabel l) where show x = genericShow x

instance IsExprLabel l => Pretty (ChangeLabel l) where
  pretty (Plus th) = "(+ " <> prettyTooth th "{}" <> ")"
  pretty (Minus th) = "(- " <> prettyTooth  th "{}" <> ")"
  pretty (Inject l) = pretty l
  pretty (Replace e1 e2) = "(" <> pretty e1 <> "~~> " <> pretty e2 <> ")"

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

-- instance Foldable ChangeLabel where
--   foldMap f = case _ of
--     Plus th -> foldMapTooth f th
--     Minus th -> foldMapTooth f th
--     Inject l -> f l
--     Replace e1 e2 -> foldMap f e1 <> foldMap f e2
--   foldl f b = case _ of
--     Plus th -> foldlTooth f b th
--     Minus th -> foldlTooth f b th
--     Inject l -> f b l
--     Replace e1 e2 -> foldl f (foldl f b e1) e2
--   foldr f b = case _ of 
--     Plus th -> foldrTooth f b th
--     Minus th -> foldrTooth f b th
--     Inject l -> f l b
--     Replace e1 e2 -> foldr f (foldr f b e2) e1

-- instance Traversable ChangeLabel where
--   traverse f = case _ of
--     Plus th -> Plus <$> traverseTooth f th
--     Minus th -> Minus <$> traverseTooth f th
--     Inject l -> Inject <$> f l
--     Replace e1 e2 -> Replace <$> traverse f e1 <*> traverse f e2
--   sequence fa = sequenceDefault fa

instance IsExprLabel l => IsExprLabel (ChangeLabel l) where
  prettyExprF'_unsafe (Plus th /\ [kid]) = prettyTooth th kid
  prettyExprF'_unsafe (Minus th /\ [kid]) = prettyTooth th kid
  prettyExprF'_unsafe (Inject l /\ kids) = prettyExprF (l /\ kids)
  prettyExprF'_unsafe (Replace e1 e2 /\ []) = parens (pretty e1 <> " ~~> " <> pretty e2)

  expectedKidsCount (Plus _) = 1
  expectedKidsCount (Minus _) = 1
  expectedKidsCount (Inject l) = expectedKidsCount l
  expectedKidsCount (Replace _ _) = 0

type MetaChange l = Change (Meta l)

plusChange :: forall l. Tooth l -> Change l -> Change l
plusChange th ch = Plus th % [ch]

minusChange :: forall l. Tooth l -> Change l -> Change l
minusChange th ch = Minus th % [ch]

injectChange :: forall l. l -> Array (Change l) -> Change l
injectChange l chs = Inject l % chs

replaceChange :: forall l. Expr l -> Expr l -> Change l
replaceChange e1 e2 = Replace e1 e2 % []
