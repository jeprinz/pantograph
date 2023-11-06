module Data.Expr where

import Data.Either
import Data.Either.Nested
import Prelude
import Type.Direction
import Utility

import Bug (bug)
import Bug.Assertion (Assertion(..), assert, assertInput_, assertM_, strictlyOrdered)
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
import Data.List (List(..), Pattern(..), (:))
import Data.List as List
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as Newtype
import Data.Ord (class Ord1, compare1)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.TotalMap (hasKey)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Data.Variant (case_, on)
import Data.Zippable (class Zippable)
import Data.Zippable as Zippable
import Debug (trace, traceM)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Hole (hole)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Text.Pretty as Pretty
import Type.Direction as Dir
import Type.Proxy (Proxy(..))
import Util (fromJust')
import Util as Util

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- IsExprLabel
--------------------------------------------------------------------------------

class (Eq l, Ord l, Show l, Pretty l) <= IsExprLabel l where
  -- !TODO rename to prettyExprF_unsafe
  prettyExprF'_unsafe :: Partial => ExprF l String -> String
  expectedKidsCount :: l -> Int

wellformedExprF :: forall l kid. IsExprLabel l => String -> (kid -> String) -> ExprF l kid -> Assertion Unit
wellformedExprF source showKid (l /\ kids) = Assertion
  { name: "wellformedExprF"
  , source
  , result:
      if expectedKidsCount l == Array.length kids 
        then Right unit
        else Left $ "An expression with ExprLabel " <> P.quotes (show l) <> " is expected to have " <> show (expectedKidsCount l) <> " kids, but an instance of it actually has " <> show (Foldable.length kids :: Int) <> " kids:" <> Pretty.bullets (showKid <$> kids)
  }

wellformedExpr :: forall l. IsExprLabel l => String -> Expr l -> Assertion Unit
wellformedExpr source (Expr l kids) = wellformedExprF source pretty (l /\ kids)

wellformedTooth :: forall l. IsExprLabel l => String -> Tooth l -> Assertion Unit
wellformedTooth source (Tooth l kidsPath) = wellformedExprF source identity (l /\ Array.fromFoldable (ZipList.unpathAround "⌶" (pretty <$> kidsPath)))

prettyExprF :: forall l. IsExprLabel l => ExprF l String -> String
prettyExprF e@(l /\ es) = assert (wellformedExprF "prettyExprF" identity e) \_ ->
  prettyExprF'_unsafe (l /\ (pretty <$> es))

instance IsExprLabel l => Pretty (Expr l) where
  pretty (Expr l es) = prettyExprF (l /\ (pretty <$> es))

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar 
  = MetaVar (Maybe String) UUID
  | RuleMetaVar String

metaVarName :: MetaVar -> String
metaVarName (RuleMetaVar s) = s
metaVarName (MetaVar (Just s) _) = s
metaVarName _ = "no_name"

derive instance Generic MetaVar _
instance Show MetaVar where show x = genericShow x
instance Eq MetaVar where eq x y = genericEq x y
instance Ord MetaVar where compare x y = genericCompare x y
instance Pretty MetaVar where
  pretty (MetaVar Nothing uuid) = "?" <> String.take 2 (UUID.toString uuid)
  pretty (MetaVar (Just str) uuid) = "?" <> str <> "#" <> String.take 2 (UUID.toString uuid)
  pretty (RuleMetaVar str) = "??" <> str

freshMetaVar :: String -> MetaVar
freshMetaVar str = MetaVar (Just str) (unsafePerformEffect (UUID.genUUID))

freshMetaVar' :: Unit -> MetaVar 
freshMetaVar' _ = MetaVar Nothing (unsafePerformEffect (UUID.genUUID))

freshenMetaVar :: MetaVar -> MetaVar
freshenMetaVar (MetaVar (Just str) _) = freshMetaVar str
freshenMetaVar (MetaVar Nothing _) = freshMetaVar' unit
freshenMetaVar (RuleMetaVar _str) = bug "[freshenMetaVar] Should never try to freshen a RuleMetaVar, since it should be substituted away in any instantiated Rule i.e. Derivation"
data Meta a = MV MetaVar | MInj a

derive instance Generic (Meta a) _
derive instance Functor Meta
--derive instance Applicative Meta
instance Show a => Show (Meta a) where show x = genericShow x
instance Eq a => Eq (Meta a) where eq x y = genericEq x y
instance Ord a => Ord (Meta a) where compare x y = genericCompare x y

instance Pretty a => Pretty (Meta a) where
    pretty =
        case _ of
        MV mv -> pretty mv
        MInj a -> pretty a

instance IsExprLabel l => IsExprLabel (Meta l) where
  prettyExprF'_unsafe (MV x /\ _kids) = pretty x
  prettyExprF'_unsafe (MInj l /\ kids) = prettyExprF (l /\ kids)

  expectedKidsCount (MV _) = 0
  expectedKidsCount (MInj l) = expectedKidsCount l

--------------------------------------------------------------------------------
-- Tooth
--------------------------------------------------------------------------------

data Tooth l = Tooth l (ZipList.Path (Expr l))

toothPath (Tooth _ p) = p
toothLabel (Tooth l _) = l

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

tooths :: forall l. Expr l -> List (Tooth l /\ Expr l)
tooths (Expr l kids) = case Array.uncons kids of
  Nothing -> mempty
  Just {head: kid, tail: kids'} ->
    let go kidsLeft kid kidsRight thExs = case kidsRight of
          Nil -> thExs
          Cons kid' kidsRight' -> 
            let kidsLeft' = RevList.snoc kidsLeft kid in
            go kidsLeft' kid' kidsRight' ((Tooth l (ZipList.Path {left: kidsLeft', right: kidsRight'}) /\ kid') : thExs)
    in go mempty kid (List.fromFoldable kids') (List.singleton (Tooth l (ZipList.Path {left: mempty, right: List.fromFoldable kids'}) /\ kid))

showTooth (l %< p) = show l <> " " <> intercalate " " (ZipList.unpathAround "{}" (show <$> p))

prettyTooth (l %< p) str = prettyExprF (l /\ Array.fromFoldable (ZipList.unpathAround str (pretty <$> p)))

--------------------------------------------------------------------------------
-- Path
--------------------------------------------------------------------------------

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
  inSaneEnglishIsItDown :: forall l. Path dir l -> Boolean -- Jacob: because I have no idea how to pattern match on variants
instance ReflectPathDir Dir.Down where
    reflectPathDir _ = upDir
    inSaneEnglishIsItDown _ = true
instance ReflectPathDir Dir.Up where
    reflectPathDir _ = downDir
    inSaneEnglishIsItDown _ = false

-- | This works on both up and down paths -- Jacob: no it doesn't, it only works on Down paths. It would display Up paths backwards.
prettyPath ∷ ∀ (dir190319372017 ∷ Symbol) (t2019 ∷ Type). ReflectPathDir dir190319372017 ⇒ IsExprLabel t2019 ⇒ Path dir190319372017 t2019 → String → String
prettyPath path str = foldMapPath str prettyTooth (toDownPath path)

instance (ReflectPathDir dir, IsExprLabel l) => Pretty (Path dir l) where
    pretty path =
        if (inSaneEnglishIsItDown path) then -- TODO: verify that this is the right way around
            prettyPath (toUpPath path) "⌶"
            else
            prettyPath path "⌶"

stepPath :: forall dir l. Tooth l -> Path dir l -> Path dir l
stepPath th (Path ths) = Path (th : ths)

unstepPath :: forall dir l. Path dir l -> Maybe (Tooth l /\ Path dir l)
unstepPath (Path Nil) = Nothing
unstepPath (Path (th : ths)) = Just (th /\ Path ths)

{-
TODO: This function doesn't work. But when I replaced it with a working version,
it caused a bunch of rendering stuff to break. Henry needs to fix all of that.
-}
toUpPath :: forall dir l. ReflectPathDir dir => Path dir l -> Path Up l
toUpPath path@(Path ths) =
    (_ $ reflectPathDir path) $ case_
  # on _up (\_ -> Path ths)
  # on _down (\_ -> reversePath (Path ths :: Path Down l))
--    if inSaneEnglishIsItDown path then Path (List.reverse ths) else Path ths

toDownPath :: forall dir l. ReflectPathDir dir => Path dir l -> Path Down l
toDownPath path@(Path ths) =
    (_ $ reflectPathDir path) $ case_
  # on _up (\_ -> reversePath (Path ths :: Path Up l))
  # on _down (\_ -> Path ths)
--    if inSaneEnglishIsItDown path then Path ths else Path (List.reverse ths)

unPath :: forall l. IsExprLabel l => Path Up l -> Expr l -> Expr l
unPath (Path ths) expr0 = do
--  let Path ths = toUpPath path
--  let res = go ths expr0
--  Debug.trace ("in unPath: input path = " <> show (reflectPathDir path) <> " || " <> pretty path <> "and ths is " <> pretty ths <> " and expr0 = " <> pretty expr0 <> " and res = " <> pretty res) \_ -> res
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

--------------------------------------------------------------------------------
-- Zipper
--------------------------------------------------------------------------------

data Zipper l = Zipper (Path Dir.Up l) (Expr l)

zipperPath (Zipper p _) = p
zipperExpr (Zipper _ e) = e
exprLabel :: forall l. Expr l -> l
exprLabel (Expr l _) = l

zipperParent :: forall l. Zipper l -> Maybe (Tooth l)
zipperParent (Zipper (Path path) _) = List.index path 0

derive instance Generic (Zipper l) _
derive instance Eq l => Eq (Zipper l)
derive instance Ord l => Ord (Zipper l)
instance Show l => Show (Zipper l) where show x = genericShow x
derive instance Functor Zipper

instance IsExprLabel l => Pretty (Zipper l) where
  pretty (Zipper path expr) = 
    prettyPath path $ 
      P.cursor $
        pretty expr

instance Zippable (Zipper l) where
  zipDowns z = snd <$> zipDowns z
  zipUp' z = zipUp z <#> \(Tooth _ kidsZip /\ z') -> (ZipList.leftLength kidsZip /\ z')

zipUp :: forall l. Zipper l -> Maybe (Tooth l /\ Zipper l)
zipUp (Zipper path expr) = case path of
  Path Nil -> Nothing
  Path (th : ths) -> Just $ th /\ Zipper (Path ths) (unTooth th expr)

zipAllTheWayUp :: forall l. Zipper l -> Zipper l
zipAllTheWayUp z = case zipUp z of
    Just (_ /\ z) -> zipAllTheWayUp z
    Nothing -> z

-- | Only zip down the kids in the tooth (not the interior of the tooth).
zipDownsTooth :: forall l. IsExprLabel l => Zipper l -> Tooth l -> ZipList.Path (Zipper l)
zipDownsTooth zipper (_ %< kidsPath) = do
  let ix = ZipList.leftLength kidsPath
  let zs = zipDowns zipper
  case ZipList.zipAt ix (List.fromFoldable zs) of
    Nothing -> bug "[zipDownsTooth] bad index"
    Just (zipsPath /\ _kidZip) -> snd <$> zipsPath

zipDowns :: forall l. Zipper l -> Array (Tooth l /\ Zipper l)
zipDowns (Zipper path expr) = do
  let l % kids0 = expr
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
    Array.reverse $ Array.cons (th /\ Zipper (stepPath th path) kid) zippers
  go zippers th@(l' %< ZipList.Path {left, right: Cons kid' kids'}) kid =
    go
      (Array.cons (th /\ Zipper (stepPath th path) kid) zippers)
      (l' %< ZipList.Path {left: RevList.snoc left kid, right: kids'})
      kid'

zipLeft :: forall l. Zipper l -> Maybe (Zipper l)
zipLeft (Zipper path expr) = case path of
  Path Nil -> Nothing
  Path (l %< kidsPath : ths) -> do
    expr' /\ kidsPath' <- ZipList.zipLeft (expr /\ kidsPath)
    Just $ Zipper (Path (l %< kidsPath' : ths)) expr'

zipRight :: forall l. Zipper l -> Maybe (Zipper l)
zipRight (Zipper path expr) = case path of
  Path Nil -> Nothing
  Path (l %< kidsPath : ths) -> do
    expr' /\ kidsPath' <- ZipList.zipRight (expr /\ kidsPath)
    Just $ Zipper (Path (l %< kidsPath' : ths)) expr'

unzipper :: forall l. IsExprLabel l => Zipper l -> Expr l
unzipper (Zipper path expr) = unPath path expr

zipperpFromTo :: forall l. IsExprLabel l => Zipper l -> Zipper l -> Maybe (Zipperp l)
zipperpFromTo begin end = do
  -- First, determine which direction to go
  let beginPathLength = List.length (unwrap (zipperPath begin))
  let endPathLength = List.length (unwrap (zipperPath end))
  case compare beginPathLength endPathLength of
    -- The path lengths are either the same (empty selection) or incomparable,
    -- so either way there's no zipperp between them
    EQ -> if (zipperPath begin == zipperPath end) then Just (zipperToZipperp begin) else Nothing
    LT -> zipperFromDownTo begin end
    GT -> zipperFromUpTo begin end

zipperFromDownTo :: forall l. IsExprLabel l => Zipper l -> Zipper l -> Maybe (Zipperp l)
zipperFromDownTo begin end = do
  -- Upward oriented
  let beginTooths = unwrap (zipperPath begin)
  let endTooths = unwrap (zipperPath end)
  assertM_ $ strictlyOrdered "zipperFromDownTo" "`begin` should have a shorter path than `end`" 
    (List.length beginTooths) (List.length endTooths)
  -- Upward oriented, since selection is going from a top begin to a bottom end
  selectionTooths <- stripSuffix (Pattern beginTooths) endTooths
  pure $ Zipperp (zipperPath begin) (Right (Path (selectionTooths))) (zipperExpr end)

zipperFromUpTo :: forall l. IsExprLabel l => Zipper l -> Zipper l -> Maybe (Zipperp l)
zipperFromUpTo begin end = do
  -- Downward oriented (so, need to reverse the zipper paths which are upward
  -- oriented)
  let beginTooths = List.reverse $ unwrap (zipperPath begin)
  let endTooths = List.reverse $ unwrap (zipperPath end)
  assertM_ $ strictlyOrdered "zipperFromUpTo" "`end` should have a shorter path than `begin`" 
    (List.length endTooths) (List.length beginTooths)
  -- Downward oriented, since selection is going from a bottom begin to a top
  -- end
  selectionTooths <- List.stripPrefix (Pattern endTooths) beginTooths
  pure $ Zipperp (zipperPath end) (Left (Path (selectionTooths))) (zipperExpr begin)

--------------------------------------------------------------------------------
-- Zipperp
--------------------------------------------------------------------------------

data Zipperp l = Zipperp (Path Dir.Up l) (Path Dir.Down l \/ Path Dir.Up l) (Expr l)

derive instance Generic (Zipperp l) _
derive instance Eq l => Eq (Zipperp l)
derive instance Ord l => Ord (Zipperp l)
instance Show l => Show (Zipperp l) where show x = genericShow x

instance IsExprLabel l => Pretty (Zipperp l) where
  pretty (Zipperp path selection expr) = 
    prettyPath path $
      P.cursor $
        either prettyPath prettyPath selection $
          P.cursor $
            pretty expr

instance Zippable (Zipperp l) where
  -- focus is at the top
  zipDowns (Zipperp path (Left sel) expr) = case unstepPath sel of
    Nothing -> Zippable.zipDowns (Zipperp path (Right mempty) expr)
    Just (th /\ sel') -> [Zipperp (stepPath th path) (Left sel') expr]
  -- focus is at the bottom
  zipDowns (Zipperp path (Right sel) expr) = do
    let zs = Zippable.zipDowns (Zipper sel expr)
    zs <#> \(Zipper sel' expr') -> Zipperp path (Right sel') expr'
  
  -- focus is at the top
  zipUp' (Zipperp path (Left sel) expr) = case unstepPath path of
    Nothing -> Nothing
    Just (th /\ path') -> Just $ 0 /\ Zipperp path' (Left (stepPath th sel)) expr
  -- focus is at the bottom
  zipUp' (Zipperp path (Right sel) expr) = case unstepPath sel of 
    Nothing -> Zippable.zipUp' (Zipperp path (Left mempty) expr)
    Just (th /\ sel') -> Just $ ZipList.leftLength (toothPath th) /\ Zipperp path (Right sel') (unTooth th expr)

zipperpTopPath :: forall l. Zipperp l -> Path Up l
zipperpTopPath (Zipperp path _ _) = path

zipperpBottomPath :: forall l. Zipperp l -> Path Up l
zipperpBottomPath (Zipperp path selection _) = either reversePath identity selection <> path

flipZipperp :: forall l. IsExprLabel l => Zipperp l -> Zipperp l
flipZipperp (Zipperp path selection expr) =
    let selection' = case selection of
            Left upPath -> Right (reversePath upPath)
            Right downPath -> Left (reversePath downPath)
    in
    Zipperp path selection' expr

unzipperp :: forall l. IsExprLabel l => Zipperp l -> Zipper l
unzipperp (Zipperp path selection expr) = case selection of
  Left downPath -> Zipper (reversePath downPath <> path) expr
  Right upPath -> Zipper path (unPath upPath expr)

zipperToZipperp :: forall l. Zipper l -> Zipperp l
zipperToZipperp (Zipper path expr) = Zipperp path (Left (Path Nil)) expr

zipperpToZipper :: forall l. IsExprLabel l => Boolean -> Zipperp l -> Zipper l
zipperpToZipper atTop (Zipperp path selection expr) =
  let upSelection = case selection of
        Left downPath -> reversePath downPath
        Right upPath -> upPath
  in if atTop
    then
        Zipper path (unPath upSelection expr)
    else Zipper (upSelection <> path) expr

--------------------------------------------------------------------------------
-- Change
--------------------------------------------------------------------------------

type Change l = Expr (ChangeLabel l)

data ChangeLabel l
  = Plus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | Minus (Tooth l) {-one kid - whatever fits inside the tooth-}
  | CInj l {-same number of kids that l has-}
  | Replace (Expr l) (Expr l) {-zero kids?-}

derive instance Generic (ChangeLabel l) _
derive instance Eq l => Eq (ChangeLabel l)
derive instance Ord l => Ord (ChangeLabel l)
instance Show l => Show (ChangeLabel l) where show x = genericShow x

instance IsExprLabel l => Pretty (ChangeLabel l) where
  pretty (Plus th) = "(+ " <> prettyTooth th "⌶" <> ")"
  pretty (Minus th) = "(- " <> prettyTooth  th "⌶" <> ")"
  pretty (CInj l) = pretty l
  pretty (Replace e1 e2) = "(" <> pretty e1 <> ") ~~> (" <> pretty e2 <> ")"

derive instance Functor ChangeLabel
derive instance Foldable ChangeLabel
derive instance Traversable ChangeLabel

-- instance Foldable ChangeLabel where
--   foldMap f = case _ of
--     Plus th -> foldMapTooth f th
--     Minus th -> foldMapTooth f th
--     CInj l -> f l
--     Replace e1 e2 -> foldMap f e1 <> foldMap f e2
--   foldl f b = case _ of
--     Plus th -> foldlTooth f b th
--     Minus th -> foldlTooth f b th
--     CInj l -> f b l
--     Replace e1 e2 -> foldl f (foldl f b e1) e2
--   foldr f b = case _ of 
--     Plus th -> foldrTooth f b th
--     Minus th -> foldrTooth f b th
--     CInj l -> f l b
--     Replace e1 e2 -> foldr f (foldr f b e2) e1

-- instance Traversable ChangeLabel where
--   traverse f = case _ of
--     Plus th -> Plus <$> traverseTooth f th
--     Minus th -> Minus <$> traverseTooth f th
--     CInj l -> CInj <$> f l
--     Replace e1 e2 -> Replace <$> traverse f e1 <*> traverse f e2
--   sequence fa = sequenceDefault fa

instance IsExprLabel l => IsExprLabel (ChangeLabel l) where
  prettyExprF'_unsafe (Plus th /\ [kid]) = P.parens $ "+" <> prettyTooth th ("[" <> kid <> "]")
  prettyExprF'_unsafe (Minus th /\ [kid]) = P.parens $ "-" <> prettyTooth th ("[" <> kid <> "]")
  prettyExprF'_unsafe (CInj l /\ kids) = prettyExprF (l /\ kids)
  prettyExprF'_unsafe (Replace e1 e2 /\ []) = P.parens (pretty e1) <> " ~~> " <> P.parens (pretty e2)

  expectedKidsCount (Plus _) = 1
  expectedKidsCount (Minus _) = 1
  expectedKidsCount (CInj l) = expectedKidsCount l
  expectedKidsCount (Replace _ _) = 0

type MetaChange l = Change (Meta l)

plusChange :: forall l. l -> Array (Expr l) -> Change l -> Array (Expr l) -> Change l
plusChange l leftKids inside rightKids =
    Plus (Tooth l (ZipList.Path {left: RevList.reverseArray leftKids, right: List.fromFoldable rightKids}))
        % [inside]

minusChange :: forall l. l -> Array (Expr l) -> Change l -> Array (Expr l) -> Change l
minusChange l leftKids inside rightKids =
    Minus (Tooth l (ZipList.Path {left: RevList.reverseArray leftKids, right: List.fromFoldable rightKids}))
        % [inside]

injectChange :: forall l. l -> Array (Change l) -> Change l
injectChange l chs = CInj l % chs

injectExprChange :: forall l. Expr l -> Change l
injectExprChange (l % kids) = CInj l % (injectExprChange <$> kids)

replaceChange :: forall l. Expr l -> Expr l -> Change l
replaceChange e1 e2 = Replace e1 e2 % []

--------------------------------------------------------------------------------
-- MetaExpr
--------------------------------------------------------------------------------

type MetaExpr l = Expr (Meta l)

fromMetaVar :: forall l. MetaVar -> MetaExpr l
fromMetaVar mx = MV mx % []

pureMetaExpr :: forall l. l -> Array (MetaExpr l) -> MetaExpr l
pureMetaExpr l = (MInj l % _)

infixl 7 pureMetaExpr as %*

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

type MetaVarSub a = Map.Map MetaVar a

subMetaExpr :: forall l. IsExprLabel l => MetaVarSub (Expr l) -> MetaExpr l -> Expr l
subMetaExpr sigma = assertInput_ (wellformedExpr "subMetaExpr") go
  where
  go :: Partial => _
  go = case _ of
    MV mx % [] -> assert (hasKey "subMetaExpr" mx sigma) identity
    MInj l % kids -> l % (go <$> kids)

subMetaExprPartially :: forall l. IsExprLabel l => MetaVarSub (MetaExpr l) -> MetaExpr l -> MetaExpr l
subMetaExprPartially sigma = assertInput_ (wellformedExpr "subMetaExprPartially") go
  where
  go :: Partial => _
  go = case _ of
    MV mx % [] -> case Map.lookup mx sigma of
      Nothing -> (MV mx) % []
      Just mexpr -> mexpr
    MInj l % kids -> (MInj l) % (go <$> kids)

--    Minus [kid] -> ?h
--    CInj kids -> ?h
--    Replace t1 t2 -> ?h
--subMetaChangePartially sigma = assertInput_ (wellformedExpr "subMetaExprPartially") go
--  where
--  go :: Partial => _
--  go = case _ of
--    Meta (Left mx) % [] -> case Map.lookup mx sigma of
--      Nothing -> Meta (Left mx) % []
--      Just mexpr -> mexpr
--    Meta (Right l) % kids -> Meta (Right l) % (go <$> kids)

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- Custom defined pattern matching for Exprs, since purescript pattern matching
-- can be very verbose.

data MatchLabel l = InjectMatchLabel l | Match

derive instance Generic (MatchLabel l) _
derive instance Eq l => Eq (MatchLabel l)
derive instance Ord l => Ord (MatchLabel l)
instance Show l => Show (MatchLabel l) where show x = genericShow x

-- Henry how am I supposed to organize these? Why are there two pretty functions?
instance IsExprLabel l => Pretty (MatchLabel l) where
  pretty Match = "[*]"
  pretty (InjectMatchLabel l) = pretty l

derive instance Functor MatchLabel
derive instance Foldable MatchLabel
derive instance Traversable MatchLabel

instance IsExprLabel l => IsExprLabel (MatchLabel l) where
  prettyExprF'_unsafe (Match /\ []) = "[*]"
  prettyExprF'_unsafe (InjectMatchLabel l /\ kids) = prettyExprF (l /\ kids)

  expectedKidsCount Match = 0
  expectedKidsCount (InjectMatchLabel l) = expectedKidsCount l

slot :: forall l. Expr (MatchLabel l)
slot = Match % []

injectMatchExpr :: forall l. l -> Array (Expr (MatchLabel l)) -> Expr (MatchLabel l)
injectMatchExpr l kids = (InjectMatchLabel l) % kids

infixl 7 injectMatchExpr as %$

matchDiffExprs :: forall l1 l2. (l1 -> l2 -> Boolean) -> Expr l1 -> Expr (MatchLabel l2) -> Maybe (Array (Expr l1))
matchDiffExprs compare (l1 % kids1) (InjectMatchLabel l2 % kids2) | compare l1 l2 =
    Array.concat <$> sequence (Array.zipWith (matchDiffExprs compare) kids1 kids2)
matchDiffExprs _ e2 (Match % []) = Just [e2]
matchDiffExprs _ _ _ = Nothing

matchExprImpl :: forall l. IsExprLabel l => Expr l -> Expr (MatchLabel l) -> Maybe (Array (Expr l))
matchExprImpl = matchDiffExprs (==)

-- helper function for matchChange
matchTeeth :: forall l. IsExprLabel l =>
    Tooth l -> Tooth (MatchLabel l)
    -> Maybe (Array (Expr l))
matchTeeth (Tooth l1 (ZipList.Path {left: left1, right: right1}))
           (Tooth (InjectMatchLabel l2) (ZipList.Path {left: left2, right: right2})) =
    if not (l1 == l2 && List.length right1 == List.length right2) then Nothing else do
    leftMatches <- sequence $ List.zipWith matchExprImpl (RevList.unreverse left1) (RevList.unreverse left2)
    rightMatches <- sequence $ List.zipWith matchExprImpl right1 right2
    let concatMatches = foldl (<>) []
    pure $ concatMatches leftMatches <> concatMatches rightMatches
matchTeeth (Tooth l1 a) (Tooth Match y) = bug "what even is this case? I guess it shouldn't happen since Match has zero children, and therefore can't be a tooth?"

matchChange :: forall l. IsExprLabel l =>
    -- Two kinds of slots: those in change positions, and those in expression postions
    Change l -> Expr (ChangeLabel (MatchLabel l))
    -- Two kinds out outputs: expressions and changes
    -> Maybe (Array (Expr l) /\ Array (Change l))
matchChange c (CInj Match % []) = Just ([] /\ [c])
matchChange (CInj l1 % kids1) (CInj (InjectMatchLabel l2) % kids2) | l1 == l2 =
    foldl (\(a/\b) (c/\d) -> (a <> c) /\ (b <> d)) ([] /\ []) <$> sequence (Array.zipWith matchChange kids1 kids2)
matchChange (Plus th1 % [kid1]) ((Plus th2) % [kid2]) = do
    toothMatches <- matchTeeth th1 th2
    es /\ cs <- matchChange kid1 kid2
    pure $ (toothMatches <> es) /\ cs
matchChange (Minus th1 % [kid1]) ((Minus th2) % [kid2]) = do
    toothMatches <- matchTeeth th1 th2
    es /\ cs <- matchChange kid1 kid2
    pure $ (toothMatches <> es) /\ cs
matchChange (Replace a1 b1 % []) (Replace a2 b2 % []) = do
    matches1 <- (matchExprImpl a1 a2)
    matches2 <- (matchExprImpl b1 b2)
    pure $ (matches1 <> matches2) /\ []
matchChange _ _ = Nothing

---- helper function for matchChange
--matchTeeth2Impl :: forall l. IsExprLabel l =>
--    Tooth l -> Tooth (MatchLabel l)
--    -> Maybe (Array (MetaVar /\ Expr l))
--matchTeeth2Impl (Tooth l1 (ZipList.Path {left: left1, right: right1}))
--           (Tooth (InjectMatchLabel l2) (ZipList.Path {left: left2, right: right2})) =
--    if not (l1 == l2 && List.length right1 == List.length right2) then Nothing else do
--    leftMatches <- sequence $ List.zipWith matchExprImpl (RevList.unreverse left1) (RevList.unreverse left2)
--    rightMatches <- sequence $ List.zipWith matchExprImpl right1 right2
--    let concatMatches = foldl (<>) []
--    pure $ concatMatches leftMatches <> concatMatches rightMatches
--matchTeeth2Impl (Tooth l1 a) (Tooth Match y) = bug "what even is this case? I guess it shouldn't happen since Match has zero children, and therefore can't be a tooth?"
--
--matchChange2Impl :: forall l. IsExprLabel l =>
--    -- Two kinds of slots: those in change positions, and those in expression postions
--    Change l -> Expr (ChangeLabel (Meta l))
--    -- Two kinds out outputs: expressions and changes
--    -> Maybe (Array (MetaVar /\ Expr l) /\ Array (MetaVar /\ Change l))
--matchChange2Impl c (CInj (Meta (Left mv)) % []) = Just ([] /\ [mv /\ c])
--matchChange2Impl (CInj l1 % kids1) (CInj (Meta (Right l2)) % kids2) | l1 == l2 =
--    foldl (\(a/\b) (c/\d) -> (a <> c) /\ (b <> d)) ([] /\ []) <$> sequence (Array.zipWith matchChange2Impl kids1 kids2)
--matchChange2Impl (Plus th1 % [kid1]) ((Plus th2) % [kid2]) = do
--    toothMatches <- matchTeeth th1 th2
--    es /\ cs <- matchChange2Impl kid1 kid2
--    pure $ (toothMatches <> es) /\ cs
--matchChange2Impl (Minus th1 % [kid1]) ((Minus th2) % [kid2]) = do
--    toothMatches <- matchTeeth th1 th2
--    es /\ cs <- matchChange2Impl kid1 kid2
--    pure $ (toothMatches <> es) /\ cs
--matchChange2Impl (Replace a1 b1 % []) (Replace a2 b2 % []) = do
--    matches1 <- (matchExprImpl a1 a2)
--    matches2 <- (matchExprImpl b1 b2)
--    pure $ (matches1 <> matches2) /\ []
--matchChange2Impl _ _ = Nothing

matchExpr :: forall l out. IsExprLabel l => Expr l -> Expr (MatchLabel l) -> (Partial => Array (Expr l) -> out) -> out
matchExpr e eMatch f = unsafePartial f (fromJust' "in matchExpr, expressions didn't match" $ matchExprImpl e eMatch)

matchExprMaybe :: forall l out. IsExprLabel l => Expr l -> Expr (MatchLabel l) -> (Partial => Array (Expr l) -> out) -> Maybe out
matchExprMaybe e eMatch f = unsafePartial f <$> matchExprImpl e eMatch

matchExprs :: forall l out. IsExprLabel l => Expr l -> Array (Expr (MatchLabel l) /\ (Partial => Array (Expr l) -> out)) -> out
matchExprs e cases = fst <<< fromJust' "matchExprs - didn't match any cases" $ Util.findWithIndex
  (\(eMatch /\ f) -> f <$> matchExprImpl e eMatch)
  (rmap unsafePartial <$> cases)

-- I couldn't get matchExprs to work, so I have this for now:
matchExpr2 :: forall l out. IsExprLabel l => Expr l
    -> Expr (MatchLabel l) -> (Partial => Array (Expr l) -> out)
    -> Expr (MatchLabel l) -> (Partial => Array (Expr l) -> out)
    -> out
matchExpr2 e eMatch1 f1 eMatch2 f2
    = case matchExprImpl e eMatch1 of
        Just args -> unsafePartial f1 args
        Nothing -> matchExpr e eMatch2 f2