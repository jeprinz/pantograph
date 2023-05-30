module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug.Assertion (Assertion, assert, assertInput_, assertInterface_, just, makeAssertionBoolean)
import Control.Plus (empty)
import Data.Array as Array
import Data.Enum (class Enum)
import Data.Expr (class IsExprLabel, class ReflectPathDir, expectedKidsCount, prettyExprF'_unsafe, reflectPathDir, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (case_, on)
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Language.Pantograph.Generic.Unification (class Freshenable, freshen')
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, pretty)
import Type.Direction (_down, _up)

--------------------------------------------------------------------------------
-- IsRuleLabel
--------------------------------------------------------------------------------

class (Expr.IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r
  isHoleRuleTotalMap :: TotalMap r Boolean
  defaultDerivTerm' :: Partial => Sort l -> Maybe (DerivTerm l r)

defaultDerivTerm :: forall l r. IsRuleLabel l r => Sort l -> Maybe (DerivTerm l r)
defaultDerivTerm sort = assert (Expr.wellformedExpr "defaultDerivTerm" sort) \_ -> 
  defaultDerivTerm' sort

isHoleRule :: forall l r. IsRuleLabel l r => r -> Boolean
isHoleRule r = TotalMap.lookup r isHoleRuleTotalMap

isHoleDerivLabel :: forall l r. IsRuleLabel l r => DerivLabel l r -> Maybe (Sort l)
isHoleDerivLabel (DerivLabel r sort) | isHoleRule r = pure sort
isHoleDerivLabel _ = empty

isHoleDerivTerm :: forall l r. IsRuleLabel l r => DerivTerm l r -> Maybe (Sort l)
isHoleDerivTerm (dl % _) = isHoleDerivLabel dl

expectedHypsCount :: forall l r. IsRuleLabel l r => r -> Int
expectedHypsCount r = do
  let Rule _ hyps _ = TotalMap.lookup r language
  Array.length hyps

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

-- !TODO Removed: DerivHole (Sort l) -- TODO: get rid of this, instead have "isHole : r -> Boolean"

data DerivLabel l r 
  = DerivLabel r (Sort l)
  | DerivString String
  
  -- | TextBox String -- this String s stands for the sort (Name (Str s))
  -- | DerivIndent??? Jacob note: I think its better to have the renderer give information about newlines, put it in """prerenderDerivTermKids"""
  -- alternate idea: any hole of a (Name s) sort is a textbox

derivLabelRule :: forall l r. DerivLabel l r -> Maybe r
derivLabelRule (DerivLabel r _) = Just r
derivLabelRule (DerivString _) = Nothing

derivLabelSort :: forall l r. DerivLabel l r -> Sort l
derivLabelSort (DerivLabel _ s) = s
derivLabelSort (DerivString str) = NameSortLabel %* [StringSortLabel str %* []]

mapDerivLabelSort :: forall l r. (Sort l -> Sort l) -> DerivLabel l r -> DerivLabel l r
mapDerivLabelSort f (DerivLabel r sort) = DerivLabel r (f sort)
mapDerivLabelSort _ (DerivString str) = DerivString str

infix 8 DerivLabel as %|-

injectSortLabelDerivLabel sortLabel kids = InjectSortLabel sortLabel %* kids

infix 8 injectSortLabelDerivLabel as %|-*

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance IsRuleLabel l r => Pretty (DerivLabel l r) where
  pretty (DerivLabel r ix) = pretty r <> "(" <> pretty ix <> ")"
  pretty (DerivString str) = "String(" <> str <> ")"

instance Freshenable (DerivLabel l r) where
  freshen rho (DerivLabel hr me) = DerivLabel hr (freshen' rho me)
  freshen rho (DerivString str) = DerivString str

--------------------------------------------------------------------------------
-- AsExprLabel
--------------------------------------------------------------------------------

newtype AsExprLabel a = AsExprLabel a
derive newtype instance Eq a => Eq (AsExprLabel a)
derive newtype instance Ord a => Ord (AsExprLabel a)
derive newtype instance Show a => Show (AsExprLabel a)
derive newtype instance Pretty a => Pretty (AsExprLabel a)

-- | Can pretend that a rule label is the expr label of derivations.
instance IsRuleLabel l r => Expr.IsExprLabel (AsExprLabel r) where
  prettyExprF'_unsafe (AsExprLabel r /\ kids) = prettyExprF'_unsafe_RuleLabel (r /\ kids)
  expectedKidsCount (AsExprLabel r) = expectedHypsCount r

instance IsRuleLabel l r => Expr.IsExprLabel (DerivLabel l r) where
  -- NOTE: This implementation ignores the expression label and metaexpression,
  -- but maybe we want to print those at some point for debugging?
  prettyExprF'_unsafe (DerivLabel r (Expr.Expr _l _metaExpr) /\ kids) = Expr.prettyExprF (AsExprLabel r /\ kids)
  prettyExprF'_unsafe (DerivString str /\ []) = "Text(" <> str <> ")"

  expectedKidsCount (DerivLabel r _) = Expr.expectedKidsCount (AsExprLabel r)
  expectedKidsCount (DerivString str) = 0

--------------------------------------------------------------------------------
-- Sorts
--------------------------------------------------------------------------------

{-
For example, in SULC, where sorts have scopes:

 (Name ?a)       Term (cons gamma ?a)
----------------------------------------------- lam
   Term gamma

-}

type Sort l = Expr.MetaExpr (SortLabel l)
type SortChange l = Expr.MetaChange (SortLabel l)

data SortLabel l 
  = InjectSortLabel l
  | NameSortLabel
  | StringSortLabel String

derive instance Generic (SortLabel l) _
instance Show l => Show (SortLabel l) where show x = genericShow x
derive instance Eq l => Eq (SortLabel l)
derive instance Ord l => Ord (SortLabel l)
derive instance Functor SortLabel
derive instance Foldable SortLabel
derive instance Traversable SortLabel

instance Pretty l => Pretty (SortLabel l) where
  pretty (InjectSortLabel l) = pretty l
  pretty NameSortLabel = "NameSort"
  pretty (StringSortLabel str) = "\"" <> str <> "\"" 

instance IsExprLabel l => IsExprLabel (SortLabel l) where
  prettyExprF'_unsafe = case _ of
    InjectSortLabel l /\ kids -> prettyExprF'_unsafe (l /\ kids)
    NameSortLabel /\ [string] -> "Name(" <> string <> ")"
    StringSortLabel str /\ [] -> "\"" <> str <> "\"" 

  expectedKidsCount = case _ of
    InjectSortLabel l -> expectedKidsCount l
    NameSortLabel -> 1
    StringSortLabel _ -> 0

--------------------------------------------------------------------------------
-- DerivTerm
--------------------------------------------------------------------------------
type DerivTerm l r = Expr.Expr (DerivLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivLabel l r)
type DerivTooth l r = Expr.Tooth (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipperp l r = Expr.Zipperp (DerivLabel l r)

derivTermSort :: forall l r. DerivTerm l r -> Sort l
derivTermSort (dl % _) = derivLabelSort dl

derivTermRuleLabel :: forall l r. DerivTerm l r -> Maybe r
derivTermRuleLabel (dl % _) = derivLabelRule dl

derivToothSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothSort = assertInterface_ (Expr.wellformedTooth "derivToothSort") (Expr.wellformedExpr "derivToothSort") case _ of
  Expr.Tooth (DerivLabel _r sort) _ -> sort

derivToothInteriorSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothInteriorSort = assertInterface_ (Expr.wellformedTooth "derivToothSort") (Expr.wellformedExpr "derivToothSort") case _ of
  Expr.Tooth (DerivLabel r _) kidsPath -> do
    let Rule _mvars hyps _con = TotalMap.lookup r language
    assert (just "derivToothInteriorSort" $ hyps Array.!! ZipList.leftLength kidsPath) identity

derivPathSort :: forall dir l r. IsRuleLabel l r => ReflectPathDir dir => Sort l -> DerivPath dir l r -> Sort l
derivPathSort topSort (Expr.Path Nil) = topSort
derivPathSort _ path@(Expr.Path (th : _)) = reflectPathDir path # 
  (case_
    # on _up (\_ -> derivToothInteriorSort th)
    # on _down (\_ -> derivToothSort th)
  )

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | A `Rule` has the form 
-- | ```
-- |   [∀ «MetaVar»]*
-- |   [«MetaExpr»] -- kids
-- |   --------------------
-- |   «MetaExpr» -- parent
-- | ```
data Rule l = 
  Rule
    (Set Expr.MetaVar)
    (Array (Sort l))
    (Sort l)

derive instance Functor Rule
derive instance Foldable Rule
derive instance Traversable Rule

nonDuplicateArray :: forall a. Eq a => Ord a => String -> String -> Array a -> Assertion Unit
nonDuplicateArray source message arr = makeAssertionBoolean
  { name: "nonDuplicateArray"
  , source
  , condition: Array.nub arr == arr
  , message
  }

makeRule' :: forall l. 
  Array String ->
  (Array (Sort l) -> Array (Sort l) /\ Sort l) -> 
  Rule l
makeRule' = assertInput_ (\strs -> nonDuplicateArray "makeRule" ("All metavar strings must be different among: " <> show strs) strs) \strs f -> do
  let mxs = Expr.freshMetaVar <$> strs
  let es = Expr.fromMetaVar <$> mxs
  let hyps /\ con = f es
  Rule (Set.fromFoldable mxs) hyps con

makeRule :: forall l. 
  Array String ->
  (Partial => Array (Sort l) -> Array (Sort l) /\ Sort l) -> 
  Rule l
makeRule = \strs f -> makeRule' strs (unsafePartial f)

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

-- | A `Language` associates each `RuleLabel` to a `Rule`
type Language l r = TotalMap r (Rule l)

--------------------------------------------------------------------------------
-- LanguageChanges, ChangeRule
--------------------------------------------------------------------------------

type LanguageChanges l r = TotalMap r (ChangeRule l) -- changes go from child to parent

defaultLanguageChanges :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r => Language l r -> LanguageChanges l r
defaultLanguageChanges = map \(Rule mvars kids parent) ->
  ChangeRule mvars (kids <#> \kid -> diff kid parent) 

-- | A `ChangeRule` is oriented from parent to kid i.e. it describes the changes
-- to apply to the parent's kids.
data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (Expr.MetaChange (SortLabel l)))

derive instance Functor ChangeRule
derive instance Foldable ChangeRule
derive instance Traversable ChangeRule

