module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug (bug)
import Bug.Assertion (Assertion, assert, assertInput_, assertInterface_, equal, just, makeAssertionBoolean)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.Enum (class Enum)
import Data.Expr (class IsExprLabel, class ReflectPathDir, MetaVar(..), expectedKidsCount, freshMetaVar, prettyExprF'_unsafe, reflectPathDir, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.Traversable (class Foldable, class Traversable)
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (case_, on)
import Debug (trace)
import Hole (hole)
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Language.Pantograph.Generic.Unification (class Freshenable, Sub, freshen', unifyLists, composeSub, composeSubs)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, bullets, pretty)
import Type.Direction (_down, _up)
import Type.Direction as Dir
import Util as Util

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

class (Expr.IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r
  isHoleRuleTotalMap :: TotalMap r Boolean
  defaultDerivTerm' :: Partial => Sort l -> Maybe (DerivTerm l r)

-- NOTE: if the given sort is (Name ?x), then it will return a term of sort (Name ""). So it doesn't always return
-- a term with the sort you gave it. On the other hand, if you give it a sort that is allowed to appear in the
-- program, then it will always return a term with the given sort. So we should think of what the exact criteria is here.
defaultDerivTerm :: forall l r. IsRuleLabel l r => Sort l -> Maybe (DerivTerm l r)
defaultDerivTerm sort = assert (Expr.wellformedExpr "defaultDerivTerm" sort) \_ -> 
  defaultDerivTerm' sort

---- replaces (Name ?x) with (Name (Str ""))
--concretizeSort :: forall l. Sort l -> Sort l
--concretizeSort (Expr.Expr (Expr.Meta (Either.Left (Expr.RuleMetaVar true _))) [])
--    = Expr.Expr (Expr.Meta (Either.Right (StringSortLabel ""))) []
--concretizeSort (Expr.Expr l kids) = Expr.Expr l (map concretizeSort kids)

isHoleRule :: forall l r. IsRuleLabel l r => r -> Boolean
isHoleRule r = TotalMap.lookup r isHoleRuleTotalMap

-- freshDerivLabelSub :: forall l r. IsRuleLabel r -> Expr.MetaVarSub (Sort l) 

-- Here are some functions that will help with dealing with the sub in DerivLabel
getSortFromSub :: forall l r. IsRuleLabel l r => r -> Expr.MetaVarSub (Sort l) -> Sort l
getSortFromSub r sub =
    let (Rule _vars _kidSorts parentSort) = TotalMap.lookup r language in
    Expr.subMetaExprPartially sub parentSort

makeLabel :: forall l r. IsRuleLabel l r => r -> Array (String /\ Sort l) -> DerivLabel l r
makeLabel ruleLabel values =
    let Rule vars _ _ = TotalMap.lookup ruleLabel language in
    let sigma = Map.fromFoldable (lmap RuleMetaVar <$> values) in
    assert (equal "makeLabel" 
      ( "Given substitution must have same vars as quantified in rule:" <> 
        bullets ["ruleLabel = " <> pretty ruleLabel, "value keys = " <> pretty values] )
      vars (Map.keys sigma)) \_ ->
    DerivLabel ruleLabel sigma

-- Makes a DerivLabel from a rule r with all of the parameters set to fresh metavars
makeFreshLabel :: forall l r. IsRuleLabel l r => r -> DerivLabel l r
makeFreshLabel ruleLabel =
    let Rule vars _ _ = TotalMap.lookup ruleLabel language in
    let sigma = Map.fromFoldable (Set.map (\mv -> mv /\ Expr.fromMetaVar (Expr.freshMetaVar (Expr.metaVarName mv))) vars) in
    DerivLabel ruleLabel sigma

isHoleDerivLabel :: forall l r. IsRuleLabel l r => DerivLabel l r -> Maybe (Sort l)
isHoleDerivLabel (DerivLabel r sub) | isHoleRule r = pure (getSortFromSub r sub)
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

data DerivLabel l r
  = DerivLabel r (Expr.MetaVarSub (Sort l)) -- NOTE: the domain of this substitution is the set of MetaVars in the Rule for r
  | DerivString String -- NOTE: When we generalize NameSortLabel to be a TypeOf label, this will also be parametrized by the same set of type labels.
    -- When we make that generalization, we could rename this to "DerivLiteral"
    -- IDEA: we could have UUID symbol literals, which are displayed nicely but have a UUID underlying them. These could be used in a language for naming libraries.
        -- You can copy/paste the symbol, which remembers the UUID, but when you generate a new one it gets a new UUID.

derivLabelRule :: forall l r. DerivLabel l r -> Maybe r
derivLabelRule (DerivLabel r _) = Just r
derivLabelRule (DerivString _) = Nothing

derivLabelSort :: forall l r. IsRuleLabel l r => DerivLabel l r -> Sort l
derivLabelSort (DerivLabel r sub) = getSortFromSub r sub
derivLabelSort (DerivString str) = NameSortLabel %* [StringSortLabel str %* []]

kidSorts :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    DerivLabel l r -> Array (Sort l)
kidSorts (DerivLabel ruleLabel sigma) =
    let (Rule _vars kidSorts _parentSort) = TotalMap.lookup ruleLabel language in
    map (Expr.subMetaExprPartially sigma) kidSorts
kidSorts (DerivString _) = []

mapDerivLabelSort :: forall l r. (Sort l -> Sort l) -> DerivLabel l r -> DerivLabel l r
mapDerivLabelSort f (DerivLabel r sub) = DerivLabel r (map f sub)
mapDerivLabelSort _ (DerivString str) = DerivString str

infix 8 DerivLabel as %|-

injectSortLabelDerivLabel sortLabel kids = InjectSortLabel sortLabel %* kids

infix 8 injectSortLabelDerivLabel as %|-*

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance (IsExprLabel l, Pretty r) => Pretty (DerivLabel l r) where
  -- pretty (DerivLabel r ix) = pretty r <> "(" <> pretty ix <> ")"
  pretty (DerivLabel r ix) = pretty r <> "(" <> List.intercalate ", " (map (\(x /\ sort) -> pretty x <> " : " <> pretty sort) (Map.toUnfoldable ix) :: List _) <> ")"
  pretty (DerivString str) = "String(" <> str <> ")"

instance Freshenable (DerivLabel l r) where
  freshen rho (DerivLabel hr me) = DerivLabel hr (freshen' rho me)
  freshen _rho (DerivString str) = DerivString str

subDerivLabel :: forall l r. IsRuleLabel l r => SortSub l -> DerivLabel l r -> DerivLabel l r
subDerivLabel sub (DerivLabel r s) = DerivLabel r (map (Expr.subMetaExprPartially sub) s)
subDerivLabel _ other = other

fillDefaultsDerivTerm :: forall l r. IsRuleLabel l r => DerivTerm l r -> DerivTerm l r
fillDefaultsDerivTerm inp@(Expr.Expr label kids) =
    case isHoleDerivLabel label of
        Just sort -> Util.fromJust' "why does defaultDerivTerm return a Maybe anyway?" $ defaultDerivTerm sort
        Nothing -> Expr.Expr label (map fillDefaultsDerivTerm kids)

fillDefaultsDerivPath :: forall l r dir. IsRuleLabel l r => DerivPath dir l r -> DerivPath dir l r
fillDefaultsDerivPath (Expr.Path teeth) = Expr.Path $
    map ( \ (Expr.Tooth label (ZipList.Path {left, right})) ->
        Expr.Tooth label (ZipList.Path {left: map fillDefaultsDerivTerm left, right: map fillDefaultsDerivTerm right})
    ) teeth

fillDefaultsDerivZipper :: forall l r. IsRuleLabel l r => DerivZipper l r -> DerivZipper l r
fillDefaultsDerivZipper (Expr.Zipper path term) = Expr.Zipper (fillDefaultsDerivPath path) (fillDefaultsDerivTerm term)

subDerivTerm :: forall l r. IsRuleLabel l r => SortSub l -> DerivTerm l r -> DerivTerm l r
subDerivTerm sub =  map (subDerivLabel sub) >>> fillDefaultsDerivTerm

subDerivPath :: forall d l r. IsRuleLabel l r => SortSub l -> DerivPath d l r -> DerivPath d l r
subDerivPath sub = map (subDerivLabel sub) >>> fillDefaultsDerivPath

subDerivZipper :: forall l r. IsRuleLabel l r => SortSub l -> DerivZipper l r -> DerivZipper l r
subDerivZipper sub = map (subDerivLabel sub) >>> fillDefaultsDerivZipper


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
  prettyExprF'_unsafe (DerivLabel r _sub /\ kids) = Expr.prettyExprF (AsExprLabel r /\ kids)
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

data SortLabel l  -- l is language specific sort labels, and SortLabel adds on generic ones that exist for every language
  = InjectSortLabel l
  | NameSortLabel -- NOTE: can generalize to "TypeOf sort label" -- Jacob note: I still think that we should call this TextboxSortLabel, since it really is just used to indicate a literal in the derivations
  | StringSortLabel String -- NOTE: can generalize to DataSortLabel

derive instance Generic (SortLabel l) _
instance Show l => Show (SortLabel l) where show x = genericShow x
derive instance Eq l => Eq (SortLabel l)
derive instance Ord l => Ord (SortLabel l)
derive instance Functor SortLabel
derive instance Foldable SortLabel
derive instance Traversable SortLabel

freshMetaVarSort :: forall l. String -> Sort l
freshMetaVarSort name = Expr.Meta (Either.Left (freshMetaVar name)) % []

-- This is used as part of a DSL for building up sorts.
sor :: forall l. l -> Expr.Meta (SortLabel l)
sor l = pure (InjectSortLabel l)

nameSort :: forall l. String -> Expr.MetaExpr (SortLabel l)
nameSort name = pure (StringSortLabel name) % []

-- This is used as part of a DSL for building up sort changes
csor :: forall l. l -> Expr.ChangeLabel (Expr.Meta (SortLabel l))
csor l = Expr.Inject (pure (InjectSortLabel l))

-- assert that a label is a name and return the string
matchNameLabel :: forall l. Sort l -> String
matchNameLabel (Expr.Expr (Expr.Meta (Either.Right NameSortLabel)) [Expr.Expr (Expr.Meta (Either.Right (StringSortLabel s))) []]) = s
matchNameLabel _ = bug "wasn't name"

-- assert that a label is a string and return the string
matchStringLabel :: forall l. Sort l -> String
matchStringLabel (Expr.Expr (Expr.Meta (Either.Right (StringSortLabel s))) []) = s
matchStringLabel _ = bug "wasn't stringlabel"

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

type SortSub l = Sub (SortLabel l)

freshenRuleMetaVars :: forall l. Set.Set MetaVar -> SortSub l
freshenRuleMetaVars mvars = 
  Map.fromFoldable $
  flip map (Set.toUnfoldable mvars :: Array _) $
  case _ of
    RuleMetaVar str -> RuleMetaVar str /\ Expr.fromMetaVar (freshMetaVar str)
    _ -> bug "[freshenRuleMetaVars] Shouldn't use this on non-RuleMetaVars"

--------------------------------------------------------------------------------
-- DerivTerm
--------------------------------------------------------------------------------
type DerivTerm l r = Expr.Expr (DerivLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivLabel l r) -- (Sort l) -- interior sort, regardless of dir
type DerivTooth l r = Expr.Tooth (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipperp l r = Expr.Zipperp (DerivLabel l r)

derivTermSort :: forall l r. IsRuleLabel l r => DerivTerm l r -> Sort l
derivTermSort (dl % _) = derivLabelSort dl

derivTermRuleLabel :: forall l r. DerivTerm l r -> Maybe r
derivTermRuleLabel (dl % _) = derivLabelRule dl

derivToothSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothSort = assertInterface_ (Expr.wellformedTooth "derivToothSort") (Expr.wellformedExpr "derivToothSort") case _ of
  Expr.Tooth (DerivLabel r sub) _ -> getSortFromSub r sub

-- NOTE: This function can't be written how we currently have teeth defined. There simply isn't the information available.
derivToothInteriorSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothInteriorSort = assertInterface_ (Expr.wellformedTooth "derivToothSort") (Expr.wellformedExpr "derivToothSort") case _ of
  Expr.Tooth (DerivLabel r sort) kidsPath -> do
    let Rule _mvars hyps _con = TotalMap.lookup r language
    assert (just "derivToothInteriorSort" $ hyps Array.!! ZipList.leftLength kidsPath) identity

-- | Get the top sort of a path (given its bottom sort)
derivPathSort :: forall dir l r. IsRuleLabel l r => ReflectPathDir dir => DerivPath dir l r -> Sort l -> Sort l
derivPathSort dpath botSort = do
  let dpath' = Expr.toDownPath dpath
  case dpath' of
    Expr.Path Nil -> botSort
    Expr.Path (th : _) -> derivToothSort th

-- NOTE: once toDownPath is fixed, this can be deleted and you can use derivPathSort combined with nonemptyPathSort
nonemptyUpPathTopSort :: forall l r. IsRuleLabel l r => DerivPath Dir.Up l r -> Sort l
nonemptyUpPathTopSort dpath = do
  let dpath' = Expr.reversePath dpath
  case dpath' of
    Expr.Path Nil -> bug "path was empty"
    Expr.Path (th : _) -> derivToothSort th

nonemptyPathInnerSort :: forall l r. IsRuleLabel l r => DerivPath Dir.Up l r -> Sort l
nonemptyPathInnerSort (Expr.Path teeth) = case teeth of
    (Expr.Tooth (DerivLabel r sigma) p) : _ ->
        let Rule _mvars hyps _con = TotalMap.lookup r language in
        Expr.subMetaExprPartially sigma (Util.fromJust' "nepis" $ Array.index hyps (ZipList.leftLength p))
    _ -> bug "path waas empty oor otherwise"

derivZipperSort :: forall l r. IsRuleLabel l r => DerivZipper l r -> Sort l
derivZipperSort (Expr.Zipper _ dterm) = derivTermSort dterm

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | A `Rule` has the form 
-- | ```
-- |   [∀ «MetaVar»]*
-- |   [«Sort»] -- kids
-- |   --------------------
-- |   «Sort» -- parent
-- | ```
data Rule l = 
  Rule
    (Set.Set Expr.MetaVar)
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
  Array String -> -- metavariables
  (Array (Sort l) -> Array (Sort l) /\ Sort l) ->
  Rule l
makeRule' = assertInput_ (\strs -> nonDuplicateArray "makeRule" ("All metavar strings must be different among: " <> show strs) strs) \strs f -> do
  let mxs = Expr.RuleMetaVar <$> strs
  let es = Expr.fromMetaVar <$> mxs
  let hyps /\ con = f es
  Rule (Set.fromFoldable mxs) hyps con

makeRule :: forall l. 
  Array String -> -- Regular metavariables
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
data ChangeRule l = ChangeRule (Set.Set Expr.MetaVar) (Array (Expr.MetaChange (SortLabel l)))

derive instance Functor ChangeRule
derive instance Foldable ChangeRule
derive instance Traversable ChangeRule


--------------------------------------------------------------------------------
----------- Typechecking (and inference) for any grammar -----------------------
--------------------------------------------------------------------------------

{-
Inputs a term, and checks if it typechecks up to unification of metavariables. If it does, returns the sub.
To do regular typechecking, then check if the sub is empty.
-}

-- TODO: I guess this has a bug that it doesn't check the parent of the top node?
infer :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    DerivTerm l r -> Maybe (SortSub l)
infer (l % kids) = do
    subs <- sequence $ map infer kids
    let sub1 = composeSubs subs
    let inferredKidSorts = map (Expr.subMetaExprPartially sub1 <<< derivTermSort) kids
    let expectedKidSorts = kidSorts l
    _ /\ sub2 <- unifyLists (List.fromFoldable inferredKidSorts) (List.fromFoldable expectedKidSorts)
    let allSubs = composeSub sub1 sub2
    pure $ allSubs

inferPath :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    Sort l -> DerivPath Dir.Up l r -> Maybe (SortSub l)
inferPath _ (Expr.Path Nil) = Just Map.empty
inferPath innerSort (Expr.Path ((Expr.Tooth l (ZipList.Path {left, right})) : ths)) = do
    subs1 <- sequence (map infer left)
    subs2 <- sequence (map infer right)
    let allSubs1 = (composeSubs subs1) `composeSub` (composeSubs subs2) -- `composeSub` sub3
    let inferredKidSorts = map (Expr.subMetaExprPartially allSubs1)
            ((derivTermSort <$> RevList.unreverse left) <> innerSort : (derivTermSort <$> right))
    let expectedKidSorts = map (Expr.subMetaExprPartially allSubs1) (kidSorts l)
    _ /\ sub2 <- unifyLists (List.fromFoldable inferredKidSorts) (List.fromFoldable expectedKidSorts)
    let sub12 = composeSub allSubs1 sub2
    sub3 <- inferPath (Expr.subMetaExprPartially sub12 (derivLabelSort l)) (Expr.Path ths)
    pure $ composeSub sub12 sub3

forgetDerivLabelSorts :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivLabel l r -> DerivLabel l r
forgetDerivLabelSorts f label =
        case f label of
            Just label' -> label'
            Nothing -> case derivLabelRule label of
                Just r -> makeFreshLabel r
                Nothing -> label
