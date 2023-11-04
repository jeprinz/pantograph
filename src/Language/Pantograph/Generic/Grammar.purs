module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug (bug)
import Bug.Assertion (Assertion, assert, assertInput_, assertInterface_, equal, just, makeAssertionBoolean)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.Either (Either(..))
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
import Debug (trace, traceM)
import Hole (hole)
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Language.Pantograph.Generic.Unification (class Freshenable, Sub, freshen', unifyLists, composeSub, composeSubs)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, bullets, pretty)
import Type.Direction (_down, _up)
import Type.Direction as Dir
import Util as Util
import Data.Foldable as Foldable

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

{-
The knowledge of if a rule is a hole rule is used for two things:
-- Should it have an inner hole
-- Should default terms be automatically filled into it after a substitution?
-}
data IsHoleRule = Yes {-Has inner hole-}Boolean | No

class (Expr.IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r
  isHoleRuleTotalMap :: TotalMap r IsHoleRule
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

hasInnerHole :: forall l r. IsRuleLabel l r => r -> Boolean
hasInnerHole r = case TotalMap.lookup r isHoleRuleTotalMap of
    Yes true -> true
    _ -> false


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
isHoleDerivLabel l =
    case l of
        (DerivLabel r _sub) | Yes _ <- TotalMap.lookup r isHoleRuleTotalMap -> pure $ derivLabelSort l
        (DerivString "") -> pure $ derivLabelSort l
        _ -> Nothing

isHoleDerivTerm :: forall l r. IsRuleLabel l r => DerivTerm l r -> Maybe (Sort l)
isHoleDerivTerm (dl % _) = isHoleDerivLabel dl

isInnerHoleDerivLabel :: forall l r. IsRuleLabel l r => DerivLabel l r -> Maybe (Sort l)
isInnerHoleDerivLabel (DerivLabel r sub) | Yes true <- TotalMap.lookup r isHoleRuleTotalMap = pure (getSortFromSub r sub)
isInnerHoleDerivLabel _ = empty

isInnerHoleDerivTerm :: forall l r. IsRuleLabel l r => DerivTerm l r -> Maybe (Sort l)
isInnerHoleDerivTerm (dl % _) = isInnerHoleDerivLabel dl

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

derivLabelSub :: forall l r. DerivLabel l r -> Maybe (SortSub l)
derivLabelSub (DerivLabel _ s) = Just s
derivLabelSub (DerivString _) = Nothing

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

injectSortLabelDerivLabel sortLabel kids = SInj sortLabel %* kids

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
fillDefaultsDerivPath (Expr.Path teeth) = Expr.Path $ map fillDefaultsDerivTooth teeth

fillDefaultsDerivTooth :: forall l r. IsRuleLabel l r => DerivTooth l r -> DerivTooth l r
fillDefaultsDerivTooth (Expr.Tooth label (ZipList.Path {left, right})) =
    Expr.Tooth label (ZipList.Path {left: map fillDefaultsDerivTerm left, right: map fillDefaultsDerivTerm right})

fillDefaultsDerivZipper :: forall l r. IsRuleLabel l r => DerivZipper l r -> DerivZipper l r
fillDefaultsDerivZipper (Expr.Zipper path term) = Expr.Zipper (fillDefaultsDerivPath path) (fillDefaultsDerivTerm term)

subDerivTerm :: forall l r. IsRuleLabel l r => SortSub l -> DerivTerm l r -> DerivTerm l r
subDerivTerm sub =  map (subDerivLabel sub) >>> fillDefaultsDerivTerm

subDerivPath :: forall d l r. IsRuleLabel l r => SortSub l -> DerivPath d l r -> DerivPath d l r
subDerivPath sub = map (subDerivLabel sub) >>> fillDefaultsDerivPath

subDerivZipper :: forall l r. IsRuleLabel l r => SortSub l -> DerivZipper l r -> DerivZipper l r
subDerivZipper sub = map (subDerivLabel sub) >>> fillDefaultsDerivZipper

subDerivTooth :: forall l r. IsRuleLabel l r => SortSub l -> DerivTooth l r -> DerivTooth l r
subDerivTooth sub = map (subDerivLabel sub) >>> fillDefaultsDerivTooth


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
  = SInj l
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
freshMetaVarSort name = (Expr.MV (freshMetaVar name)) % []

-- This is used as part of a DSL for building up sorts.
sor :: forall l. l -> Expr.Meta (SortLabel l)
sor l = Expr.MInj (SInj l)

nameSort :: forall l. String -> Expr.MetaExpr (SortLabel l)
nameSort name = Expr.MInj (StringSortLabel name) % []

-- This is used as part of a DSL for building up sort changes
csor :: forall l. l -> Expr.ChangeLabel (Expr.Meta (SortLabel l))
csor l = Expr.CInj (Expr.MInj (SInj l))

-- assert that a label is a name and return the string
matchNameLabel :: forall l. Sort l -> String
matchNameLabel (Expr.Expr (Expr.MInj NameSortLabel) [Expr.Expr (Expr.MInj (StringSortLabel s)) []]) = s
matchNameLabel _ = bug "wasn't name"

-- assert that a label is a string and return the string
matchStringLabel :: forall l. Sort l -> String
matchStringLabel (Expr.Expr (Expr.MInj (StringSortLabel s)) []) = s
matchStringLabel _ = bug "wasn't stringlabel"

instance Pretty l => Pretty (SortLabel l) where
  pretty (SInj l) = pretty l
  pretty NameSortLabel = "NameSort"
  pretty (StringSortLabel str) = "\"" <> str <> "\"" 

instance IsExprLabel l => IsExprLabel (SortLabel l) where
  prettyExprF'_unsafe = case _ of
    SInj l /\ kids -> prettyExprF'_unsafe (l /\ kids)
    NameSortLabel /\ [string] -> "Name(" <> string <> ")"
    StringSortLabel str /\ [] -> "\"" <> str <> "\"" 

  expectedKidsCount = case _ of
    SInj l -> expectedKidsCount l
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
derivPathSort :: forall l r. IsRuleLabel l r => DerivPath Dir.Up l r -> Sort l -> Sort l
derivPathSort dpath botSort = do
  let dpath' = Expr.toDownPath dpath
  case dpath' of
    Expr.Path Nil -> botSort
    Expr.Path (th : ths) -> derivPathSort (Expr.Path ths) (derivToothSort th)

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

derivZipperLabel :: forall l r. DerivZipper l r -> DerivLabel l r
derivZipperLabel (Expr.Zipper _ (l % _)) = l

derivZipperpSorts :: forall l r. IsRuleLabel l r => DerivZipperp l r -> Sort l /\ Sort l
derivZipperpSorts (Expr.Zipperp _ selection dterm) =
    let selection' = case selection of
            Left downPath -> Expr.reversePath downPath
            Right upPath -> upPath
    in
    let botSort = (derivTermSort dterm) in
    derivPathSort selection' botSort /\ botSort

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
--derive instance Foldable Rule
--derive instance Traversable Rule

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
--derive instance Foldable ChangeRule
--derive instance Traversable ChangeRule


--------------------------------------------------------------------------------
----------- Typechecking (and inference) for any grammar -----------------------
--------------------------------------------------------------------------------

{-
Inputs a term, and checks if it typechecks up to unification of metavariables. If it does, returns the sub.
To do regular typechecking, then check if the sub is empty.
-}

inferTerms :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    List (DerivTerm l r) -> Maybe (SortSub l)
inferTerms Nil = Just Map.empty
inferTerms (t : ts) = do
    sub1 <- infer t
    sub2 <- inferTerms (map (subDerivTerm sub1) ts)
    pure $ composeSub sub1 sub2

-- TODO: I guess this has a bug that it doesn't check the parent of the top node?
infer :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    DerivTerm l r -> Maybe (SortSub l)
infer (l % kids) = do
    sub1 <- inferTerms (List.fromFoldable kids)
    let inferredKidSorts = map (Expr.subMetaExprPartially sub1 <<< derivTermSort) kids
    let expectedKidSorts = map (Expr.subMetaExprPartially sub1) (kidSorts l)
    _ /\ sub2 <- unifyLists (List.fromFoldable inferredKidSorts) (List.fromFoldable expectedKidSorts)
    let allSubs = composeSub sub1 sub2
    pure $ allSubs

-- NOTE: the time complexity of this implementation is at least O(n^2), but it definitely can be O(n) if I try harder.
inferPath :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    Sort l -> DerivPath Dir.Up l r -> Maybe (SortSub l)
inferPath _ (Expr.Path Nil) = Just Map.empty
inferPath innerSort (Expr.Path ((Expr.Tooth l (ZipList.Path {left, right})) : ths)) = do
    sub1 <- inferTerms (List.fromFoldable ((List.fromFoldable left) <> right)) -- the order shouldn't matter?
    let inferredKidSorts = map (Expr.subMetaExprPartially sub1)
            ((derivTermSort <$> RevList.unreverse left) <> innerSort : (derivTermSort <$> right))
    let expectedKidSorts = map (Expr.subMetaExprPartially sub1) (kidSorts l)
    _ /\ sub2 <- unifyLists (List.fromFoldable inferredKidSorts) (List.fromFoldable expectedKidSorts)
    let sub12 = composeSub sub1 sub2
    sub3 <- inferPath (Expr.subMetaExprPartially sub12 (derivLabelSort l)) (subDerivPath sub12 (Expr.Path ths))
    pure $ composeSub sub12 sub3

forgetDerivLabelSorts :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivLabel l r -> DerivLabel l r
forgetDerivLabelSorts f label =
        case f label of
            Just label' -> label'
            Nothing -> case derivLabelRule label of
                Just r -> makeFreshLabel r
                Nothing -> label

--------------------------------------------------------------------------------
{-
These functions help with getting the change across a path. My original idea of LanguageChanges didn't work,
so hopefully this can deal with the other case.
-}

{-
Takes a derivlabel, returns a new one with a freshened sub where the Rule MetaVars map to fresh MetaVars,
and also returns a mapping of those metavar names to the original things that they mapped to.
-}
forgetCollectDerivLabel :: forall l r. IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivLabel l r -> DerivLabel l r /\ SortSub l
forgetCollectDerivLabel forgetSorts label =
    case (forgetSorts label) /\ label of
        Just label' /\ (DerivLabel r sigma) ->
            let (Rule vars _children _conclusion) = TotalMap.lookup r language in
            let freshVars = Map.fromFoldable (Set.map (\mv -> mv /\ (Expr.freshMetaVar (Expr.metaVarName mv))) vars) in
            let newSigma = map Expr.fromMetaVar freshVars :: SortSub l in
            let collectedMap = Map.fromFoldable (Set.map (\mv -> (Util.lookup' mv freshVars) /\ Util.lookup' mv newSigma) vars) :: SortSub l in
            (DerivLabel r newSigma) /\ collectedMap
        _ -> label /\ Map.empty -- TODO: is this going to work as a fallback case?

forgetCollectDerivTerm :: forall l r. IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivTerm l r -> DerivTerm l r /\ SortSub l
forgetCollectDerivTerm forgetSorts (label % kids) =
    let label' /\ sub1 = forgetCollectDerivLabel forgetSorts label in
    let kids' /\ subs2 = Array.unzip (map (forgetCollectDerivTerm forgetSorts) kids) in
    let sub = Map.union sub1 (Map.unions subs2) in
    (label' % kids') /\ sub

forgetCollectDerivTooth :: forall l r. IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivTooth l r -> DerivTooth l r /\ SortSub l
forgetCollectDerivTooth forgetSorts (Expr.Tooth label (ZipList.Path {left, right})) =
    let label' /\ sub1 = forgetCollectDerivLabel forgetSorts label in
    let left' /\ subs2 = RevList.unzip (map (forgetCollectDerivTerm forgetSorts) left) in
    let right' /\ subs3 = List.unzip (map (forgetCollectDerivTerm forgetSorts) right) in
    let sub = Map.union sub1 (Map.union (Map.unions subs2) (Map.unions subs3)) in
    (Expr.Tooth label' (ZipList.Path {left: left', right: right'})) /\ sub

forgetCollectDerivPath :: forall l r. IsRuleLabel l r =>
    (DerivLabel l r -> Maybe (DerivLabel l r)) -> DerivPath Dir.Up l r -> DerivPath Dir.Up l r /\ SortSub l
forgetCollectDerivPath forgetSorts (Expr.Path teeth) =
    let teeth' /\ subs2 = List.unzip (map (forgetCollectDerivTooth forgetSorts) teeth) in
    Expr.Path teeth' /\ Map.unions subs2
