module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug.Assertion (Assertion, assertInput_, makeAssertionBoolean)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, enumFromTo)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (class IsExprLabel, prettyExprF'_unsafe, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.List as List
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Language.Pantograph.Generic.Unification (class Freshenable, freshen', genFreshener)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, pretty, (<+>))
import Type.Direction (Up)

--------------------------------------------------------------------------------
-- IsRuleLabel
--------------------------------------------------------------------------------

class (Expr.IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r

expectedHypsCount :: forall l r. IsRuleLabel l r => r -> Int
expectedHypsCount r = do
  -- let Rule _ hyps _ = TotalMap.lookup (InjectRuleLabel r) language 
  let Rule _ hyps _ = TotalMap.lookup r language 
  Array.length hyps

-- --------------------------------------------------------------------------------
-- -- HoleyExprLabel
-- --------------------------------------------------------------------------------

-- data HoleyExprLabel l
--   = ConcreteExprLabel l
--   | HoleInteriorSort

-- -- !TODO does the version without `MetaExpr` ever get used? if not, then maybe
-- -- just make that the definition of HoleyExpr (i.e. HoleyExpr inherently can
-- -- have metavars)
-- type HoleyExpr l = Expr.Expr (HoleyExprLabel l)
-- type MetaHoleyExpr l = Expr.MetaExpr (HoleyExprLabel l)

-- pureHoleyExpr :: forall l. l -> Array (HoleyExpr l) -> HoleyExpr l
-- pureHoleyExpr l = (ConcreteExprLabel l % _)

-- injectHoleyExpr :: forall l. Expr.Expr l -> HoleyExpr l
-- injectHoleyExpr (l % kids) = pureHoleyExpr l (injectHoleyExpr <$> kids)

-- pureMetaHoleyExpr :: forall l. l -> Array (MetaHoleyExpr l) -> MetaHoleyExpr l
-- pureMetaHoleyExpr l = (pure (ConcreteExprLabel l) % _)

-- injectMetaHoleyExpr :: forall l. Expr.MetaExpr l -> MetaHoleyExpr l
-- injectMetaHoleyExpr (l % kids) = (ConcreteExprLabel <$> l) % (injectMetaHoleyExpr <$> kids)

-- derive instance Generic (HoleyExprLabel l) _
-- instance Show l => Show (HoleyExprLabel l) where show x = genericShow x
-- derive instance Eq l => Eq (HoleyExprLabel l)
-- derive instance Ord l => Ord (HoleyExprLabel l)
-- derive instance Functor HoleyExprLabel

-- instance Expr.IsExprLabel l => Pretty (HoleyExprLabel l) where
--   pretty (ConcreteExprLabel l) = pretty l
--   pretty l = show l

-- instance Expr.IsExprLabel l => Expr.IsExprLabel (HoleyExprLabel l) where
--   prettyExprF'_unsafe (ConcreteExprLabel l /\ kids) = prettyExprF'_unsafe (l /\ kids)
--   prettyExprF'_unsafe (HoleInteriorSort /\ []) = "?"

--   expectedKidsCount = case _ of
--     ConcreteExprLabel l -> Expr.expectedKidsCount l
--     HoleInteriorSort -> 0

-- --------------------------------------------------------------------------------
-- -- HoleyRuleLabel
-- --------------------------------------------------------------------------------

-- data HoleyRuleLabel r
--   = InjectRuleLabel r
--   | Hole
--   | HoleInterior

-- derive instance Generic (HoleyRuleLabel r) _
-- instance Show r => Show (HoleyRuleLabel r) where show x = genericShow x
-- derive instance Eq r => Eq (HoleyRuleLabel r)
-- derive instance Ord r => Ord (HoleyRuleLabel r)
-- instance Bounded r => Bounded (HoleyRuleLabel r) where
--   top = genericTop
--   bottom = genericBottom
-- instance (Enum r, Bounded r) => Enum (HoleyRuleLabel r) where
--   pred x = genericPred x
--   succ x = genericSucc x

-- instance IsRuleLabel l r => Pretty (HoleyRuleLabel r) where
--   pretty (InjectRuleLabel r) = pretty r
--   pretty Hole = "[?]"
--   pretty HoleInterior = "?"

-- instance IsRuleLabel l r => IsRuleLabel (HoleyExprLabel l) (HoleyRuleLabel r) where
--   prettyExprF'_unsafe_RuleLabel (InjectRuleLabel r /\ kids) = prettyExprF'_unsafe_RuleLabel (r /\ kids)
--   prettyExprF'_unsafe_RuleLabel (Hole /\ [kid]) = "Hole[" <> kid <> "]"
--   prettyExprF'_unsafe_RuleLabel (HoleInterior /\ []) = "?"
  
--   language = TotalMap.makeTotalMap case _ of
--     -- inject rule from base language
--     InjectRuleLabel r -> ConcreteExprLabel <$> TotalMap.lookup r language
--     -- intro new "hole" rule that can produce an instance of any sort
--     Hole -> makeRule ["sort"] \[sort] -> 
--       [ HoleInteriorSort %* [] ]
--       /\ ----------------
--       ( sort )
--     -- intro new "hole interior" rule that can produce only a hole interior
--     -- (which goes inside a hole)
--     HoleInterior -> makeRule [] \[] ->
--       [ ]
--       /\ ----------------
--       ( HoleInteriorSort %* [] )

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

data DerivLabel l r 
  = DerivLabel r (Expr.MetaExpr l)
  | DerivHole (Expr.MetaExpr l)

-- derivLabelRuleLabel :: forall l r. DerivLabel l r -> HoleyRuleLabel r
derivLabelRuleLabel :: forall l r. DerivLabel l r -> Maybe r
derivLabelRuleLabel (DerivLabel r _) = Just r
derivLabelRuleLabel (DerivHole _) = Nothing

derivLabelSort :: forall l r. DerivLabel l r -> Expr.MetaExpr l
derivLabelSort (DerivLabel _ s) = s
derivLabelSort (DerivHole s) = s

infix 8 DerivLabel as |-

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance IsRuleLabel l r => Pretty (DerivLabel l r) where
  pretty (DerivLabel r ix) = pretty r <> "(" <> pretty ix <> ")"
  pretty (DerivHole ix) = "(?" <+> pretty ix <> ")"

instance Freshenable (DerivLabel l r) where
  freshen rho (DerivLabel hr me) = DerivLabel hr (freshen' rho me)
  freshen rho (DerivHole me) = DerivHole (freshen' rho me)

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
  prettyExprF'_unsafe (DerivLabel r (Expr.Expr _l _metaExpr) /\ kids) = 
    Expr.prettyExprF (AsExprLabel r /\ kids)
  prettyExprF'_unsafe (DerivHole ix /\ []) = "(?" <+> pretty ix <> ")"

  expectedKidsCount (DerivLabel r _) = Expr.expectedKidsCount (AsExprLabel r)
  expectedKidsCount (DerivHole _) = 0

--------------------------------------------------------------------------------
-- DerivTerm
--------------------------------------------------------------------------------

{-
--DerivTerm needs built-in hole?
--DerivTerm and DerivPath need boundaries?
-}
type DerivTerm l r = Expr.Expr (DerivLabel l r)
-- !HENRY: personally, I think it's better to keep the name `Expr` consistent, 
-- rather than conflate generic "terms" and "terms" in a particular language 
-- (e.g. as in "terms and their types")
type DerivExpr l r = Expr.Expr (DerivLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivLabel l r)
type DerivTooth l r = Expr.Tooth (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipperp l r = Expr.Zipperp (DerivLabel l r)

-- derivExprSort :: forall l r. DerivExpr l r -> HoleyRuleLabel r
-- derivExprSort (dl % _) = derivLabelRuleLabel dl

-- derivExprSort :: forall l r. DerivExpr l r -> MetaHoleyExpr l
derivExprSort :: forall l r. DerivExpr l r -> Expr.MetaExpr l
derivExprSort (dl % _) = derivLabelSort dl

-- derivExprRuleLabel :: forall l r. DerivExpr l r -> HoleyRuleLabel r
derivExprRuleLabel :: forall l r. DerivExpr l r -> Maybe r
derivExprRuleLabel (dl % _) = derivLabelRuleLabel dl

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
    (Array (Expr.MetaExpr l))
    (Expr.MetaExpr l)

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
  (Array (Expr.MetaExpr l) -> Array (Expr.MetaExpr l) /\ Expr.MetaExpr l) -> 
  Rule l
makeRule' = assertInput_ (\strs -> nonDuplicateArray "makeRule" ("All metavar strings must be different among: " <> show strs) strs) \strs f ->
  let mxs = Expr.freshMetaVar <$> strs in
  let es = Expr.fromMetaVar <$> mxs in
  let hyps /\ con = f es in
  Rule (Set.fromFoldable mxs) hyps con

makeRule :: forall l. 
  Array String ->
  (Partial => Array (Expr.MetaExpr l) -> Array (Expr.MetaExpr l) /\ Expr.MetaExpr l) -> 
  Rule l
makeRule = \strs f -> makeRule' strs (unsafePartial f)

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

-- | A `Language` associates each `RuleLabel` to a `Rule`
type Language l r = TotalMap r (Rule l)
-- type HoleyLanguage l r = Language l (HoleyRuleLabel r)

--------------------------------------------------------------------------------
-- LanguageChanges, ChangeRule
--------------------------------------------------------------------------------

type LanguageChanges l r = TotalMap r (ChangeRule l) -- changes go from child to parent
-- type HoleyLanguageChanges l r = LanguageChanges l (HoleyRuleLabel r)

defaultLanguageChanges :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r => Language l r -> LanguageChanges l r
defaultLanguageChanges = map \(Rule mvars kids parent) ->
  ChangeRule mvars ((flip diff) parent <$> kids)

-- | A `ChangeRule` is oriented from parent to kid i.e. it describes the changes
-- to apply to the parent's kids.
data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (Expr.MetaChange l))

derive instance Functor ChangeRule
derive instance Foldable ChangeRule
derive instance Traversable ChangeRule

--------------------------------------------------------------------------------
-- Edit, Action
--------------------------------------------------------------------------------

type Edit l r =
  { label :: String
  , preview :: String
  , action :: Action l r
  }

data Action l r
  = SetDerivZipperAction (Lazy (Maybe (DerivZipper l r))) -- !TODO phase this out for more specific edits
  | WrapDerivZipper (Unit -> DerivPath Up l r)
  | Dig

defaultEdits :: forall l r. IsRuleLabel l r => Array (Edit l r)
defaultEdits = Array.concat [ruleEdits]
  where
  ruleEdits = flip Array.foldMap (enumFromTo bottom top :: Array r) \r -> do
    let Rule mvars hyps con = TotalMap.lookup r language
    -- for each hyp, there is an edit that wraps with a tooth into that hyp,
    -- where the other kids are holes
    case ZipList.zips (List.fromFoldable hyps) of
      Nothing -> []
      Just hypZips -> Array.fromFoldable $ hypZips <#> \(hypPath /\ _hyp) -> do
        let tooth = Expr.Tooth 
              -- (DerivLabel (InjectRuleLabel r) (injectMetaHoleyExpr con))
              (DerivLabel r con)
              -- (holeDerivExpr <<< injectMetaHoleyExpr <$> hypPath) -- each other kid is a hole deriv
              (holeDerivExpr <$> hypPath) -- each other kid is a hole deriv
        { label: pretty r
        , preview: pretty tooth
        , action: WrapDerivZipper \_ -> do
            let rho = genFreshener mvars
            let tooth' = freshen' rho tooth
            Expr.Path $ List.singleton tooth'
        }
    
-- holeDerivExpr :: forall l r. MetaHoleyExpr l -> DerivExpr l r
holeDerivExpr :: forall l r. Expr.MetaExpr l -> DerivExpr l r
holeDerivExpr ix = 
  -- DerivLabel Hole prop % 
    -- [DerivLabel HoleInterior (pure HoleInteriorSort % []) % []]
  DerivHole ix % []

digEdit :: forall l r. IsRuleLabel l r => Edit l r
digEdit = 
  { label: "dig"
  , preview: "?"
  , action: Dig
  }