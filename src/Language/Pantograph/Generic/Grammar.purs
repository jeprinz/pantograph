module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug.Assertion (Assertion, assertInput_, makeAssertionBoolean)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (class IsExprLabel, expectedKidsCount, fromMetaVar, prettyExprF'_unsafe, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple.Nested (type (/\), (/\))
import Hole as Hole
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, pretty)

--------------------------------------------------------------------------------
-- IsRuleLabel
--------------------------------------------------------------------------------

class (IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r

expectedHypsCount :: forall l r. IsRuleLabel l r => r -> Int
expectedHypsCount r = do
  let Rule _ hyps _ = TotalMap.lookup r language 
  Array.length hyps

--------------------------------------------------------------------------------
-- HoleyExprLabel
--------------------------------------------------------------------------------

data HoleyExprLabel l
  = BaseExprLabel l
  | HoleInteriorSort

derive instance Generic (HoleyExprLabel l) _
instance Show l => Show (HoleyExprLabel l) where show x = genericShow x
derive instance Eq l => Eq (HoleyExprLabel l)
derive instance Ord l => Ord (HoleyExprLabel l)
derive instance Functor HoleyExprLabel

instance Pretty l => Pretty (HoleyExprLabel l) where
  pretty (BaseExprLabel l) = pretty l
  pretty HoleInteriorSort = "HoleInteriorSort"

instance IsExprLabel l => IsExprLabel (HoleyExprLabel l) where
  prettyExprF'_unsafe = Hole.hole "IsExprLabel (HoleyExprLabel l) . prettyExprF'_unsafe"
  expectedKidsCount = Hole.hole "IsExprLabel (HoleyExprLabel l) . expectedKidsCount"

--------------------------------------------------------------------------------
-- HoleyRuleLabel
--------------------------------------------------------------------------------

data HoleyRuleLabel r
  = BaseRuleLabel r
  | Hole
  | HoleInterior

derive instance Generic (HoleyRuleLabel r) _
instance Show r => Show (HoleyRuleLabel r) where show x = genericShow x
derive instance Eq r => Eq (HoleyRuleLabel r)
derive instance Ord r => Ord (HoleyRuleLabel r)
instance Bounded r => Bounded (HoleyRuleLabel r) where
  top = genericTop
  bottom = genericBottom
instance (Enum r, Bounded r) => Enum (HoleyRuleLabel r) where
  pred x = genericPred x
  succ x = genericSucc x

instance Pretty r => Pretty (HoleyRuleLabel r) where
  pretty (BaseRuleLabel r) = pretty r
  pretty Hole = "Hole"
  pretty HoleInterior = "HoleInterior"

instance IsRuleLabel l r => IsRuleLabel (HoleyExprLabel l) (HoleyRuleLabel r) where
  prettyExprF'_unsafe_RuleLabel (BaseRuleLabel r /\ kids) = prettyExprF'_unsafe_RuleLabel (r /\ kids)
  prettyExprF'_unsafe_RuleLabel (Hole /\ [kid]) = "Hole[" <> kid <> "]"
  prettyExprF'_unsafe_RuleLabel (HoleInterior /\ []) = "?"
  
  language = TotalMap.makeTotalMap case _ of
    -- inject rule from base language
    BaseRuleLabel r -> BaseExprLabel <$> TotalMap.lookup r language
    -- intro new "hole" rule that can produce an instance of any sort
    Hole -> makeRule ["sort"] \[sort] -> 
      [ HoleInteriorSort %* [] ]
      /\ ----------------
      ( sort )
    -- intro new "hole interior" rule that can produce only a hole interior
    -- (which goes inside a hole)
    HoleInterior -> makeRule [] \[] ->
      [ ]
      /\ ----------------
      ( HoleInteriorSort %* [] )

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

data DerivLabel l r = DerivLabel (HoleyRuleLabel r) (Expr.MetaExpr (HoleyExprLabel l))

infix 8 DerivLabel as |-

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance IsRuleLabel l r => Pretty (DerivLabel l r) where
  pretty (DerivLabel r mexpr) = pretty r <> "(" <> pretty mexpr <> ")"

--------------------------------------------------------------------------------
-- AsExprLabel
--------------------------------------------------------------------------------

newtype AsExprLabel a = AsExprLabel a
derive newtype instance Eq a => Eq (AsExprLabel a)
derive newtype instance Ord a => Ord (AsExprLabel a)
derive newtype instance Show a => Show (AsExprLabel a)
derive newtype instance Pretty a => Pretty (AsExprLabel a)

-- | Can pretend that a rule label is the expr label of derivations.
instance IsRuleLabel l r => IsExprLabel (AsExprLabel r) where
  prettyExprF'_unsafe (AsExprLabel r /\ kids) = prettyExprF'_unsafe_RuleLabel (r /\ kids)
  expectedKidsCount (AsExprLabel r) = expectedHypsCount r

instance IsRuleLabel l r => IsExprLabel (DerivLabel l r) where
  -- NOTE: This implementation ignores the expression label and metaexpression,
  -- but maybe we want to print those at some point for debugging?
  prettyExprF'_unsafe (DerivLabel r (Expr.Expr _l _metaExpr) /\ kids) = 
    Expr.prettyExprF (AsExprLabel r /\ kids)

  expectedKidsCount (DerivLabel r _) = expectedKidsCount (AsExprLabel r)

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
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipperp l r = Expr.Zipperp (DerivLabel l r)

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
data Rule l = Rule (Set Expr.MetaVar) (Array (Expr.MetaExpr l)) (Expr.MetaExpr l)

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
  let es = fromMetaVar <$> mxs in
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

--------------------------------------------------------------------------------
-- LanguageChanges, ChangeRule
--------------------------------------------------------------------------------

type LanguageChanges l r = TotalMap r (ChangeRule l) -- changes go from child to parent

defaultLanguageChanges :: forall l r. IsExprLabel l => IsRuleLabel l r => Language l r -> LanguageChanges l r
defaultLanguageChanges = map \(Rule mvars kids parent) ->
  ChangeRule mvars ((flip diff) parent <$> kids)

-- | A `ChangeRule` is oriented from parent to kid i.e. it describes the changes
-- to apply to the parent's kids.
data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (Expr.MetaChange l))

derive instance Functor ChangeRule
derive instance Foldable ChangeRule
derive instance Traversable ChangeRule

