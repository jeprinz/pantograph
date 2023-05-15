module Language.Pantograph.Generic.Grammar where

import Data.Either.Nested
import Prelude

import Bug.Assertion (assert, assertInput, makeAssertionBoolean)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Expr (class IsExprLabel, expectedKidsCount, fromMetaVar, (%))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap (TotalMap)
import Data.TotalMap as TotalMap
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple (curry, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))

--data ExprLabel

--data RuleLabel = Lam | App | Z | S

class (IsExprLabel l, Eq r, Enum r, Bounded r, Show r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r

expectedHypsCount :: forall l r. IsRuleLabel l r => r -> Int
expectedHypsCount r = do
  let Rule _ hyps _ = TotalMap.lookup r language 
  Array.length hyps

data DerivLabel l r = DerivLabel r (Expr.MetaExpr l)

infix 8 DerivLabel as |-

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

newtype AsExprLabel a = AsExprLabel a
derive newtype instance Eq a => Eq (AsExprLabel a)
derive newtype instance Ord a => Ord (AsExprLabel a)
derive newtype instance Show a => Show (AsExprLabel a)

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
type DerivZipperP l r = Expr.ZipperP (DerivLabel l r)

fromDerivExpr :: forall l r. Expr.Expr (DerivLabel l r) -> Expr.MetaExpr l
fromDerivExpr (Expr.Expr (DerivLabel r mexpr) kids) = mexpr

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
makeRule' = assertInput (\strs -> nonDuplicateArray "makeRule" ("All metavar strings must be different among: " <> show strs) strs) \strs f ->
  let mxs = Expr.freshMetaVar <$> strs in
  let es = fromMetaVar <$> mxs in
  let hyps /\ con = f es in
  Rule (Set.fromFoldable mxs) hyps con

makeRule :: forall l. 
  Array String ->
  (Partial => Array (Expr.MetaExpr l) -> Array (Expr.MetaExpr l) /\ Expr.MetaExpr l) -> 
  Rule l
makeRule = \strs f -> makeRule' strs (unsafePartial f)

-- | A `Language` associates each `RuleLabel` to a `Rule`
type Language l r = TotalMap r (Rule l)

-- | A `ChangeRule` is oriented from parent to kid i.e. it describes the changes
-- to apply to the parent's kids.
data ChangeRule l = ChangeRule (Set Expr.MetaVar) (Array (Expr.MetaChange l))

derive instance Functor ChangeRule
derive instance Foldable ChangeRule
derive instance Traversable ChangeRule

type LanguageChanges l r = TotalMap r (ChangeRule l)

defaultLanguageChanges :: forall l r. IsExprLabel l => IsRuleLabel l r => Language l r -> LanguageChanges l r
defaultLanguageChanges = map \(Rule mvars kids parent) ->
  ChangeRule mvars (diff parent <$> kids)
