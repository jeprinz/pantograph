module Language.Pantograph.Generic.Grammar where

import Prelude

import Bug as Bug
import Bug.Assertion (Assertion, assert, assertInput_, just, makeAssertionBoolean)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, enumFromTo)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Expr (class IsExprLabel, class ReflectPathDir, prettyExprF'_unsafe, reflectPathDir, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, defer)
import Data.List (List(..), (:))
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
import Data.Variant (case_, on)
import Language.Pantograph.Generic.ChangeAlgebra (diff)
import Language.Pantograph.Generic.Unification (class Freshenable, composeSub, freshen', genFreshener, unify)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, pretty)
import Type.Direction (Up, _down, _up)

--------------------------------------------------------------------------------
-- IsRuleLabel
--------------------------------------------------------------------------------

class (Expr.IsExprLabel l, Eq r, Enum r, Bounded r, Show r, Pretty r) <= IsRuleLabel l r | r -> l where
  prettyExprF'_unsafe_RuleLabel :: Partial => r /\ Array String -> String
  language :: Language l r

expectedHypsCount :: forall l r. IsRuleLabel l r => r -> Int
expectedHypsCount r = do
  let Rule _ hyps _ = TotalMap.lookup r language
  Array.length hyps

--------------------------------------------------------------------------------
-- DerivLabel
--------------------------------------------------------------------------------

data DerivLabel l r 
  = DerivLabel r (Sort l)
  | DerivHole (Sort l)
  -- | TextBox String
  -- alternate idea: any hole of a (Name s) sort is a textbox

derivLabelRule :: forall l r. DerivLabel l r -> Maybe r
derivLabelRule (DerivLabel r _) = Just r
derivLabelRule (DerivHole _) = Nothing

derivLabelSort :: forall l r. DerivLabel l r -> Expr.MetaExpr l
derivLabelSort (DerivLabel _ s) = s
derivLabelSort (DerivHole s) = s

mapDerivLabelSort :: forall l r. (Sort l -> Sort l) -> DerivLabel l r -> DerivLabel l r
mapDerivLabelSort f (DerivLabel r sort) = DerivLabel r (f sort)
mapDerivLabelSort f (DerivHole sort) = DerivHole (f sort)

infix 8 DerivLabel as |-

derive instance Generic (DerivLabel l r) _
instance (Show l, Show r) => Show (DerivLabel l r) where show x = genericShow x
derive instance (Eq l, Eq r) => Eq (DerivLabel l r)
derive instance (Ord l, Ord r) => Ord (DerivLabel l r)

instance IsRuleLabel l r => Pretty (DerivLabel l r) where
  pretty (DerivLabel r ix) = pretty r <> "(" <> pretty ix <> ")"
  pretty (DerivHole ix) = "(? : " <> pretty ix <> ")"

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
  prettyExprF'_unsafe (DerivHole ix /\ []) = "(? : " <> pretty ix <> ")"

  expectedKidsCount (DerivLabel r _) = Expr.expectedKidsCount (AsExprLabel r)
  expectedKidsCount (DerivHole _) = 0

--------------------------------------------------------------------------------
-- Sorts
--------------------------------------------------------------------------------
-- currently, we implicity have:
type Sort l = Expr.MetaExpr l

--but, we could instead have:
data Sort' l = SortInject l | Str String | Name {-String-}


--------------------------------------------------------------------------------
-- DerivTerm
--------------------------------------------------------------------------------
type DerivTerm l r = Expr.Expr (DerivLabel l r)
type DerivExpr l r = Expr.Expr (DerivLabel l r)
type DerivPath dir l r = Expr.Path dir (DerivLabel l r)
type DerivTooth l r = Expr.Tooth (DerivLabel l r)
type DerivZipper l r = Expr.Zipper (DerivLabel l r)
type DerivZipperp l r = Expr.Zipperp (DerivLabel l r)

holeDerivExpr :: forall l r. Expr.MetaExpr l -> DerivExpr l r
holeDerivExpr ix = DerivHole ix % []

derivExprSort :: forall l r. DerivExpr l r -> Sort l
derivExprSort (dl % _) = derivLabelSort dl

derivExprRuleLabel :: forall l r. DerivExpr l r -> Maybe r
derivExprRuleLabel (dl % _) = derivLabelRule dl

derivToothSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothSort (Expr.Tooth (DerivLabel _r sort) _) = sort
derivToothSort (Expr.Tooth (DerivHole sort) _) = sort

derivToothInteriorSort :: forall l r. IsRuleLabel l r => DerivTooth l r -> Sort l
derivToothInteriorSort (Expr.Tooth (DerivLabel r _) kidsPath) = do
  let Rule _mvars hyps _con = TotalMap.lookup r language
  assert (just "derivToothInteriorSort" $ hyps Array.!! ZipList.leftLength kidsPath) identity
derivToothInteriorSort (Expr.Tooth (DerivHole _sort) _) = Bug.bug "[derivToothInteriorSort] should not have a tooth into DerivHole"

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

--------------------------------------------------------------------------------
-- LanguageChanges, ChangeRule
--------------------------------------------------------------------------------

type LanguageChanges l r = TotalMap r (ChangeRule l) -- changes go from child to parent

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
  = SetCursorAction (Lazy (DerivZipper l r))
  | Dig


defaultEditsAtDerivZipper :: forall l r. IsRuleLabel l r => Sort l -> DerivZipper l r -> Array (Edit l r)
defaultEditsAtDerivZipper topSort dz = 
  Array.concat $
  [digEdit] Array.:
  flip Array.foldMap (enumFromTo bottom top :: Array r) \r -> do
    let Rule mvars hyps con = TotalMap.lookup r language
    -- For each hyp, there is an edit that wraps with a tooth into that hyp,
    -- where the other kids are holes
    case ZipList.zips (List.fromFoldable hyps) of
      Nothing -> []
      -- `hyp` is what _would_ be at the bottom of the tooth
      Just hypZips -> Array.fromFoldable $ hypZips <#> \(hypPath /\ _hyp) -> do
        let rho = genFreshener mvars
        let tooth0 = freshen' rho $ 
              -- Each kid of the tooth is a hole deriv
              Expr.Tooth (DerivLabel r con) (holeDerivExpr <$> hypPath) 

        -- In `isValidTooth`, we do only the necessary computation to check if
        -- the tooth is valid to wrap around the cursor. In particular, we don't
        -- yet apply the unifying substitutions to the while program.
        let isValidTooth = do
              -- Unify sort of the tooth with the sort of the bottom of the path
              -- above the cursor
              _ /\ sigma1 <- unify (derivToothSort tooth0) (derivPathSort topSort (Expr.zipperPath dz))
              let tooth1 = mapDerivLabelSort (Expr.subMetaExprPartially sigma1) <$> tooth0
              -- Unify sort of the tooth interior with the sort of the expression
              -- at the cursor
              _ /\ sigma2 <- unify (derivToothInteriorSort tooth1) (derivExprSort (Expr.zipperExpr dz))
              let tooth2 = mapDerivLabelSort (Expr.subMetaExprPartially sigma2) <$> tooth1

              pure (composeSub sigma2 sigma1 /\ tooth2)

        -- In `newCursor`, we do the rest of the computations involved in
        -- computing the new cursor, in particular inserting the tooth and
        -- applying the unifying substitutions to the whole program. It's `Lazy`
        -- so that we only invoke this when actually _doing_ the action.
        let newCursor sigma tooth = defer \_ -> do
              let path = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> Expr.zipperPath dz
              let expr = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> Expr.zipperExpr dz
              Expr.Zipper (Expr.stepPath tooth path) expr

        case isValidTooth of
          Nothing -> []
          Just (sigma /\ tooth) -> pure
            { label: pretty r 
            , preview: pretty tooth
            , action: SetCursorAction (newCursor sigma tooth)
            }

defaultEditsAtHoleInterior :: forall l r. IsRuleLabel l r => DerivPath Up l r -> Sort l -> Array (Edit l r)
defaultEditsAtHoleInterior path sort =
  -- For each rule, there is an edit that fills the hole with that constructor,
  -- where all the kids are hole derivs
  flip Array.foldMap (enumFromTo bottom top :: Array r) \r -> do
  let Rule mvars hyps con = TotalMap.lookup r language
  let rho = genFreshener mvars
  let fill0 = freshen' rho $
        -- Each kid is a hole deriv
        Expr.Expr (DerivLabel r con) (holeDerivExpr <$> hyps)
  
  -- In `isValidFill`, only do computation necessary to check if fill is valid
  let isValidFill = do
        -- Unify sort of the fill with the sort of the hole
        _ /\ sigma <- unify (derivExprSort fill0) sort
        let fill1 = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> fill0

        pure (sigma /\ fill1)

  -- In `newCursor`, we are lazily doing the rest of the comptuations necessary
  -- to compute the new cursor, so that we only do this when we are actually
  -- applying the action.
  let newCursor sigma fill = defer \_ -> do
        let path' = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> path
        let expr' = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> fill
        Expr.Zipper path' expr'

  case isValidFill of
    Nothing -> []
    Just (sigma /\ fill) -> pure
      { label: pretty r
      , preview: pretty fill
      , action: SetCursorAction (newCursor sigma fill)
      }

digEdit :: forall l r. IsRuleLabel l r => Edit l r
digEdit = 
  { label: "dig"
  , preview: "?"
  , action: Dig
  }
