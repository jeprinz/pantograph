module Language.Pantograph.Specific.CustomLanguage where

import Language.Pantograph.Generic.Grammar
import Prelude

import Bug.Assertion (assertI, just)
import Control.Plus (empty)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel)
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple.Nested ((/\))
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty)

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

data PreSortLabel = Num | Bool

derive instance Generic PreSortLabel _
instance Show PreSortLabel where
  show x = genericShow x

instance Eq PreSortLabel where
  eq x = genericEq x

instance Ord PreSortLabel where
  compare x y = genericCompare x y

instance EncodeJson PreSortLabel where
  encodeJson a = genericEncodeJson a

instance DecodeJson PreSortLabel where
  decodeJson a = genericDecodeJson a

instance Pretty PreSortLabel where
  pretty = show

instance IsExprLabel PreSortLabel where
  prettyExprF'_unsafe (Num /\ _) = "Num"
  prettyExprF'_unsafe (Bool /\ _) = "Bool"

  expectedKidsCount _ = 0

--------------------------------------------------------------------------------
-- Shorter Aliases
--------------------------------------------------------------------------------

-- Expr
type Expr = Expr.Expr PreSortLabel
type MetaExpr = Expr.MetaExpr PreSortLabel
type Zipper = Expr.Zipper PreSortLabel
type Tooth = Expr.Tooth PreSortLabel
type Sort = Grammar.Sort PreSortLabel

-- Grammar
type DerivTerm = Grammar.DerivTerm PreSortLabel RuleLabel
type DerivLabel = Grammar.DerivLabel PreSortLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir PreSortLabel RuleLabel
type DerivZipper = Grammar.DerivZipper PreSortLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp PreSortLabel RuleLabel
type SSTerm = Smallstep.SSTerm PreSortLabel RuleLabel
type LanguageChanges = Grammar.LanguageChanges PreSortLabel RuleLabel
type SortChange = Grammar.SortChange PreSortLabel
type ChangeRule = Grammar.ChangeRule PreSortLabel
type SortSub = Grammar.SortSub PreSortLabel

-- Rendering
type Query = Base.Query
type Output = Base.Output PreSortLabel RuleLabel
type HoleyDerivZipper = Base.HoleyDerivZipper PreSortLabel RuleLabel

type Edit = Edit.Edit PreSortLabel RuleLabel
type Action = Edit.Action PreSortLabel RuleLabel

-- SmallStep
type StepRule = Smallstep.StepRule PreSortLabel RuleLabel

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

-- | Naming convention: <title>_<output sort>
data RuleLabel
  = True
  | False
  | One
  | Zero
  | And
  | Plus
  | Hole
  | Newline

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where
  show x = genericShow x

instance Enum RuleLabel where
  pred x = genericPred x
  succ x = genericSucc x

instance Bounded RuleLabel where
  bottom = genericBottom
  top = genericTop

instance Pretty RuleLabel where
  pretty = show

instance EncodeJson RuleLabel where
  encodeJson a = genericEncodeJson a

instance DecodeJson RuleLabel where
  decodeJson a = genericDecodeJson a

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language PreSortLabel RuleLabel
type Rule = Grammar.Rule PreSortLabel

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel _ = ""

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    Hole -> Yes false
    _ -> No

  -- TODO: is this right?
  defaultDerivTerm' _ = Nothing -- TODO

language :: Language
language = TotalMap.makeTotalMap case _ of
  _ -> unsafeCrashWith "TODO"

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ { renCtx: preRenCtx, rule, sort, sigma, dzipper, mb_parent, renderTerm } = unsafeCrashWith "TODO"

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

makeEditFromPath = DefaultEdits.makeEditFromPath (unsafeCrashWith "?forgetSorts") (unsafeCrashWith "?splitChange")

editsAtCursor :: Sort -> Array Edit
editsAtCursor _ = [] -- TODO: auto derive edits from wraps and inserts 

--------------------------------------------------------------------------------
-- Changes
--------------------------------------------------------------------------------

isValidCursorSort :: Sort -> Boolean
isValidCursorSort _ = true -- TODO: is this right?

isValidSelectionSorts :: { bottom :: Sort, top :: Sort } -> Boolean
isValidSelectionSorts { bottom, top } = bottom == top

keyAction :: String -> Sort -> Maybe Action
keyAction key cursorSort = unsafeCrashWith "TODO"

extraQueryEdits :: Sort -> String -> Array Edit
extraQueryEdits _ _ = []

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "CustomLanguage" $ Grammar.defaultDerivTerm (Bool %|-* [])
  , editsAtCursor
  , arrangeDerivTermSubs
  , stepRules: empty
  , isValidCursorSort
  , isValidSelectionSorts
  , keyAction
  , extraQueryEdits
  , splitChange: \_ -> unsafeCrashWith "TODO"
  , editsAtHoleInterior: \_ -> []
  , onDelete: \cursorSort -> ChangeAlgebra.inject cursorSort
  , generalizeDerivation: \other -> ChangeAlgebra.inject other
  , specializeDerivation: \clipboard _cursor -> ChangeAlgebra.inject clipboard
  , forgetSorts: \_ -> Maybe.Nothing
  , clipboardSort: \s -> s
  }
