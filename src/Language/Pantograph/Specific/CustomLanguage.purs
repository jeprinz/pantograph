module Language.Pantograph.Specific.CustomLanguage (PreSortLabel(..), RuleLabel(..), editorSpec) where

import Language.Pantograph.Generic.Grammar
import Prelude

import Bug.Assertion (assertI, just)
import Control.Plus (empty)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, injectExprChange, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Pantograph.Generic.ChangeAlgebra (lEndpoint, rEndpoint)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty)

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

data PreSortLabel
  = NumSort
  | BoolSort

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
  prettyExprF'_unsafe (NumSort /\ _) = "Num"
  prettyExprF'_unsafe (BoolSort /\ _) = "Bool"

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
  | BoolVar
  | NumVar

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

  defaultDerivTerm' sort = pure $ Grammar.makeLabel Hole [ "sort" /\ sort ] % []

language :: Language
language = TotalMap.makeTotalMap case _ of
  True -> Grammar.makeRule [] \[] ->
    [] /\ (BoolSort %|-* [])
  False -> Grammar.makeRule [] \[] ->
    [] /\ (BoolSort %|-* [])
  One -> Grammar.makeRule [] \[] ->
    [] /\ (NumSort %|-* [])
  Zero -> Grammar.makeRule [] \[] ->
    [] /\ (NumSort %|-* [])
  And -> Grammar.makeRule [] \[] ->
    [ BoolSort %|-* [], BoolSort %|-* [] ] /\ (BoolSort %|-* [])
  Plus -> Grammar.makeRule [] \[] ->
    [ NumSort %|-* [], NumSort %|-* [] ] /\ (NumSort %|-* [])
  Hole -> Grammar.makeRule [ "sort" ] \[ sort ] ->
    [] /\ sort
  Newline -> Grammar.makeRule [ "sort" ] \[ sort ] ->
    [ sort ] /\ sort
  BoolVar -> Grammar.makeRule [ "x" ] \[ x ] ->
    [ TypeOfLabel SortString %* [ x ] ] /\ (BoolSort %|-* [])
  NumVar -> Grammar.makeRule [ "x" ] \[ x ] ->
    [ TypeOfLabel SortString %* [ x ] ] /\ (NumSort %|-* [])

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ { renCtx: preRenCtx, rule, sort, sigma, dzipper, mb_parent, renderTerm } = case rule /\ sort of
  True /\ _ -> [ Right [ HH.text "True" ] ]
  False /\ _ -> [ Right [ HH.text "False" ] ]
  Zero /\ _ -> [ Right [ HH.text "0" ] ]
  One /\ _ -> [ Right [ HH.text "1" ] ]
  And /\ _ -> [ Left (preRenCtx /\ 0), Right [ HH.text " && " ], Left (preRenCtx /\ 1) ]
  Plus /\ _ -> [ Left (preRenCtx /\ 0), Right [ HH.text " + " ], Left (preRenCtx /\ 1) ]
  Hole /\ _ -> [ pure [ Rendering.lbraceElem ], Right [ HH.text (pretty sort) ], pure [ Rendering.rbraceElem ] ]
  Newline /\ _ -> [ pure [ HH.div [ HP.classes [ HH.ClassName "newline-symbol" ] ] [ HH.text " ↪" ] ], pure (newlineIndentElem preRenCtx.indentationLevel), Left (preRenCtx /\ 0) ]
  -- BoolVar /\ (MInj (Grammar.SInj VarSort) % [ MInj (Grammar.DataLabel (DataString str)) ]) -> [ pure [HH.div [] []] ] -- TODO
  -- NumVar /\ _ -> [] -- TODO
  l -> unsafeCrashWith $ "arrangeDerivTermSubs didn't handle RuleLabel: " <> pretty l

newlineIndentElem :: forall t1 t2. Int -> Array (HH.HTML t1 t2)
--newlineIndentElem n = [Rendering.fillRightSpace, Rendering.newlineElem] <> Array.replicate n tabElem
newlineIndentElem n = [ Rendering.newlineElem ] <> Array.replicate n tabElem

tabElem = Rendering.makePuncElem "indent" "    "

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

forgetSorts :: DerivLabel -> Maybe DerivLabel
forgetSorts = pure

splitChange
  :: SortChange
  -> { downChange :: SortChange, upChange :: SortChange, cursorSort :: Sort }
splitChange c =
  { cursorSort: rEndpoint c
  , upChange: injectExprChange $ lEndpoint c
  , downChange: c
  }

makeEditFromPath = DefaultEdits.makeEditFromPath forgetSorts splitChange

editsAtCursor :: Sort -> Array Edit
editsAtCursor sort = Array.mapMaybe identity
  [ DefaultEdits.makeChangeEditFromTerm ((True %|- empty) % []) "True" sort
  , DefaultEdits.makeChangeEditFromTerm ((False %|- empty) % []) "False" sort
  , DefaultEdits.makeChangeEditFromTerm ((One %|- empty) % []) "One" sort
  , DefaultEdits.makeChangeEditFromTerm ((Zero %|- empty) % []) "Zero" sort
  , makeEditFromPath (newPathFromRule And 0) "And" sort
  , makeEditFromPath (newPathFromRule And 1) "And" sort
  , makeEditFromPath (newPathFromRule Plus 0) "Plus" sort
  , makeEditFromPath (newPathFromRule Plus 1) "Plus" sort
  ]

--------------------------------------------------------------------------------
-- Changes
--------------------------------------------------------------------------------

isValidCursorSort :: Sort -> Boolean
isValidCursorSort _ = true

isValidSelectionSorts :: { bottom :: Sort, top :: Sort } -> Boolean
isValidSelectionSorts { bottom, top } = bottom == top

keyAction :: String -> Sort -> Maybe Action
keyAction "Enter" cursorSort = DefaultEdits.makeActionFromPath true forgetSorts splitChange (fst (newPathFromRule Newline 0)) "newline" cursorSort
keyAction _key _cursorSort = Nothing

-- TODO: add edits for inserting new variables
extraQueryEdits :: Sort -> String -> Array Edit
extraQueryEdits _ _ = []

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "CustomLanguage" $ Grammar.defaultDerivTerm (BoolSort %|-* [])
  , editsAtCursor
  , arrangeDerivTermSubs
  , stepRules: empty
  , isValidCursorSort
  , isValidSelectionSorts
  , keyAction
  , extraQueryEdits
  , splitChange
  , editsAtHoleInterior: \_ -> []
  , onDelete: ChangeAlgebra.inject
  , generalizeDerivation: ChangeAlgebra.inject
  , specializeDerivation: \clipboard _cursor -> ChangeAlgebra.inject clipboard
  , forgetSorts
  , clipboardSort: \s -> s
  }
