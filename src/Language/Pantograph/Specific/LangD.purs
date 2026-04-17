module Language.Pantograph.Specific.LangD where

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
import Data.Expr (class IsExprLabel, Meta(..), MetaVar(..), injectExprChange, matchExprImpl, (%), (%$), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable as Unfoldable
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Pantograph.Generic.ChangeAlgebra (lEndpoint, rEndpoint)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar (IsHoleRule(..), SortData(..), SortLabel(..), SortType(..), makeLabel, sor, (%|-), (%|-*))
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Language.Pantograph.Specific.Square (LangD(..))
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty)
import Util as Util

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

data PreSortLabel
  = SortSL -- String -> Sort
  | NameSL -- Sort

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
  prettyExprF'_unsafe (SortSL /\ [label]) = "Sort(" <> label <> ")"
  prettyExprF'_unsafe (NameSL /\ _) = "Name"

  expectedKidsCount SortSL = 1
  expectedKidsCount NameSL = 0

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

data RuleLabel
  = ConRL
  --
  | NameRL -- String -> Name
  --
  | HoleRL
  | NewlineRL

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
  prettyExprF'_unsafe_RuleLabel _ = unsafeCrashWith "TODO"

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of 
    HoleRL -> Yes false 
    _ -> No

  defaultDerivTerm' sort = pure $ Grammar.makeLabel HoleRL [ "sort" /\ sort ] % []

-- language :: Language
-- language = TotalMap.makeTotalMap case _ of 
--   ConRL -> Grammar.makeRule ["sort"] \[sort] ->
--     [  ]
--     /\ --------------------------------
--     ( sort )

--   HoleRL -> Grammar.makeRule ["sort"] \[sort] ->
--     [] 
--     /\ --------------------------------
--     ( sort )

--   NewlineRL -> Grammar.makeRule ["sort"] \[sort] ->
--     [ sort ] 
--     /\ --------------------------------
--     ( sort )

--   NameRL -> Grammar.makeRule ["name"] \[name] ->
--     [ TypeOfLabel SortString %* [name] ]
--     /\ --------------------------------
--     ( NameSL %|-* [] )

language :: Language
language = TotalMap.makeTotalMapFromFoldable_unsafe $
  [
    Tuple HoleRL $ Grammar.makeRule ["sort"] \[sort] -> Tuple
      [] 
      --------------------------------
      ( sort )

  , Tuple NewlineRL $ Grammar.makeRule ["sort"] \[sort] -> Tuple
      [ sort ] 
      --------------------------------
      ( sort )

  , Tuple NameRL $ Grammar.makeRule ["name"] \[name] -> Tuple
      [ TypeOfLabel SortString %* [name] ]
      --------------------------------
      ( NameSL %|-* [] )
  ]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

keyword :: forall w i . String -> HH.HTML w i
keyword label = HH.span [HP.classes [HH.ClassName "keyword"]] [HH.text label]

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ { renCtx, rule, sort, sigma, dzipper, mb_parent, renderTerm } = case rule /\ sort of
  ConRL /\ _ -> [ Right [HH.text "Con"] ]
  HoleRL /\ _ -> [ Right [Rendering.lbraceElem], Right [HH.text (pretty sort)], Right [Rendering.rbraceElem] ]
  NameRL /\ _ -> [ Right [Rendering.lbracketElem], Left (renCtx /\ 0), Right [Rendering.rbracketElem] ]
  NewlineRL /\ _ -> 
    [ Right [ HH.div [ HP.classes [ HH.ClassName "newline-symbol" ] ] [ HH.text " ↪" ] ]
    , Right (newlineIndentElem renCtx.indentationLevel), Left (renCtx /\ 0) 
    ]

newlineIndentElem :: forall t1 t2. Int -> Array (HH.HTML t1 t2)
--newlineIndentElem n = [Rendering.fillRightSpace, Rendering.newlineElem] <> Array.replicate n tabElem
newlineIndentElem n = [ Rendering.newlineElem ] <> Array.replicate n tabElem

tabElem :: forall w i. HH.HTML w i
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
  []

holeDerivTerm :: Sort -> DerivTerm
holeDerivTerm sort = (HoleRL %|- (Map.fromFoldable [RuleMetaVar "sort" /\ sort])) % []

emptyNameDerivTerm :: DerivTerm 
emptyNameDerivTerm = (NameRL %|- (Map.fromFoldable [RuleMetaVar "name" /\ (DataLabel (DataString "")) %* []]) % [emptyStringDerivTerm])

emptyStringDerivTerm :: DerivTerm
emptyStringDerivTerm = Grammar.DerivLiteral (DataString "") % []

--------------------------------------------------------------------------------
-- Changes
--------------------------------------------------------------------------------

isValidCursorSort :: Sort -> Boolean
isValidCursorSort (MInj (SInj SortSL) % _) = true
isValidCursorSort (MInj (SInj NameSL) % _) = true
isValidCursorSort (MInj (TypeOfLabel _) % _) = false
isValidCursorSort _ = false

isValidSelectionSorts :: { bottom :: Sort, top :: Sort } -> Boolean
isValidSelectionSorts { bottom, top } = bottom == top

keyAction :: String -> Sort -> Maybe Action
keyAction "Enter" cursorSort = DefaultEdits.makeActionFromPath true forgetSorts splitChange (fst (newPathFromRule NewlineRL 0)) "newline" cursorSort
keyAction _key _cursorSort = Nothing

extraQueryEdits :: Sort -> String -> Array Edit
extraQueryEdits cursorSort query | isJust $ matchExprImpl cursorSort (sor NameSL %$ []) = Unfoldable.fromMaybe $ makeNameEdit cursorSort query NameRL
extraQueryEdits _ _ = []

makeNameEdit :: Sort -> String -> RuleLabel -> Maybe Edit
makeNameEdit sort name rl =
  let
    varSort = MInj (Grammar.DataLabel (DataString name)) % []
  in
    DefaultEdits.makeSubEditFromTerm
      ( makeLabel rl [ "name" /\ varSort ] %
          [ Grammar.defaultDerivTerm (Grammar.TypeOfLabel SortString %* [ varSort ])
              # Util.fromJust' "defaultDerivTerm always works for a `TypeOfLabel SortString`"
          ]
      )
      name
      sort


--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: LangD -> EditorSpec PreSortLabel RuleLabel
editorSpec lang@(LangD langName langRules) =
  { dterm: assertI $ just "LangD" $ Grammar.defaultDerivTerm (SortSL %|-* [])
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
