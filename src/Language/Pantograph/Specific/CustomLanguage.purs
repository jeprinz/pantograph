module Language.Pantograph.Specific.CustomLanguage (PreSortLabel(..), RuleLabel(..), editorSpec) where

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
import Data.Expr (class IsExprLabel, Meta(..), matchExprImpl, (%), (%$), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Language.Pantograph.Generic.ChangeAlgebra (lEndpoint)
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
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty)
import Util as Util

--------------------------------------------------------------------------------
{-
This file is a template for making a custom language with pantograph.
An editor for the language in this file can be compiled with `npm run build-custom-language-standalone`.
This will output into dist/custom-language-standalone. Open index.html in your browser to view it.

This template defines a simple calculator language, with the following grammar:

Bool := <variable> | Bool && Bool | true | false | Num == Num
Num := <variable> | Num + Num

you can write expressions like "True && ((x + y) == z)"

We call `Bool` and `Num` sorts. We think of expressions as typing derivations, and so refer to things like `+` as a derivation rule.

To modify this template and tell pantograph how to work with your own language, you need to define:
- the sorts (in this case, `Bool` and `Num`)
- the derivation rules (e.g., `&&`, `true`, `==`)
- which sorts are inputs and output by which rules
- how to display each rule
- which edits are available to the user


Due to a combination of bad design on our part and purescript not having modules, there is some verbose boilerplate in this file.
Instead of reading everything, go to the comments that say "LOOK HERE".
-}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

-- LOOK HERE: define the sorts here.
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

-- LOOK HERE: for debugging purposes, define how each sort is printed as a string
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

-- LOOK HERE: define the derivation rules. These are the different constructions in your grammar.
data RuleLabel
  = True -- true
  | False -- false
  | One -- 1
  | Zero -- 0
  | And -- Bool && Bool
  | Plus -- Num + Num
  | Hole -- ?
  | Equals -- Num == Num
  | BoolVar -- variable of sort Bool
  | NumVar -- variable of sort Num
  | Newline -- newline construction for formatting (can have any sort)

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

-- LOOK HERE: for each derivation rule, define which sorts it inputs and outputs.
-- for example, the `==` operator inputs two numbers and outputs a boolean, so its
-- rule has two NumSorts in the list of the left, and a BoolSort on the right.
-- to make your own, follow the examples here of our insane notation
-- (the reason for this notation is that it can support typing rules that depend on metavariables,
-- which is used for typed editing as in the main pantograph language)
language :: Language
language = TotalMap.makeTotalMap case _ of
  -- true : Bool
  True -> Grammar.makeRule [] \[] ->
    [] /\ (BoolSort %|-* [])
  -- false : Bool
  False -> Grammar.makeRule [] \[] ->
    [] /\ (BoolSort %|-* [])
  -- 1 : Num
  One -> Grammar.makeRule [] \[] ->
    [] /\ (NumSort %|-* [])
  -- 0 : Num
  Zero -> Grammar.makeRule [] \[] ->
    [] /\ (NumSort %|-* [])
  -- Bool && Bool : Bool
  And -> Grammar.makeRule [] \[] ->
    [ BoolSort %|-* [], BoolSort %|-* [] ] /\ (BoolSort %|-* [])
  -- Num + Num : Num
  Plus -> Grammar.makeRule [] \[] ->
    [ NumSort %|-* [], NumSort %|-* [] ] /\ (NumSort %|-* [])
  -- Num == Num : Bool
  Equals -> Grammar.makeRule [] \[] ->
    [ NumSort %|-* [], NumSort %|-* [] ] /\ (BoolSort %|-* [])
  -- <variable> : Bool
  BoolVar -> Grammar.makeRule [ "x" ] \[ x ] ->
    [ TypeOfLabel SortString %* [ x ] ] /\ (BoolSort %|-* [])
  -- <variable> : Num
  NumVar -> Grammar.makeRule [ "x" ] \[ x ] ->
    [ TypeOfLabel SortString %* [ x ] ] /\ (NumSort %|-* [])
  --
  -- you probably want these last two for any language:
  --
  -- ? : any sort
  Hole -> Grammar.makeRule [ "sort" ] \[ sort ] ->
    [] /\ sort
  -- The "newline" construct can wrap around something of any sort, and then the newline-wrapped construction has the same sort
  -- For example, the program
  --
  -- ```
  -- 1 +
  --   1
  -- ```
  --
  -- is encoded as the AST
  --
  -- ```
  -- Plus One (Newline One)
  -- ```
  --
  -- Note I'm just using pseudocode to write out these examples, since the actual Pantograph encodings are much more verbose.
  Newline -> Grammar.makeRule [ "sort" ] \[ sort ] ->
    [ sort ] /\ sort

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- LOOK HERE: this function defines how to display each derivation rule.
-- it uses the Halogen purescript library, which is a DSL for HTML.
-- But you can probably get away with not learning Halogen and just copying from these examples.
-- For each rule, it outputs a list of elements, which either `Right [some halogen here]` or `Left (preRenCtx /\ index)`
-- the Left elements correspond to children of the expression.
-- For example, the `+` operator has three elements in it's list: the first is the left child expression, the second is text for
-- "+", and the third is the right child expression.
arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ { renCtx: preRenCtx, rule, sort, sigma, dzipper, mb_parent, renderTerm } = case rule /\ sort of
  True /\ _ -> [ Right [ HH.text "True" ] ]
  False /\ _ -> [ Right [ HH.text "False" ] ]
  Zero /\ _ -> [ Right [ HH.text "0" ] ]
  One /\ _ -> [ Right [ HH.text "1" ] ]
  And /\ _ -> [ Left (preRenCtx /\ 0), Right [ HH.text " && " ], Left (preRenCtx /\ 1) ]
  Plus /\ _ -> [ Right [ HH.text "(" ], Left (preRenCtx /\ 0), Right [ HH.text " + " ], Left (preRenCtx /\ 1), Right [ HH.text ")" ] ]
  Equals /\ _ -> [ Left (preRenCtx /\ 0), Right [ HH.text " == " ], Left (preRenCtx /\ 1) ]
  Hole /\ _ -> [ pure [ Rendering.lbraceElem ], Right [ HH.text (pretty sort) ], pure [ Rendering.rbraceElem ] ]
  Newline /\ _ -> [ pure [ HH.div [ HP.classes [ HH.ClassName "newline-symbol" ] ] [ HH.text " ↪" ] ], pure (newlineIndentElem preRenCtx.indentationLevel), Left (preRenCtx /\ 0) ]
  BoolVar /\ (MInj (Grammar.SInj BoolSort) % []) -> [ Left (preRenCtx /\ 0) ]
  NumVar /\ (MInj (Grammar.SInj NumSort) % []) -> [ Left (preRenCtx /\ 0) ]
  l -> unsafeCrashWith $ "arrangeDerivTermSubs didn't handle RuleLabel: " <> show l

newlineIndentElem :: forall t1 t2. Int -> Array (HH.HTML t1 t2)
--newlineIndentElem n = [Rendering.fillRightSpace, Rendering.newlineElem] <> Array.replicate n tabElem
newlineIndentElem n = [ Rendering.newlineElem ] <> Array.replicate n tabElem

tabElem :: forall w i. HTML w i
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
  { cursorSort: lEndpoint c
  , upChange: c
  , downChange: ChangeAlgebra.inject (lEndpoint c)
  }

makeEditFromPath = DefaultEdits.makeEditFromPath forgetSorts splitChange

-- LOOK HERE: this defines which edits are possible for the user to make.
-- in Pantograph, there are two kinds of edits: filling terms in a hole, and wrapping one-hole contexts around the cursor.
-- for each constructor that you want to be able to be filled in a hole, copy the examples for "makeChangeEditFromTerm"
-- for each constructor that you want to be able to be wrapped around the cursor, copy the example for "makeEditFromPath"
editsAtCursor :: Sort -> Array Edit
editsAtCursor sort = Array.mapMaybe identity
  [ DefaultEdits.makeChangeEditFromTerm ((True %|- empty) % []) "True" sort
  , DefaultEdits.makeChangeEditFromTerm ((False %|- empty) % []) "False" sort
  , DefaultEdits.makeChangeEditFromTerm ((One %|- empty) % []) "One" sort
  , DefaultEdits.makeChangeEditFromTerm ((Zero %|- empty) % []) "Zero" sort
  , DefaultEdits.makeChangeEditFromTerm ((True %|- empty) % []) "True" sort
  , DefaultEdits.makeChangeEditFromTerm ((Equals %|- empty) % [ (Hole %|- empty) % [], (Hole %|- empty) % [] ]) "==" sort
  , makeEditFromPath (newPathFromRule And 0) "&&" sort
  , makeEditFromPath (newPathFromRule And 1) "&&" sort
  , makeEditFromPath (newPathFromRule Plus 0) "+" sort
  , makeEditFromPath (newPathFromRule Plus 1) "+" sort
  ]

--------------------------------------------------------------------------------
-- Changes
--------------------------------------------------------------------------------

isValidCursorSort :: Sort -> Boolean
isValidCursorSort (MInj (TypeOfLabel _) % _) = false
isValidCursorSort (MInj (DataLabel _) % _) = false
isValidCursorSort _ = true

isValidSelectionSorts :: { bottom :: Sort, top :: Sort } -> Boolean
isValidSelectionSorts { bottom, top } = bottom == top

keyAction :: String -> Sort -> Maybe Action
keyAction "Enter" cursorSort = DefaultEdits.makeActionFromPath true forgetSorts splitChange (fst (newPathFromRule Newline 0)) "newline" cursorSort
keyAction _key _cursorSort = Nothing

extraQueryEdits :: Sort -> String -> Array Edit
extraQueryEdits cursorSort query | isJust $ matchExprImpl cursorSort (sor BoolSort %$ []) = fromMaybe $ makeVarEdit cursorSort query BoolVar
extraQueryEdits cursorSort query | isJust $ matchExprImpl cursorSort (sor NumSort %$ []) = fromMaybe $ makeVarEdit cursorSort query NumVar
extraQueryEdits _ _ = []

makeVarEdit :: Sort -> String -> RuleLabel -> Maybe Edit
makeVarEdit sort name rl =
  let
    varSort = MInj (Grammar.DataLabel (DataString name)) % []
  in
    DefaultEdits.makeSubEditFromTerm
      ( makeLabel rl [ "x" /\ varSort ] %
          [ Grammar.defaultDerivTerm (Grammar.TypeOfLabel SortString %* [ varSort ])
              # Util.fromJust' "defaultDerivTerm always works for a `TypeOfLabel SortString`"
          ]
      )
      name
      sort

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
