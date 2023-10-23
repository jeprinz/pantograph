module Pantograph.Specific.LC.Language where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (curry)
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language as PL
import Pantograph.Generic.Rendering as PR
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, pretty, quotes2, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))
import Util (fromJust, fromJust')

data SN = StringValue String | StringSort | TermSort
derive instance Generic SN _
derive instance Eq SN
derive instance Ord SN
instance Show SN where show = genericShow

instance TreeNode SN where
  kidsCount = case _ of
    StringValue _ -> 0
    StringSort -> 1
    TermSort -> 0

instance PrettyTreeNode SN where
  prettyTreeNode sn = case sn of
    (StringValue string) -> assertValidTreeKids "(PrettyTreeNode SN).prettyTreeNode" sn \[] -> string
    StringSort -> assertValidTreeKids "(PrettyTreeNode SN).prettyTreeNode" sn \[str] -> quotes2 str
    TermSort -> assertValidTreeKids "(PrettyTreeNode SN).prettyTreeNode" sn \[] -> "Term"

data EL
  = StringRule
  | VarRule
  | LamRule
  | AppRule
  | LetRule
  | HoleRule
  | FormatRule Format
derive instance Generic EL _
derive instance Eq EL
derive instance Ord EL
instance Show EL where show = genericShow

instance TreeNode EL where
  kidsCount = case _ of
    StringRule -> 0
    VarRule -> 1
    LamRule -> 2
    AppRule -> 2
    LetRule -> 3
    HoleRule -> 0
    FormatRule _ -> 1

instance PrettyTreeNode EL where
  prettyTreeNode el = 
    let ass = assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" in
    case el of
      StringRule -> ass el \[] -> "<string>"
      VarRule -> ass el \[x] -> "#" <> x
      LamRule -> ass el \[x, b] -> Pretty.parens $ "λ" <> x <> "." <> b
      AppRule -> ass el \[f, a] -> Pretty.parens $ f <+> a
      LetRule -> ass el \[x, a, b] -> Pretty.parens $ "let" <+> x <+> "=" <+> a <+> "in" <+> b
      HoleRule -> ass el \[] -> "?"
      FormatRule IndentedNewline -> ass el \[a] -> "<indent>" <+> a
      FormatRule Newline -> ass el \[a] -> "<newline>" <+> a

data Format = IndentedNewline | Newline
derive instance Generic Format _
derive instance Eq Format
derive instance Ord Format
instance Show Format where show = genericShow

type Expr = PL.Expr SN EL
type Language = PL.Language SN EL

language :: Language
language = PL.Language
  { name: "lambda calculus"
  , getSortingRule: case _ of
      StringRule -> PL.buildSortingRule ["str"] \[str] -> {kids: [], parent: ruleSort.string str}
      VarRule -> PL.buildSortingRule ["x"] \[x] -> {kids: [ruleSort.string x], parent: ruleSort.term}
      LamRule -> PL.buildSortingRule ["x"] \[x] -> {kids: [ruleSort.string x, ruleSort.term], parent: ruleSort.term}
      AppRule -> PL.buildSortingRule [] \[] -> {kids: [ruleSort.term, ruleSort.term], parent: ruleSort.term}
      LetRule -> PL.buildSortingRule ["x"] \[x] -> {kids: [ruleSort.string x, ruleSort.term, ruleSort.term], parent: ruleSort.term}
      HoleRule -> PL.buildSortingRule [] \[] -> {kids: [], parent: ruleSort.term}
      FormatRule _format -> PL.buildSortingRule [] \[] -> {kids: [ruleSort.term], parent: ruleSort.term}
  , getChangingRule: case _ of
      StringRule -> PL.buildChangingRule [] \[] -> []
      VarRule -> PL.buildChangingRule ["x"] \[x] -> [replaceChange (ruleSort.string x) (ruleSort.term)]
      LamRule -> PL.buildChangingRule ["x"] \[x] -> [replaceChange (ruleSort.string x) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
      AppRule -> PL.buildChangingRule [] \[] -> [replaceChange (ruleSort.term) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
      LetRule -> PL.buildChangingRule ["x"] \[x] -> [replaceChange (ruleSort.string x) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
      HoleRule -> PL.buildChangingRule [] \[] -> []
      FormatRule _format -> PL.buildChangingRule [] \[] -> [treeChange (PL.makeConstRuleSortNode TermSort) []]
  , getDefaultExpr: case _ of
      Tree (PL.SortNode (StringValue _)) [] -> Nothing
      Tree (PL.SortNode StringSort) [Tree (PL.SortNode (StringValue str)) _] -> Just $ term.string str
      Tree (PL.SortNode TermSort) [] ->
        -- Just $ term.hole
        Just $ term.app term.hole term.hole
        -- Just $ term.newline term.hole
      _ -> bug $ "invalid sort"
  , topSort: sort.term
  , getEdits: 
      let
        makeInsertEdits args = fromJust $ NonEmptyArray.fromArray $ args <#> \arg -> InsertEdit {outerChange: arg.outerChange, middle: PL.makeNonEmptyExprPath arg.middle, innerChange: arg.innerChange}
        termEdits = 
          [ -- AppRule
            makeInsertEdits 
              [ {outerChange: change.term, middle: [tooth.app.apl term.hole], innerChange: change.term}
              , {outerChange: change.term, middle: [tooth.app.arg term.hole], innerChange: change.term} ]
          , -- LamRule
            makeInsertEdits
              [ {outerChange: change.term, middle: [tooth.lam.bod (term.string "")], innerChange: change.term} ]
          , -- FormatRule Newline
            makeInsertEdits
              [ {outerChange: change.term, middle: [tooth.format.newline], innerChange: change.term} ]
          , -- FormatRule IndentedNewline
            makeInsertEdits
              [ {outerChange: change.term, middle: [tooth.format.indentedNewline], innerChange: change.term} ]
          ]
      in
      \sort _ -> case sort of
        Tree (PL.SortNode (StringValue _)) [] -> mempty
        Tree (PL.SortNode StringSort) [_] -> mempty
        Tree (PL.SortNode TermSort) [] -> termEdits
        _ -> bug $ "invalid sort: " <> show sort
  , validGyro: case _ of
      RootGyro _ -> true
      CursorGyro (Cursor {orientation: Outside}) -> true
      CursorGyro (Cursor {inside: Tree (PL.AnnExprNode {label: HoleRule}) _, orientation: Inside}) -> true
      SelectGyro (Select {outside, middle, inside, orientation}) -> true -- TODO: impl
      _ -> false
  , steppingRules: mempty
  }

-- shallow

sort = {stringValue, string, term}
  where
  stringValue str = PL.makeSort (StringValue str) []
  string str = PL.makeSort StringSort [stringValue str]
  term = PL.makeSort TermSort []

ruleSort = {stringValue, string, term}
  where
  stringValue str = PL.makeConstRuleSort (StringValue str) []
  string str = PL.makeConstRuleSort StringSort [str]
  term = PL.makeConstRuleSort TermSort []

term = {var, string, lam, app, let_, hole, indent, newline, example}
  where
  string str = PL.makeExpr StringRule ["str" /\ sort.stringValue str] []
  var str = PL.makeExpr VarRule ["s" /\ sort.string str] [string str]
  lam str b = PL.makeExpr LamRule ["s" /\ sort.string str] [string str, b]
  app f a = PL.makeExpr AppRule [] [f, a]
  let_ str a b = PL.makeExpr LetRule ["s" /\ sort.string str] [string str, a, newline b]
  hole = PL.makeExpr HoleRule [] []
  indent a = PL.makeExpr (FormatRule IndentedNewline) [] [a]
  newline a = PL.makeExpr (FormatRule Newline) [] [a]

  example 0 = hole
  example n = 
    let_ "x" (lam "y" (indent (example n'))) (app (var "x") (indent (example n')))
    -- let_ "x" app (lam "y" (example n')) (indent (example n'))
    where n' = n - 1

change = {term}
  where
  term = treeChange (PL.SortNode TermSort) []

tooth = {app, lam, format}
  where
  app = 
    { apl: \arg -> PL.makeExprTooth AppRule [] 0 [arg]
    , arg: \apl -> PL.makeExprTooth AppRule [] 1 [apl]
    }
  
  lam =
    { bod: \var -> PL.makeExprTooth LamRule [] 1 [var] }
  
  format = 
    { newline: PL.makeExprTooth (FormatRule Newline) [] 0 [] 
    , indentedNewline: PL.makeExprTooth (FormatRule IndentedNewline) [] 0 []
    }