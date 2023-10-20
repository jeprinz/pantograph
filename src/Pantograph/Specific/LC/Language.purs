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
import Hole (hole)
import Pantograph.Generic.Language as PL
import Pantograph.Generic.Rendering as PR
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, pretty, quotes2, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))
import Util (fromJust')

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
  prettyTreeNode el = case el of
    StringRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[] -> "<string>"
    VarRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[x] -> "#" <> x
    LamRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[x, b] -> Pretty.parens $ "λ" <> x <> "." <> b
    AppRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[f, a] -> Pretty.parens $ f <+> a
    LetRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[x, a, b] -> Pretty.parens $ "let" <+> x <+> "=" <+> a <+> "in" <+> b
    HoleRule -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[] -> "?"
    FormatRule IndentedNewline -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[a] -> "<indent>" <+> a
    FormatRule Newline -> assertValidTreeKids "(PrettyTreeNode EL).prettyTreeNode" el \[a] -> "<newline>" <+> a

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
  -- , getDefaultExpr: \sort -> case sort of
  --     Tree {node: PL.SortNode (StringValue _)} -> Nothing
  --     _ | Just ["str" /\ Tree {node: PL.SortNode (StringValue str)}] <- matchSort "(String $str)" sort -> Just $ term.string str
  --     _ | Just [] <- matchSort "(Term)" sort -> Nothing
  --     _ -> bug $ "invalid sort: " <> show sort
  , getDefaultExpr: case _ of
      Tree (PL.SortNode (StringValue _)) [] -> Nothing
      Tree (PL.SortNode StringSort) [Tree (PL.SortNode (StringValue str)) _] -> Just $ term.string str
      Tree (PL.SortNode TermSort) [] ->
        -- Just $ term.hole
        Just $ term.app term.hole term.hole
        -- Just $ term.app (term.app (term.lam "x" (term.app (term.lam "x" term.hole) term.hole)) (term.app (term.lam "x" term.hole) term.hole)) (term.app (term.app (term.lam "x" term.hole) term.hole) (term.app (term.lam "x" term.hole) term.hole))
        -- Just $ term.app (makeIndentedNewline (term.app term.hole term.hole)) (term.app (makeIndentedNewline (term.app term.hole (makeIndentedNewline term.hole))) term.hole)
        -- Just $ 
        --   term.app
        --     (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole term.hole))))))
        --     (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole term.hole))))))
        -- Just $ term.example 10
        -- Just $ term.example 6
        -- Just $ term.app (term.lam "x" (makeVar "x")) (term.lam "x" term.hole)
        -- Just $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.hole
        -- Just $ term.app (term.lam "x" (term.var "x")) (term.var "y")
        -- Just $ term.lam "x1" $ term.lam "x2" $ term.lam "x3" $ term.lam "x4" $ term.var "y"
        -- Just $ term.app (term.var "x1") (term.app (term.var "x2") (term.app (term.var "x3") (term.var "x4")))
        -- Just $ term.app term.hole (term.app term.hole (term.app term.hole term.hole))
      _ -> bug $ "invalid sort"
  , topSort: sort.term
  , getEdits: \sort _ -> case sort of
      Tree (PL.SortNode (StringValue _)) [] -> mempty
      Tree (PL.SortNode StringSort) [_] -> mempty
      Tree (PL.SortNode TermSort) [] ->
        [
          -- AppRule
          fromJust' "getEdits" $ NonEmptyArray.fromArray
            [ InsertEdit {outerChange: change.term, middle: PL.makeNonEmptyExprPath [tooth.app.apl term.hole], innerChange: change.term}
            , InsertEdit {outerChange: change.term, middle: PL.makeNonEmptyExprPath [tooth.app.arg term.hole], innerChange: change.term}
            ]
        ,
          -- FormatRule Newline
          NonEmptyArray.singleton $
            InsertEdit {outerChange: change.term, middle: PL.makeNonEmptyExprPath [tooth.format.newline term.hole], innerChange: change.term}
        ,
          -- FormatRule IndentedNewline
          NonEmptyArray.singleton $
            InsertEdit {outerChange: change.term, middle: PL.makeNonEmptyExprPath [tooth.format.indentedNewline term.hole], innerChange: change.term}
        ]
      _ -> bug $ "invalid sort: " <> show sort
  , validGyro: case _ of
      RootGyro _ -> true
      CursorGyro (Cursor {orientation: Outside}) -> true
      CursorGyro (Cursor {inside: Tree (PL.AnnExprNode {label: HoleRule}) _, orientation: Inside}) -> true
      SelectGyro (Select {outside, middle, inside, orientation}) -> true -- TODO: impl
      _ -> false
  , steppingRules: mempty
  , matchingSyntax
  }
  where
  -- matchSort = PL.matchSort matchingSyntax
  -- matchExpr = PL.matchExpr matchingSyntax
  matchingSyntax = PL.MatchingSyntax
    { parseExprLabel: case _ of
        "String" -> Just $ StringRule
        "Var" -> Just $ VarRule
        "Lam" -> Just $ LamRule
        "App" -> Just $ AppRule
        "Let" -> Just $ LetRule
        "Hole" -> Just $ HoleRule
        "Indent" -> Just $ FormatRule IndentedNewline
        "Newline" -> Just $ FormatRule Newline
        _ -> Nothing
    , parseSortNode: \input -> case input of
        "StringValue" | Just str <- String.stripPrefix (Pattern "StringValue:") input -> Just (StringValue str)
        "String" -> Just $ StringSort
        "Term" -> Just $ TermSort
        _ -> Nothing
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

term = {var, string, lam, app, let_, hole, indent, example}
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

tooth = {app, format}
  where
  app = 
    { apl: \arg -> PL.makeExprTooth AppRule [] 0 [arg]
    , arg: \apl -> PL.makeExprTooth AppRule [] 1 [apl]
    }
  
  format = 
    { newline: \kid -> PL.makeExprTooth (FormatRule Newline) [] 0 [kid] 
    , indentedNewline: \kid -> PL.makeExprTooth (FormatRule IndentedNewline) [] 0 [kid] 
    }