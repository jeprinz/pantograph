module Pantograph.Specific.LC where

import Data.Tree
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.SearchableArray (SearchableArray)
import Data.SearchableArray as SearchableArray
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.StringQuery as StringQuery
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language as PL
import Pantograph.Generic.Rendering as PR
import Pantograph.Generic.Rendering.Html as PH
import Pantograph.Library.Language.Step as LibStep
import Record as R
import Text.Pretty (quotes2, (<+>))
import Text.Pretty as Pretty
import Todo (todo)
import Type.Proxy (Proxy(..))
import Util (fromJust)

editorComponent :: PR.EditorComponent SN EL CTX ENV
editorComponent = PR.editorComponent

editorInput :: PR.EditorInput SN EL CTX ENV
editorInput = PR.EditorInput {}

-- SN

data SN = StringSort | TermSort
derive instance Generic SN _
derive instance Eq SN
derive instance Ord SN
instance Show SN where show = genericShow

instance TreeNode SN where
  kidsCount = case _ of
    StringSort -> 0
    TermSort -> 0

instance PrettyTreeNode SN where
  prettyTreeNode sn = case sn of
    StringSort -> assertValidTreeKids "(PrettyTreeNode SN).prettyTreeNode" sn \[str] -> quotes2 str
    TermSort -> assertValidTreeKids "(PrettyTreeNode SN).prettyTreeNode" sn \[] -> "Term"

-- EL

data EL
  = StringRule String
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
    StringRule _ -> 0
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
      StringRule str -> ass el \[] -> str
      VarRule -> ass el \[x] -> "#" <> x
      LamRule -> ass el \[x, b] -> Pretty.parens $ "λ" <> x <> "." <> b
      AppRule -> ass el \[f, a] -> Pretty.parens $ f <+> a
      LetRule -> ass el \[x, a, b] -> Pretty.parens $ "let" <+> x <+> "=" <+> a <+> "in" <+> b
      HoleRule -> ass el \[] -> "?"
      FormatRule Indent -> ass el \[a] -> "<indent>" <+> a
      FormatRule Newline -> ass el \[a] -> "<newline>" <+> a

data Format = Indent | Newline
derive instance Generic Format _
derive instance Eq Format
derive instance Ord Format
instance Show Format where show = genericShow

-- Language

type Expr = PL.Expr SN EL
type ExprTooth = PL.ExprTooth SN EL
type SortChange = PL.SortChange SN
type RuleSort = PL.RuleSort SN
type Sort = PL.Sort SN
type Edit = PL.Edit SN EL
type Edits = PL.Edits SN EL
type SteppingRule = PL.SteppingRule SN EL
type ChangingRule = PL.ChangingRule SN
type SortingRule = PL.SortingRule SN

instance PL.Language SN EL where
  getSortingRule l = getSortingRule l
  getChangingRule l = getChangingRule l
  getDefaultExpr s = getDefaultExpr s
  topSort = topSort
  getEdits s = getEdits s
  validGyro g = validGyro g
  steppingRules = steppingRules
  specialEdits =
    { deleteCursor: \sr -> PL.getDefaultExpr sr <#> \inside -> PL.Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Just inside}
    , deleteSelect: \_ch -> Just $ PL.Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Nothing}
    , enter: \_ -> Just $ PL.Edit {outerChange: Nothing, middle: Just (PL.makeExprNonEmptyPath [tooth.format.newline]), innerChange: Nothing, inside: Nothing} 
    , tab: \_ -> Just $ PL.Edit {outerChange: Nothing, middle: Just (PL.makeExprNonEmptyPath [tooth.format.indent]), innerChange: Nothing, inside: Nothing} }

getSortingRule = case _ of
  StringRule _ -> PL.buildSortingRuleFromStrings [] \[] -> [] /\ ruleSort.string
  VarRule -> PL.buildSortingRuleFromStrings [] \[] -> [ruleSort.string] /\ ruleSort.term
  LamRule -> PL.buildSortingRuleFromStrings [] \[] -> [ruleSort.string, ruleSort.term] /\ ruleSort.term
  AppRule -> PL.buildSortingRuleFromStrings [] \[] -> [ruleSort.term, ruleSort.term] /\ ruleSort.term
  LetRule -> PL.buildSortingRuleFromStrings [] \[] -> [ruleSort.string, ruleSort.term, ruleSort.term] /\ ruleSort.term
  HoleRule -> PL.buildSortingRuleFromStrings [] \[] -> [] /\ ruleSort.term
  FormatRule _format -> PL.buildSortingRuleFromStrings [] \[] -> [ruleSort.term] /\ ruleSort.term

getChangingRule = case _ of
  StringRule _ -> PL.buildChangingRule [] \[] -> []
  VarRule -> PL.buildChangingRule [] \[] -> [replaceChange (ruleSort.string) (ruleSort.term)]
  LamRule -> PL.buildChangingRule [] \[] -> [replaceChange (ruleSort.string) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
  AppRule -> PL.buildChangingRule [] \[] -> [replaceChange (ruleSort.term) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
  LetRule -> PL.buildChangingRule [] \[] -> [replaceChange (ruleSort.string) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term), replaceChange (ruleSort.term) (ruleSort.term)]
  HoleRule -> PL.buildChangingRule [] \[] -> []
  FormatRule _format -> PL.buildChangingRule [] \[] -> [injectChange (PL.makeInjectRuleSortNode TermSort) []]

getDefaultExpr = case _ of
  Tree (PL.SortNode StringSort) [] -> Just $ term.string ""
  Tree (PL.SortNode TermSort) [] -> Just $ term.hole
  _ -> bug $ "invalid sort"

topSort = sort.term

getEdits :: Sort -> Orientation -> Edits
getEdits =
  let
    makeInsertEdits :: Array {outerChange :: PL.SortChange SN, middle :: Array (PL.ExprTooth SN EL), innerChange :: PL.SortChange SN} -> NonEmptyArray (PL.Edit SN EL)
    makeInsertEdits edits = fromJust $ NonEmptyArray.fromArray $ edits <#> 
      \{outerChange, middle, innerChange} -> PL.Edit {outerChange: Just outerChange, middle: Just $ PL.makeExprNonEmptyPath middle, innerChange: Just innerChange, inside: Nothing}
    
    maxDistance = Fuzzy.Distance 1 0 0 0 0 0

    getEdits' (Tree (PL.SortNode StringSort) []) _ = PL.Edits $ StringQuery.fuzzy 
      { toString: fst, maxPenalty: maxDistance
      , getItems: \str -> 
          [ str /\ 
            NonEmptyArray.singleton 
              (PL.Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Just (term.string str)})]
      }
    getEdits' (Tree (PL.SortNode TermSort) []) Outside = PL.Edits $ StringQuery.fuzzy 
      { maxPenalty: maxDistance, toString: fst
      , getItems: const
          [ 
            -- AppRule
            "app" /\
            makeInsertEdits 
              [ {outerChange: change.term, middle: [tooth.app.apl term.hole], innerChange: change.term}
              , {outerChange: change.term, middle: [tooth.app.arg term.hole], innerChange: change.term} ]
          , -- LamRule
            "lam" /\
            makeInsertEdits
              [ {outerChange: change.term, middle: [tooth.lam.bod (term.string "")], innerChange: change.term} ]
          ]
      }
    getEdits' (Tree (PL.SortNode TermSort) []) Inside = PL.Edits $ StringQuery.fuzzy 
      { maxPenalty: maxDistance, toString: fst
      , getItems: const
          [
            -- VarRule
            "var" /\ NonEmptyArray.singleton
            (PL.Edit {outerChange: Nothing, middle: Just $ PL.makeExprNonEmptyPath [tooth.var.str], innerChange: Nothing, inside: Just (term.string "")})
          ]
      }
    getEdits' sr _ = bug $ "invalid sort: " <> show sr
  in
  getEdits'

validGyro = case _ of
  RootGyro _ -> true
  CursorGyro (Cursor {orientation: Outside}) -> true
  CursorGyro (Cursor {inside: Tree (PL.ExprNode HoleRule _ _) _, orientation: Inside}) -> true
  SelectGyro (Select {middle}) -> PL.getExprNonEmptyPathOuterSort middle == PL.getExprNonEmptyPathInnerSort middle
  _ -> false

steppingRules :: Array SteppingRule
steppingRules =
  [ LibStep.eraseBoundary Nothing \_ -> true
  ]

deleteCursorEdit :: Sort -> Maybe Edit
deleteCursorEdit sr = PL.getDefaultExpr sr <#> \inside -> PL.Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Just inside}

deleteSelectEdit :: SortChange -> Maybe Edit
deleteSelectEdit _ = Just $ PL.Edit {outerChange: Nothing, middle: Nothing, innerChange: Nothing, inside: Nothing}

-- Rendering

type CTX = (indentLevel :: Int)

type ENV = () :: Row Type

instance PR.Rendering SN EL CTX ENV where
  topCtx = Proxy /\ {indentLevel: 0}

  topEnv = Proxy /\ {}

  arrangeExpr =
    let punc str = PR.HtmlArrangeKid [HH.span_ [HH.text str]] in
    let indent i = PR.HtmlArrangeKid (Array.replicate i (PH.whitespace "⇥ ")) in
    let newline i = PR.HtmlArrangeKid ([PH.whitespace " ↪", HH.br_] <> Array.replicate i (PH.whitespace "⇥ ")) in
    \node@(PL.ExprNode label _ _) ->
      let ass = assertValidTreeKids "arrangeExpr" (PL.shrinkAnnExprNode node :: PL.ExprNode SN EL) in
      case label of
        StringRule str -> ass \[] -> do
          let Tree (PL.SortNode StringSort) [] = PL.getExprNodeSort node
          pure [PR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "string"]] [HH.text (if String.null str then "~" else str)]]]
        VarRule -> ass \[mx] -> do
          x_ /\ _x <- mx
          pure [punc "#", PR.ExprKidArrangeKid x_]
        LamRule -> ass \[mx, mb] -> do
          x_ /\ _x <- mx 
          b_ /\ _b <- mb 
          pure [punc "(", punc "λ", PR.ExprKidArrangeKid x_, punc ".", PR.ExprKidArrangeKid b_, punc ")"]
        AppRule -> ass \[mf, ma] -> do
          f_ /\ _f <- mf 
          a_ /\ _a <- ma 
          pure [punc "(", PR.ExprKidArrangeKid f_, punc " ", PR.ExprKidArrangeKid a_, punc ")"]
        LetRule -> ass \[mx, ma, mb] -> do
          ctx <- ask
          x_ /\ _x <- mx
          a_ /\ _a <- ma
          b_ /\ _b <- mb
          pure [punc "(", punc "let", punc " ", PR.ExprKidArrangeKid x_, punc " ", punc "=", punc " ", PR.ExprKidArrangeKid a_, punc " ", punc "in", punc " ", PR.ExprKidArrangeKid b_, punc ")"]
        HoleRule -> ass \[] -> do
          holeIndex <- State.gets _.holeCount
          State.modify_ _ {holeCount = holeIndex + 1}
          pure [PR.HtmlArrangeKid [PH.hole {index: Just $ HH.text (show holeIndex), ann: Nothing}]]
        FormatRule Indent -> ass \[ma] -> do
          ctx <- ask
          a_ /\ _a <- local (R.modify (Proxy :: Proxy "indentLevel") (1 + _)) ma
          pure [indent 1, PR.ExprKidArrangeKid a_]
        FormatRule Newline -> ass \[ma] -> do
          ctx <- ask
          a_ /\ _a <- ma
          pure [newline ctx.indentLevel, PR.ExprKidArrangeKid a_]

  getBeginsLine = case _ of
    Cursor {outside: Path (Cons (Tooth (PL.ExprNode (FormatRule Newline) _ _) _) _), orientation: Outside} -> true
    _ -> false

  getInitialQuery = case _ of
    Cursor {inside: Tree (PL.ExprNode (StringRule str) _ _) []} -> str
    _ -> ""
  
-- shallow

sort = {string, term}
  where
  string str = PL.makeSort StringSort [] :: Sort
  term = PL.makeSort TermSort [] :: Sort

ruleSort = {string, term}
  where
  string = PL.makeInjectRuleSort StringSort [] :: RuleSort
  term = PL.makeInjectRuleSort TermSort [] :: RuleSort

term = {var, string, lam, app, let_, hole, indent, newline, example}
  where
  string str = PL.makeExpr (StringRule str) [] [] :: Expr
  var str = PL.makeExpr VarRule [] [string str] :: Expr
  lam str b = PL.makeExpr LamRule [] [string str, b] :: Expr
  app f a = PL.makeExpr AppRule [] [f, a] :: Expr
  let_ str a b = PL.makeExpr LetRule [] [string str, a, newline b] :: Expr
  hole = PL.makeExpr HoleRule [] [] :: Expr
  indent a = PL.makeExpr (FormatRule Indent) [] [a] :: Expr
  newline a = PL.makeExpr (FormatRule Newline) [] [a] :: Expr

  example :: Int -> Expr
  example 0 = hole
  example n = 
    let_ "x" (lam "y" (indent (example n'))) (app (var "x") (indent (example n')))
    -- let_ "x" app (lam "y" (example n')) (indent (example n'))
    where n' = n - 1

change = {term}
  where
  term = injectChange (PL.SortNode TermSort) [] :: SortChange

tooth = {app, lam, var, format}
  where
  app = 
    { apl: (\arg -> PL.makeExprTooth AppRule [] 0 [arg]) :: Expr -> ExprTooth
    , arg: (\apl -> PL.makeExprTooth AppRule [] 1 [apl]) :: Expr -> ExprTooth
    }
  
  lam =
    { bod: (\var -> PL.makeExprTooth LamRule [] 1 [var]) :: Expr -> ExprTooth }

  var = 
    { str: PL.makeExprTooth VarRule [] 0 [] :: ExprTooth }
  
  format = 
    { newline: PL.makeExprTooth (FormatRule Newline) [] 0 [] :: ExprTooth
    , indent: PL.makeExprTooth (FormatRule Indent) [] 0 [] :: ExprTooth
    }