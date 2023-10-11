module Pantograph.Specific.LC.Language where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
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
import Text.Pretty (class Pretty, pretty, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))

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
    StringRule -> assertValidTreeKids "prettyTreeNode" el \[] -> "<string>"
    VarRule -> assertValidTreeKids "prettyTreeNode" el \[x] -> x
    LamRule -> assertValidTreeKids "prettyTreeNode" el \[x, b] -> "λ" <> x <> "." <> b
    AppRule -> assertValidTreeKids "prettyTreeNode" el \[f, a] -> f <+> a
    LetRule -> assertValidTreeKids "prettyTreeNode" el \[x, a, b] -> "let" <+> x <+> "=" <+> a <+> "in" <+> b
    HoleRule -> assertValidTreeKids "prettyTreeNode" el \[] -> "?"
    FormatRule IndentedNewline -> assertValidTreeKids "prettyTreeNode" el \[a] -> "<indent>" <+> a
    FormatRule Newline -> assertValidTreeKids "prettyTreeNode" el \[a] -> "<newline>" <+> a

data Format = IndentedNewline | Newline
derive instance Generic Format _
derive instance Eq Format
derive instance Ord Format
instance Show Format where show = genericShow

data SN = StringValue String | StringSort | TermSort
derive instance Generic SN _
derive instance Eq SN
derive instance Ord SN
instance Show SN where show = genericShow

type RenderCtx :: Row Type
type RenderCtx = (indentLevel :: Int)

type RenderEnv :: Row Type
type RenderEnv = ()

type Expr = PL.Expr SN EL
type Language = PL.Language SN EL
type Renderer = PR.Renderer SN EL RenderCtx RenderEnv

language :: Language
language = PL.Language
  { name: "lambda calculus"
  , getSortingRule: case _ of
      StringRule -> do
        let str = PL.MakeRuleSortVar "str"
        PL.SortingRule
          { parameters: Set.fromFoldable [str]
          , kids: []
          , parent: PL.makeConstRuleSort StringSort [PL.makeVarRuleSort str] }
      VarRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.SortingRule
          { parameters: Set.fromFoldable [x]
          , kids: [PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x]]
          , parent: PL.makeConstRuleSort TermSort [] }
      LamRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.SortingRule
          { parameters: Set.fromFoldable [x]
          , kids: [PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x], PL.makeConstRuleSort TermSort []]
          , parent: PL.makeConstRuleSort TermSort [] }
      AppRule -> do
        PL.SortingRule
          { parameters: Set.fromFoldable []
          , kids: [PL.makeConstRuleSort TermSort [], PL.makeConstRuleSort TermSort []]
          , parent: PL.makeConstRuleSort TermSort [] }
      LetRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.SortingRule
          { parameters: Set.fromFoldable [x]
          , kids: [PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x], PL.makeConstRuleSort TermSort [], PL.makeConstRuleSort TermSort []]
          , parent: PL.makeConstRuleSort TermSort [] }
      HoleRule -> do
        PL.SortingRule
          { parameters: Set.fromFoldable []
          , kids: []
          , parent: PL.makeConstRuleSort TermSort [] }
      FormatRule _format -> do
        PL.SortingRule
          { parameters: Set.fromFoldable []
          , kids: [PL.makeConstRuleSort TermSort []]
          , parent: PL.makeConstRuleSort TermSort [] }
  , getChangingRule: case _ of
      StringRule -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: []  }
      VarRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.ChangingRule
          { parameters: Set.fromFoldable [x]
          , kids: [Replace (PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x]) (PL.makeConstRuleSort TermSort [])] }
      LamRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.ChangingRule
          { parameters: Set.fromFoldable [x]
          , kids: [Replace (PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x]) (PL.makeConstRuleSort TermSort []), Replace (PL.makeConstRuleSort TermSort []) (PL.makeConstRuleSort TermSort [])]  }
      AppRule -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: [Replace (PL.makeConstRuleSort TermSort []) (PL.makeConstRuleSort TermSort []), Replace (PL.makeConstRuleSort TermSort []) (PL.makeConstRuleSort TermSort [])] }
      LetRule -> do
        let x = PL.MakeRuleSortVar "x"
        PL.ChangingRule
          { parameters: Set.fromFoldable [x]
          , kids: [Replace (PL.makeConstRuleSort StringSort [PL.makeVarRuleSort x]) (PL.makeConstRuleSort TermSort []), Replace (PL.makeConstRuleSort TermSort []) (PL.makeConstRuleSort TermSort []), Replace (PL.makeConstRuleSort TermSort []) (PL.makeConstRuleSort TermSort [])]  }
      HoleRule -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: []  }
      FormatRule _format -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: [Reflect (PL.makeConstRuleSortNode TermSort) []] }
  , defaultExpr: case _ of
      Tree {node: PL.SortNode (StringValue _)} -> Nothing
      Tree {node: PL.SortNode StringSort} -> Just $ term.string ""
      Tree {node: PL.SortNode TermSort} ->
        -- Just $ term.hole
        -- Just $ term.app term.hole term.hole
        -- Just $ term.app (term.app (term.lam "x" (term.app (term.lam "x" term.hole) term.hole)) (term.app (term.lam "x" term.hole) term.hole)) (term.app (term.app (term.lam "x" term.hole) term.hole) (term.app (term.lam "x" term.hole) term.hole))
        -- Just $ term.app (makeIndentedNewline (term.app term.hole term.hole)) (term.app (makeIndentedNewline (term.app term.hole (makeIndentedNewline term.hole))) term.hole)
        -- Just $ 
        --   term.app
        --     (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole term.hole))))))
        --     (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole (makeIndentedNewline (term.app term.hole term.hole))))))
        -- Just $ term.example 10
        -- Just $ term.example 4
        -- Just $ term.app (term.lam "x" (makeVar "x")) (term.lam "x" term.hole)
        Just $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.let_ "x" term.hole $ term.hole
  , topSort: sort.term
  }

-- shallow

sort = {stringValue, string, term}
  where
  stringValue str = PL.makeSort (StringValue str) []
  string str = PL.makeSort StringSort [stringValue str]
  term = PL.makeSort TermSort []

term = {var, string, lam, let_, hole, indent, example}
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
