module Pantograph.Specific.LC where

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
import Pantograph.Generic.Language.Common (unAnnExprNode)
import Pantograph.Generic.Rendering as PR
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, pretty, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))

data EL = StringRule | VarRule | LamRule | AppRule | HoleRule | FormatRule Format
derive instance Generic EL _
derive instance Eq EL
derive instance Ord EL
instance Show EL where show = genericShow

instance TreeNode EL where
  kidsCount = case _ of
    StringRule -> 1
    VarRule -> 1
    LamRule -> 2
    AppRule -> 2
    HoleRule -> 0
    FormatRule _ -> 1

instance PrettyTreeNode EL where
  prettyTreeNode el = case el of
    StringRule -> assertValidTreeKids "prettyTreeNode" el \[str] -> str
    VarRule -> assertValidTreeKids "prettyTreeNode" el \[x] -> x
    LamRule -> assertValidTreeKids "prettyTreeNode" el \[x, b] -> "λ" <> x <> "." <> b
    AppRule -> assertValidTreeKids "prettyTreeNode" el \[f, a] -> f <+> a
    HoleRule -> assertValidTreeKids "prettyTreeNode" el \[] -> "?"
    FormatRule Indent -> assertValidTreeKids "prettyTreeNode" el \[a] -> "<indent>" <+> a

data Format = Indent
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
      HoleRule -> do
        PL.SortingRule
          { parameters: Set.fromFoldable []
          , kids: []
          , parent: PL.makeConstRuleSort TermSort [] }
      FormatRule format -> do
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
      HoleRule -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: []  }
      FormatRule format -> do
        PL.ChangingRule
          { parameters: Set.fromFoldable []
          , kids: [Reflect (PL.makeConstRuleSortNode TermSort) []] }
  , defaultExpr: case _ of
      Tree {node: PL.SortNode (StringValue _)} -> Nothing
      Tree {node: PL.SortNode StringSort} -> Just $ PL.makeExpr StringRule (PL.RuleSortVarSubst $ Map.fromFoldable [PL.MakeRuleSortVar "str" /\ PL.makeSort (StringValue "") []]) {} []
      Tree {node: PL.SortNode TermSort} ->
        -- Just $ makeHole
        -- Just $ makeApp makeHole makeHole
        -- Just $ makeApp (makeApp (makeLam "x" (makeApp (makeLam "x" makeHole) makeHole)) (makeApp (makeLam "x" makeHole) makeHole)) (makeApp (makeApp (makeLam "x" makeHole) makeHole) (makeApp (makeLam "x" makeHole) makeHole))
        -- Just $ makeApp (makeIndent (makeApp makeHole makeHole)) (makeApp (makeIndent (makeApp makeHole (makeIndent makeHole))) makeHole)
        Just $ 
          makeApp
            (makeIndent (makeApp makeHole (makeIndent (makeApp makeHole (makeIndent (makeApp makeHole makeHole))))))
            (makeIndent (makeApp makeHole (makeIndent (makeApp makeHole (makeIndent (makeApp makeHole makeHole))))))
        -- Just $ makeExample 10
  , topSort: PL.makeSort TermSort []
  }

makeExample :: Int -> Expr
makeExample 0 = makeHole
makeExample n = makeApp (makeIndent (makeExample (n - 1))) (makeExample (n - 1))

makeVar :: String -> Expr
makeVar str =
  PL.makeExpr VarRule (PL.RuleSortVarSubst $ Map.fromFoldable [PL.MakeRuleSortVar "s" /\ PL.makeSort StringSort [PL.makeSort (StringValue str) []]]) {}
    [PL.makeExpr StringRule (PL.RuleSortVarSubst $ Map.fromFoldable [PL.MakeRuleSortVar "str" /\ PL.makeSort (StringValue str) []]) {} []]

makeLam :: String -> Expr -> Expr
makeLam str b =
  PL.makeExpr LamRule (PL.RuleSortVarSubst $ Map.fromFoldable [PL.MakeRuleSortVar "s" /\ PL.makeSort StringSort [PL.makeSort (StringValue str) []]]) {}
    [ PL.makeExpr StringRule (PL.RuleSortVarSubst $ Map.fromFoldable [PL.MakeRuleSortVar "str" /\ PL.makeSort (StringValue str) []]) {} []
    , b ]

makeApp :: Expr -> Expr -> Expr
makeApp f a = PL.makeExpr AppRule (PL.RuleSortVarSubst $ Map.fromFoldable []) {} [f, a]

makeHole :: Expr
makeHole = PL.makeExpr HoleRule (PL.RuleSortVarSubst $ Map.fromFoldable []) {} []

makeIndent :: Expr -> Expr
makeIndent a = PL.makeExpr (FormatRule Indent) (PL.RuleSortVarSubst $ Map.fromFoldable []) {} [a]

renderer :: Renderer
renderer = PR.Renderer
  { name: "basic"
  , language
  , topCtx: {indentLevel: 0}
  , topEnv: {}
  , arrangeExpr:
      let punc str = PR.PunctuationArrangeKid [HH.span_ [HH.text str]] in
      let ind i = PR.IndentationArrangeKid (Array.replicate (i + 1) (HH.text "  ")) in
      \node -> case node of
        PL.AnnExprNode {label: StringRule} -> assertValidTreeKids "prettyTreeNode" node \[mstr] -> do
          str_ /\ _ <- mstr
          pure [PR.ExprKidArrangeKid str_]
        PL.AnnExprNode {label: VarRule} -> assertValidTreeKids "prettyTreeNode" node \[mx] -> do
          x_ /\ x <- mx
          pure [punc "#", PR.ExprKidArrangeKid x_]
        PL.AnnExprNode {label: LamRule} -> assertValidTreeKids "prettyTreeNode" node \[mx, mb] -> do
          x_ /\ x <- mx 
          b_ /\ b <- mb 
          pure [punc "(", punc "λ", PR.ExprKidArrangeKid x_, punc ".", PR.ExprKidArrangeKid b_, punc ")"]
        PL.AnnExprNode {label: AppRule} -> assertValidTreeKids "prettyTreeNode" node \[mf, ma] -> do
          f_ /\ f <- mf 
          a_ /\ a <- ma 
          pure [punc "(", PR.ExprKidArrangeKid f_, punc " ", PR.ExprKidArrangeKid a_, punc ")"]
        PL.AnnExprNode {label: HoleRule} -> assertValidTreeKids "prettyTreeNode" node \[] -> do
          holeCount <- State.gets _.holeCount
          State.modify_ _ {holeCount = holeCount + 1}
          pure [punc ("?" <> show holeCount)]
        PL.AnnExprNode {label: FormatRule Indent} -> assertValidTreeKids "prettyTreeNode" node \[ma] -> do
          ctx <- ask
          a_ /\ a <- local (R.modify (Proxy :: Proxy "indentLevel") (1 + _)) ma
          pure [ind ctx.indentLevel, PR.ExprKidArrangeKid a_]
  }
