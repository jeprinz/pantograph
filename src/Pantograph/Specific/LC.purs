module Pantograph.Specific.LC where

import Data.Either.Nested
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Bug (bug)
import Control.Monad.State as State
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hole (hole)
import Record as R
import Text.Pretty (class Pretty, pretty, quotes)
import Type.Proxy (Proxy(..))

data R = StringRule | VarRule | LamRule | AppRule | HoleRule

data N
  -- StringSort
  = String
  -- TermSort
  | Var
  | Lam
  | App
  | Hole
  -- Sort
  | StringValueSort String
  | StringSort
  | TermSort

instance Pretty N where
  pretty String = "String"
  pretty Var = "#"
  pretty Lam = "λ"
  pretty App = "$"
  pretty Hole = "?"
  pretty (StringValueSort str) = quotes str
  pretty StringSort = "StringSort"
  pretty TermSort = "TermSort"

type D :: Row Type
type D = ()

type Ctx = ()

type Env = ()

-- Language

language :: Language R N D
language = Language {name: "LC", getSortingRule, getChangingRule, defaultExpr, topSort}

getSortingRule :: R -> SortingRule N D
getSortingRule = case _ of
  StringRule -> do
    let s = MakeRuleVar "s"
    SortingRule
      { parameters: Set.fromFoldable [s]
      , kids: [makeVarRuleSort s]
      , parent: makeRuleSort StringSort {} [makeVarRuleSort s] }
  VarRule -> do
    let s = MakeRuleVar "s"
    SortingRule
      { parameters: Set.fromFoldable [s]
      , kids: [makeRuleSort StringSort {} [makeVarRuleSort s]]
      , parent: makeRuleSort TermSort {} [] }
  LamRule -> do
    SortingRule
      { parameters: Set.empty
      , kids: [makeRuleSort TermSort {} [], makeRuleSort TermSort {} []]
      , parent: makeRuleSort TermSort {} [] }
  AppRule -> do
    SortingRule
      { parameters: Set.empty
      , kids: [makeRuleSort TermSort {} [], makeRuleSort TermSort {} []]
      , parent: makeRuleSort TermSort {} [] }
  HoleRule -> do
    SortingRule
      { parameters: Set.empty 
      , kids: []
      , parent: makeRuleSort TermSort {} [] }

getChangingRule :: R -> ChangingRule N D
getChangingRule = case _ of
  StringRule -> do
    let s = MakeRuleVar "s"
    ChangingRule
      { parameters: Set.fromFoldable [s]
      , kids: [RuleReplace (makeRuleSort StringSort {} [makeVarRuleSort s]) (makeRuleSort StringSort {} [makeVarRuleSort s])] }
  VarRule -> do
    let s = MakeRuleVar "s"
    ChangingRule
      { parameters: Set.fromFoldable [s]
      , kids: [RuleReplace (makeVarRuleSort s) (makeRuleSort StringSort {} [makeRuleSort TermSort {} []])] }
  LamRule -> do
    ChangingRule
      { parameters: Set.empty
      , kids: [makeRuleReflect TermSort {} [], makeRuleReflect TermSort {} []] }
  AppRule -> do
    ChangingRule
      { parameters: Set.empty
      , kids: [makeRuleReflect TermSort {} [], makeRuleReflect TermSort {} []] }
  HoleRule -> do
    ChangingRule
      { parameters: Set.empty
      , kids: [] }

defaultExpr :: Sort N D -> Maybe (Expr R N D (Sort N D))
defaultExpr = case _ of
  Sort {node: SortNode {n: StringSort, d: {}}, kids: [s]} ->
    Just $ makeExpr StringRule String (RuleVarSubst $ Map.fromFoldable [MakeRuleVar "s" /\ s]) {} []
  Sort {node: SortNode {n: TermSort, d: {}}, kids: []} ->
    -- Just $ makeExpr HoleRule Hole (RuleVarSubst Map.empty) {} []
    Just $ 
      makeExpr AppRule App (RuleVarSubst Map.empty) {}
        [ makeExpr HoleRule Hole (RuleVarSubst Map.empty) {} []
        , makeExpr HoleRule Hole (RuleVarSubst Map.empty) {} [] ]
  sort -> Nothing

topSort :: Sort N D
topSort = makeSort TermSort {} []

-- Renderer

renderer :: Renderer Ctx Env R N D
renderer = Renderer {name: "LC", arrangeExpr, topCtx, topEnv}

arrangeExpr :: forall a.
  ExprNode R N D (Sort N D) ->
  Array (RenderM Ctx Env R N D (ExprNode R N D (Sort N D) /\ a)) ->
  RenderM Ctx Env R N D (Array (Array (Html R N D) \/ a))
arrangeExpr node@(ExprNode {r: StringRule, n: String}) [] = do
  case getExprNodeSort language node of
    Sort {node: SortNode {n: StringSort}, kids: [Sort {node: SortNode {n: StringValueSort str}}]} ->
      pure [Left [punctuation "?"], Left [HH.text str]]
    _ -> bug $ "invalid Sort"
arrangeExpr (ExprNode {r: VarRule, n: Var}) [ms] = do
  _s /\ sId <- ms
  pure [Left [punctuation "#"], Right sId]
arrangeExpr (ExprNode {r: LamRule, n: Lam}) [mx, mb] = do
  _x /\ xId <- mx
  _b /\ bId <- mb
  pure [Left [punctuation "("], Left [punctuation "λ"], Right xId, Left [punctuation "↦"], Right bId, Left [punctuation ")"]]
arrangeExpr (ExprNode {r: AppRule, n: App}) [mf, ma] = do
  _f /\ fId <- mf
  _a /\ aId <- ma
  pure [Left [punctuation "("], Right fId, Left [punctuation " "], Right aId, Left [punctuation ")"]]
arrangeExpr (ExprNode {r: HoleRule, n: Hole}) [] = do
  holeIndex <- State.gets _.holeCount
  State.modify_ (R.modify (Proxy :: Proxy "holeCount") (1 + _))
  pure [Left [punctuation "?"], Left [HH.text $ show holeIndex]]
arrangeExpr _node _kids = bug $ "invalid ExprNode"

punctuation str = HH.span [HP.classes [HH.ClassName "punctuation"]] [HH.text str]

topCtx :: Record Ctx
topCtx = {}

topEnv :: Record Env
topEnv = {}

engine = Engine {name: "LC", language, renderer}
