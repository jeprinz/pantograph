module Pantograph.Specific.LC where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude

import Bug (bug)
import Control.Monad.State as State
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (curry, uncurry)
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hole (hole)
import Pantograph.Generic.Language.Common (unAnnExprNode)
import Partial.Unsafe (unsafePartial)
import Record as R
import Text.Pretty (class Pretty, pretty, (<+>))
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))

data EL = StringRule | VarRule | LamRule | AppRule | HoleRule
derive instance Generic EL _
derive instance Eq EL
derive instance Ord EL
instance Show EL where show = genericShow

instance PrettyTreeNode EL where
  prettyTreeNode = curry case _ of
    StringRule /\ [str] -> str
    VarRule /\ [x] -> x
    LamRule /\ [x, b] -> "λ" <> x <> "." <> b
    AppRule /\ [f, a] -> f <+> a
    HoleRule /\ [] -> "?"
    el /\ kids -> bug $ Pretty.newlines
      [ "invalid Expr:"
      , "  - el: " <> show el
      , "  - kids: " <> show kids
      ]

data SN = StringValue String | StringSort | TermSort
derive instance Generic SN _
derive instance Eq SN
derive instance Ord SN
instance Show SN where show = genericShow

language :: Language SN EL
language = Language
  { name: "lambda calculus"
  , getSortingRule: case _ of
      StringRule -> do
        let str = MakeRuleSortVar "str"
        SortingRule
          { parameters: Set.fromFoldable [str]
          , kids: []
          , parent: makeConstRuleSort StringSort [makeVarRuleSort str] }
      VarRule -> do
        let x = MakeRuleSortVar "x"
        SortingRule
          { parameters: Set.fromFoldable [x]
          , kids: [makeConstRuleSort StringSort [makeVarRuleSort x]]
          , parent: makeConstRuleSort TermSort [] }
      LamRule -> do
        let x = MakeRuleSortVar "x"
        SortingRule
          { parameters: Set.fromFoldable [x]
          , kids: [makeConstRuleSort StringSort [makeVarRuleSort x], makeConstRuleSort TermSort []]
          , parent: makeConstRuleSort TermSort [] }
      AppRule -> do
        SortingRule
          { parameters: Set.fromFoldable []
          , kids: [makeConstRuleSort TermSort [], makeConstRuleSort TermSort []]
          , parent: makeConstRuleSort TermSort [] }
      HoleRule -> do
        SortingRule
          { parameters: Set.fromFoldable []
          , kids: []
          , parent: makeConstRuleSort TermSort [] }
  , getChangingRule: case _ of
      StringRule -> do
        ChangingRule
          { parameters: Set.fromFoldable []
          , kids: []  }
      VarRule -> do
        let x = MakeRuleSortVar "x"
        ChangingRule
          { parameters: Set.fromFoldable [x]
          , kids: [Replace (makeConstRuleSort StringSort [makeVarRuleSort x]) (makeConstRuleSort TermSort [])] }
      LamRule -> do
        let x = MakeRuleSortVar "x"
        ChangingRule
          { parameters: Set.fromFoldable [x]
          , kids: [Replace (makeConstRuleSort StringSort [makeVarRuleSort x]) (makeConstRuleSort TermSort []), Replace (makeConstRuleSort TermSort []) (makeConstRuleSort TermSort [])]  }
      AppRule -> do
        ChangingRule
          { parameters: Set.fromFoldable []
          , kids: [Replace (makeConstRuleSort TermSort []) (makeConstRuleSort TermSort []), Replace (makeConstRuleSort TermSort []) (makeConstRuleSort TermSort [])] }
      HoleRule -> do
        ChangingRule
          { parameters: Set.fromFoldable []
          , kids: []  }
  , defaultExpr: case _ of
      Tree {node: SortNode (StringValue _)} -> Nothing
      Tree {node: SortNode StringSort} -> Just $ makeExpr StringRule (RuleSortVarSubst $ Map.fromFoldable [MakeRuleSortVar "str" /\ makeSort (StringValue "") []]) {} []
      Tree {node: SortNode TermSort} ->
        -- Just $ makeHole
        Just $ makeApp makeHole makeHole
        -- Just $ makeApp (makeApp makeHole makeHole) (makeApp makeHole makeHole)
  , topSort: makeSort TermSort []
  }

makeVar str =
  makeExpr VarRule (RuleSortVarSubst $ Map.fromFoldable [MakeRuleSortVar "s" /\ makeSort StringSort [makeSort (StringValue str ) []]]) {}
    [makeExpr StringRule (RuleSortVarSubst $ Map.fromFoldable [MakeRuleSortVar "str" /\ makeSort (StringValue str ) []]) {} []]

makeHole = makeExpr HoleRule (RuleSortVarSubst $ Map.fromFoldable []) {} []

makeApp f a = makeExpr AppRule (RuleSortVarSubst $ Map.fromFoldable []) {} [f, a]

-- renderer :: Renderer () (holeCount :: Int) EL ED SN
renderer = Renderer
  { name: "basic"
  , language
  , topCtx: {}
  -- , topEnv: {holeCount: 0}
  , topEnv: {}
  , arrangeExpr: curry case _ of
      AnnExprNode {label: StringRule} /\ [] -> do
        pure []
      AnnExprNode {label: VarRule} /\ [mx] -> do
        x_ /\ x <- mx
        pure [PunctuationArrangeKid [HH.text "#"], ExprKidArrangeKid x_]
      AnnExprNode {label: LamRule} /\ [mx, mb] -> do
        x_ /\ x <- mx 
        b_ /\ b <- mb 
        pure [PunctuationArrangeKid [HH.text "(λ"], ExprKidArrangeKid x_, ExprKidArrangeKid b_, PunctuationArrangeKid [HH.text ")"]]
      AnnExprNode {label: AppRule} /\ [mf, ma] -> do
        f_ /\ f <- mf 
        a_ /\ a <- ma 
        pure [PunctuationArrangeKid [HH.text "("], ExprKidArrangeKid f_, ExprKidArrangeKid a_, PunctuationArrangeKid [HH.text ")"]]
      AnnExprNode {label: HoleRule} /\ [] -> do
        holeCount <- State.gets _.holeCount
        State.modify_ _ {holeCount = holeCount + 1}
        pure [PunctuationArrangeKid [HH.text $ "?" <> show holeCount]]
      node /\ mkids -> do
        _ /\ kids <- Array.unzip <$> sequence mkids
        bug $ Pretty.newlines
          [ "invalid Expr:"
          , "  - node: " <> show (unAnnExprNode node)
          , "  - kids: " <> show (unAnnExprNode <$> kids)
          ]
  }
