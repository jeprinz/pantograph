module Pantograph.Specific.ULC where

import Data.Tuple.Nested
import Prelude

import Data.Derivative (class Derivative, differentiate)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Utilities as HU
import Halogen.VDom.Driver as VDomDriver
import Pantograph.Generic.Language as L
import Pantograph.Generic.Rendering as R
import Text.Pretty (class Pretty, class Pretty1, class PrettyS1, pretty)
import Type.Proxy (Proxy(..))

type OpenSort = L.OpenSort Joint
type RuleSort = L.RuleSort Joint
type OpenExprPath dir = L.OpenExprPath dir Rule Joint Tooth
type SomeOpenExprPath = L.SomeOpenExprPath Rule Joint Tooth
type OpenChange = L.OpenChange Joint Tooth
type OpenExpr = L.OpenExpr Rule Joint
type OpenExprCursor = L.OpenExprCursor Rule Joint Tooth
type OpenExprSelect = L.OpenExprSelect Rule Joint Tooth
type ProductionRule = L.ProductionRule Joint
type ChangeRule = L.ChangeRule Joint Tooth

-- Joint

data Joint a
  -- terms
  = Lam a a
  | App a a
  | Ref a
  -- types
  | Expr

derive instance Functor Joint
derive instance Foldable Joint
derive instance Traversable Joint

instance Pretty1 Joint where
  pretty1 = case _ of
    Lam x b -> "(λ " <> pretty x <> " ↦ " <> pretty b <> ")"
    App f a -> "(" <> pretty f <> " " <> pretty a <> ")"
    Ref x -> "#" <> pretty x
    Expr -> "Expr"

instance L.IsJoint Joint

-- Tooth

data Tooth a
  = Lam'Param a
  | Lam'Body a
  | App'Apl a
  | App'Arg a
  | Ref'Var

derive instance Functor Tooth
derive instance Foldable Tooth

instance Derivative Joint Tooth where
  differentiate (Lam x b) = Lam (x /\ Lam'Param b) (b /\ Lam'Body x)
  differentiate (App f a) = App (f /\ App'Apl f) (a /\ App'Apl f)
  differentiate (Ref x) = Ref (x /\ Ref'Var)
  differentiate Expr = Expr

  integrate x (Lam'Param b) = Lam x b
  integrate b (Lam'Body x) = Lam x b
  integrate f (App'Apl a) = App f a
  integrate a (App'Arg f) = App f a
  integrate x Ref'Var = Ref x

instance PrettyS1 Tooth where
  prettyS1 (Lam'Param b) s = "(λ "<> s <> " ↦ " <> pretty b <> ")"
  prettyS1 (Lam'Body x) s = "(λ " <> pretty x <> " ↦ " <> s <> ")"
  prettyS1 (App'Apl a) s = "(" <> s <> " " <> pretty a <> ")"
  prettyS1 (App'Arg f) s = "(" <> pretty f <> " " <> s <> ")"
  prettyS1 Ref'Var s = "#" <> s

instance Pretty1 Tooth where
  pretty1 (Lam'Param b) = "(λ ⌶ ↦ " <> pretty b <> ")"
  pretty1 (Lam'Body x) = "(λ " <> pretty x <> " ↦ ⌶)"
  pretty1 (App'Apl a) = "(⌶ " <> pretty a <> ")"
  pretty1 (App'Arg f) = "(" <> pretty f <> " ⌶)"
  pretty1 Ref'Var = "#⌶"

-- Rule

data Rule
  = LamRule
  | AppRule
  | RefRule
  | HoleRule

derive instance Generic Rule _
derive instance Eq Rule
derive instance Ord Rule
instance Show Rule where show = genericShow
instance Pretty Rule where pretty = show

-- IsLanguage

instance L.IsLanguage Rule Joint Tooth where
  productionRule = do
    let term_sort = L.Fix $ L.InjectSortJoint $ L.InjectRuleJoint Expr
    let label_sort label = L.Fix $ L.SomeSymbol $ L.Fix $ L.InjectSortJoint $ L.RuleVar label
    case _ of
      LamRule ->
        let var_label = L.MakeRuleVar "lam_var_label" in
        let lam_param_sort = label_sort var_label in
        L.ProductionRule
          { parameters: Set.fromFoldable [var_label]
          , kidSorts: L.InjectOpenJoint $ Lam lam_param_sort term_sort
          , parentSort: term_sort }
      AppRule ->
        L.ProductionRule
          { parameters: Set.empty
          , kidSorts: L.InjectOpenJoint $ App term_sort term_sort
          , parentSort: term_sort }
      RefRule -> do
        let var_label = L.MakeRuleVar "ref_var_label"
        let var_label_sort = label_sort var_label
        L.ProductionRule
          { parameters: Set.fromFoldable [var_label]
          , kidSorts: L.InjectOpenJoint $ Ref var_label_sort
          , parentSort: term_sort }
      HoleRule -> do
        let holeVar = L.freshHoleVar "hole"
        L.ProductionRule
          { parameters: Set.empty
          , kidSorts: L.Hole holeVar
          , parentSort: term_sort }

  changeRule rule = L.defaultChangeRule rule

  defaultExpr (L.Fix (L.InjectSortJoint (L.InjectOpenJoint Expr))) = Just $ L.Fix $ L.Expr HoleRule (L.RuleVarSubst Map.empty) $ L.Hole $ L.freshHoleVar "expr"
  defaultExpr (L.Fix (L.SomeSymbol (L.Fix (L.Symbol str)))) = Just $ L.Fix $ L.SymbolExpr str
  defaultExpr _ = Nothing

  splitChange {change, sort} = {down: L.idChange sort, up: change, sort}

  validCursorSort _ = true

  validSelectionSorts 
    { top: L.Fix (L.InjectSortJoint (L.InjectOpenJoint Expr))
    , bot: L.Fix (L.InjectSortJoint (L.InjectOpenJoint Expr)) } = true
  validSelectionSorts _ = false

-- IsEditor

type Ctx :: Row Type
type Ctx = ()

type Env :: Row Type
type Env = ()

instance R.IsEditor Rule Joint Tooth where
  arrangeOpenExpr' _rule _sigma (Lam x b) =
    pure $ HH.div
      [HU.classNames ["Lam"]]
      [HH.text "(", HH.text "λ", punctuation.space, x.html, punctuation.space, HH.text "↦", punctuation.space, b.html, HH.text ")"]
  arrangeOpenExpr' _rule _sigma (App f a) = do
    pure $ HH.div
      [HU.classNames ["App"]]
      [HH.text "(", f.html, punctuation.space, a.html, HH.text ")"]
  arrangeOpenExpr' _rule _sigma (Ref x) = do
    pure $ HH.div
      [HU.classNames ["Ref"]]
      [HH.text "#", x.html]
  arrangeOpenExpr' _rule _sigma Expr = do
    pure $ HH.div
      [HU.classNames ["Expr"]]
      [HH.text "Expr"]

punctuation = 
  { space: HH.div [HU.classNames ["punctuation", "space"]] [HH.text " "]
  }

-- run

lam :: _ -> _ -> OpenExpr
lam x a = L.Fix $ L.Expr LamRule (L.RuleVarSubst $ Map.fromFoldable [L.MakeRuleVar "x" /\ holeSort "x"]) $ L.InjectOpenJoint $ Lam x a

app :: _ -> _ -> OpenExpr
app f a = L.Fix $ L.Expr AppRule (L.RuleVarSubst Map.empty) $ L.InjectOpenJoint $ App f a

ref :: _ -> OpenExpr
ref x = L.Fix $ L.Expr RefRule (L.RuleVarSubst $ Map.fromFoldable [L.MakeRuleVar "x" /\ holeSort "x"]) $ L.InjectOpenJoint $ Ref x

holeExpr :: String -> OpenExpr
holeExpr str = L.Fix $ L.Expr HoleRule (L.RuleVarSubst Map.empty) $ L.Hole $ L.freshHoleVar str

holeSort :: String -> OpenSort
holeSort str = L.Fix $ L.InjectSortJoint $ L.Hole $ L.freshHoleVar str

symbolExpr :: String -> OpenExpr
symbolExpr str = L.Fix $ L.SymbolExpr str

run = VDomDriver.runUI R.editorComponent
  { buffer:
      { 
        expr: lam (symbolExpr "x") $ lam (symbolExpr "y") $ lam (symbolExpr "z") $ ref (symbolExpr "x")
      , ctx: {}
      , env: {} } }
