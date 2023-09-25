module Pantograph.Specific.ULC where

import Data.Tuple.Nested
import Prelude

import Data.Derivative (class Derivative, differentiate)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Halogen.VDom.Driver as VDomDriver
import Hole (hole)
import Pantograph.Generic.Language as L
import Pantograph.Generic.Rendering as R
import Text.Pretty (class Pretty, class Pretty1, pretty)
import Type.Proxy (Proxy(..))

type Sort = L.Sort Joint
type RuleSort = L.RuleSort Joint
type TermPath dir = L.TermPath dir Rule Joint Joint'
type SomeTermPath = L.SomeTermPath Rule Joint Joint'
type Change = L.Change Joint Joint'
type Term = L.Term Rule Joint
type TermCursor = L.TermCursor Rule Joint Joint'
type TermSelect = L.TermSelect Rule Joint Joint'
type ProductionRule = L.ProductionRule Joint
type ChangeRule = L.ChangeRule Joint Joint'

-- Joint

data Joint a
  -- terms
  = Lam a a
  | App a a
  | Ref a
  -- types
  | Term

derive instance Functor Joint
derive instance Foldable Joint

instance Pretty1 Joint where
  pretty1 = case _ of
    Lam x b -> "(λ " <> pretty x <> " ↦ " <> pretty b <> ")"
    App f a -> "(" <> pretty f <> " " <> pretty a <> ")"
    Ref x -> "#" <> pretty x
    Term -> "Term"

instance L.IsJoint Joint

-- Joint'

data Joint' a
  = Lam'Param a
  | Lam'Body a
  | App'Apl a
  | App'Arg a
  | Ref'Var

derive instance Functor Joint'
derive instance Foldable Joint'

instance Derivative Joint Joint' where
  differentiate (Lam x b) = Lam (x /\ Lam'Param b) (b /\ Lam'Body x)
  differentiate (App f a) = App (f /\ App'Apl f) (a /\ App'Apl f)
  differentiate (Ref x) = Ref (x /\ Ref'Var)
  differentiate Term = Term

  integrate x (Lam'Param b) = Lam x b
  integrate b (Lam'Body x) = Lam x b
  integrate f (App'Apl a) = App f a
  integrate a (App'Arg f) = App f a
  integrate x Ref'Var = Ref x

instance Pretty1 Joint' where
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

derive instance Generic Rule _
derive instance Eq Rule
derive instance Ord Rule
instance Show Rule where show = genericShow
instance Pretty Rule where pretty = show

-- IsLanguage

instance L.IsLanguage Rule Joint Joint' where
  productionRule =
    let term_sort = L.Fix $ L.InjectSortJoint $ L.InjectRuleVarJoint Term in
    let label_sort label = L.Fix $ L.SomeLabel $ L.Fix $ L.InjectSortJoint $ L.RuleVar label in
    case _ of
      LamRule ->
        let var_label = L.MakeRuleVar "lam_var_label" in
        let lam_param_sort = label_sort var_label in
        L.ProductionRule
          { quantifiers: Set.fromFoldable [var_label]
          , kidSorts: Lam lam_param_sort term_sort
          , parentSort: term_sort }
      AppRule ->
        L.ProductionRule
          { quantifiers: Set.empty
          , kidSorts: App term_sort term_sort
          , parentSort: term_sort }
      RefRule ->
        let var_label = L.MakeRuleVar "ref_var_label" in
        let var_label_sort = label_sort var_label in
        L.ProductionRule
          { quantifiers: Set.fromFoldable [var_label]
          , kidSorts: Ref var_label_sort
          , parentSort: term_sort }

  changeRule rule = L.defaultChangeRule rule

  defaultTerm = hole "TODO"

  splitChange = hole "TODO"

  validCursorSort _ = true

  validSelectionSorts 
    { top: L.Fix (L.InjectSortJoint (L.InjectHoleJoint Term))
    , bot: L.Fix (L.InjectSortJoint (L.InjectHoleJoint Term)) } = true
  validSelectionSorts _ = false

  digChange = hole "TODO"

  generalize = hole "TODO"

  specialize = hole "TODO"

-- IsEditor

type Ctx = ()

type Env = ()

-- instance R.IsEditor () () Rule Joint Joint' where
--   arrangeTerm = ?a

-- run

-- run body = VDomDriver.runUI R.editorComponent
--   { term: ?a
--   , ctx: ?a
--   , env: ?a }