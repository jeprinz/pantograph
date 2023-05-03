module Drv1 where

import Data.Nat as Nat
open import Data.Nat using (ℕ ; zero ; suc)
import Data.List as List
open import Data.List using (List ; [_] ; _∷_ ; [])
open import Data.Product
open import Data.String

-- Label
data Label : Set where
  -- kinds
  BaseKind : Label
  -- types
  NatType : Label
  ArrowType : Label
  HoleType : String → Label
  -- terms
  NatTerm : ℕ → Label
  VarTerm : Label
  LamTerm : Label
  AppTerm : Label
  HoleTerm : Label
  -- term vars
  TermVar : String → Label

-- Expression
data Exp : Set where
  Exp[_,_] : Label → List Exp → Exp
  Hole[_] : String → Exp

-- kinds

baseKindExp = Exp[ BaseKind , [] ]

-- types

natTypeExp = Exp[ NatType , [] ]

arrowTypeExp : Exp → Exp → Exp
arrowTypeExp A1 A2 = Exp[ NatType , (A1 ∷ A2 ∷ []) ]

holeTypeExp : String → Exp
holeTypeExp id = Exp[ HoleType id , [] ]

-- terms

natTermExp : ℕ → Exp
natTermExp n = Exp[ NatTerm n , [] ]

varTermExp : Exp → Exp
varTermExp x = Exp[ VarTerm , [ x ] ]

lamTermExp : Exp → Exp → Exp
lamTermExp x b = Exp[ LamTerm , (x ∷ b ∷ []) ]

appTermExp : Exp → Exp → Exp
appTermExp f a = Exp[ AppTerm , (f ∷ a ∷ []) ]

holeTermExp : Exp
holeTermExp = Exp[ HoleTerm , [] ]

-- term vars

termVarExp : String → Exp
termVarExp str = Exp[ TermVar str , [] ]

-- Context
-- Note that variables are also expressions.
Ctx : Set
Ctx = List (String × Exp)

data _[_]↦_⦂_ : Ctx → ℕ → String → Exp → Set where
  here : ∀ {Γ} {x} {A} →
    ((x , A) ∷ Γ) [ 0 ]↦ x ⦂ A
  there : ∀ {Γ} {x} {n} {A} {y} {B} →
    Γ [ suc n ]↦ x ⦂ A →
    ((y , B) ∷ Γ) [ n ]↦ x ⦂ A

-- Derivation
data _⊢_⦂_ : Ctx → Exp → Exp → Set where
  -- hole

  hole : ∀ {Γ} e s → Γ ⊢ e ⦂ s

  -- types

  natType : ∀ {Γ} → Γ ⊢ natTypeExp ⦂ baseKindExp
  arrowType : ∀ {Γ} {A} {B} → (Γ ⊢ A ⦂ baseKindExp) → (Γ ⊢ B ⦂ baseKindExp) → (Γ ⊢ arrowTypeExp A B ⦂ baseKindExp)

  -- terms

  natTerm : ∀ {Γ} n → Γ ⊢ natTermExp n ⦂ natTypeExp

  varTerm : ∀ {Γ} {A} x {n} → Γ [ n ]↦ x ⦂ A → Γ ⊢ varTermExp (termVarExp x) ⦂ A

  lamTerm : ∀ {Γ} x {A} {B} {b} → Γ ⊢ A ⦂ baseKindExp → ((x , A) ∷ Γ) ⊢ b ⦂ B → Γ ⊢ lamTermExp (termVarExp x) b ⦂ arrowTypeExp A B

  appTerm : ∀ {Γ} {A} {B} {f} {a} → Γ ⊢ f ⦂ arrowTypeExp A B → Γ ⊢ a ⦂ A → Γ ⊢ appTermExp f a ⦂ B

-- using holes as types
_ : [] ⊢ _ ⦂ _
_ =
  appTerm
    (lamTerm "x" (hole (arrowTypeExp (holeTypeExp "?A") (holeTypeExp "?A")) baseKindExp) (varTerm "x" here))
    (lamTerm "y" (hole (holeTypeExp "?A") baseKindExp) (varTerm "y" here))

_ : [] ⊢ _ ⦂ _
_ =
  appTerm
    (lamTerm "x" (arrowType (hole (holeTypeExp "?A") baseKindExp) (hole (holeTypeExp "?A") baseKindExp)) (varTerm "x" here))
    (lamTerm "y" (hole (holeTypeExp "?A") baseKindExp) (varTerm "y" here))

-- using holes as terms
_ : [] ⊢ _ ⦂ _
_ = appTerm (lamTerm "x" natType (hole holeTermExp natTypeExp)) (natTerm 32)

_ : [] ⊢ _ ⦂ _
_ = appTerm (hole holeTermExp (arrowTypeExp natTypeExp natTypeExp)) (natTerm 32)

