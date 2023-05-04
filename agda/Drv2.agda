module Drv2 where

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

-- data In : Γ n x A where
data In : Ctx → ℕ → String → Exp → Set where
  here : ∀ {x} {A} {Γ} → In ((x , A) ∷ Γ) 0 x A
  there : ∀ {y} {B} {x} {A} {Γ} {n} → In Γ n x A → In ((y , B) ∷ Γ) (suc n) x A

data _⊢_ : Ctx → Exp → Set where
  hole : ∀ {Γ} e → Γ ⊢ e

  natType : ∀ {Γ} → Γ ⊢ baseKindExp
  arrowType : ∀ {Γ} → Γ ⊢ baseKindExp → Γ ⊢ baseKindExp → Γ ⊢ baseKindExp

  natTerm : ∀ {Γ} → ℕ → Γ ⊢ natTypeExp
  varTerm : ∀ {Γ} {n} x {A} → In Γ n x A → Γ ⊢ A
  lamTerm : ∀ {Γ} x A {B} → ((x , A) ∷ Γ) ⊢ B → Γ ⊢ arrowTypeExp A B
  appTerm : ∀ {Γ} {A} {B} → Γ ⊢ arrowTypeExp A B → Γ ⊢ A → Γ ⊢ B


module Examples where
  ex1 : [] ⊢ _
  ex1 = lamTerm "x" (holeTypeExp "?A") (varTerm "x" here)

  ex2 : [] ⊢ _
  ex2 = appTerm (hole (arrowTypeExp (holeTypeExp "?A") (holeTypeExp "?B"))) (hole (holeTypeExp "?A"))

  -- PROBLEM: allows you to put the wrong things in the hole
  ex3 : [] ⊢ _
  ex3 = lamTerm "x" (holeTypeExp "?A") (hole (natTermExp 1))
