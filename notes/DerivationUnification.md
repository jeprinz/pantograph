# Derivation Unification

Proposal: derivation unification <- Expession unification, typing rules.

```
Ctx : Type

Exp : Type
⋆, ℕ : Exp
?_ : TypeHole -> Exp

Drv : Ctx -> Exp -> Exp -> Type
_⊢_:_ : ∀ Γ t T -> Drv Γ t T
```

Some typing rules (to construct derivations)
```
natType : ∀ {Γ} -> Drv (Γ ⊢ ℕ : ⋆)
holeType : ∀ {Γ} {X} -> (Γ ⊢ ?X : ⋆)
holeTerm : ∀ {Γ} {A} -> (Γ ⊢ A : ⋆) -> (Γ ⊢ A)
```

Suppose we have the derivation
```
Γ₁ : Ctx
X₁ : TypeHole

a : Γ₁ ⊢ ?X
a = holeTerm {Γ₁} holeType
```
And we want to apply the type hole substitution (which is over Expessions)
```
σ₁ = [X ↦ ℕ]
```
We need to know the derivation for `ℕ` here, and `σ` must exist in some context
that respects that derivation. So actually here's the rules for constructing
type hole substitutions (not type var substitutions, since that changes the
context):
```
Sub : Ctx -> Type

idSub : Sub Γ
typeHoleSub : ∀ {Γ} -> TypeHole -> (Γ ⊢ ⋆) -> Sub Γ -> Sub Γ
```
(Perhaps this indicates that we should be keeping track of the type hole context
in the signatures as well.)

So the actual substitution should have been:
```
σ₁ : Sub Γ
σ₁ = typeHoleSub X₁ ℕ idSub
```

Applying substitutions:
```
subType : ∀ {Γ} -> Sub Γ -> (Γ ⊢ ⋆) -> (Γ ⊢ ⋆)
subTerm : ∀ {Γ} (σ : Sub Γ) -> (Γ ⊢ A) -> (Γ ⊢ subType σ A)
```

