```
Δγ :: Change γ₀ γ₁
Δβ :: Change β₀ β₁
s  :: Sort(StrInner)
α  :: Sort(Ty)
β  :: Sort(Ty)

x  :: Str s
a  :: Tm (x : α, γ₀) α
A :: Ty α
b  :: Tm (x : α, γ₀) β₀

↓{{ let x = a : A in b | Tm Δγ Δβ }}
```

All RuleSortVars that are not concretized by the change are injected when
figuring the changes that go on the kids' boundaries. For example, `Δα` and
`Δs`.

```
Δγ := Δγ
Δs  := inject s
Δα  := inject α
Δβ := Δβ

Δx  := Str Δs
Δa  := Tm (Δx : Δα, Δγ) Δα
ΔA := Ty Δα
Δb  := Tm (Δx : Δα, Δγ) Δβ

let ↓{{ x | Δx }} = ↓{{ a | Δa }} : ↓{{ A | ΔA }} in ↓{{ b | Δb }}
```