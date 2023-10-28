module Pantograph.Specific.FSTLC where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Eq.Generic (genericEq)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.HeteList ((:), nil)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.StringQuery as StringQuery
import Data.Subtype (inject)
import Data.Tuple (fst)
import Pantograph.Generic.Language ((%.), (%.|))
import Pantograph.Generic.Language as PL
import Pantograph.Library.Language.Change (getDiffChangingRule)
import Pantograph.Library.Language.Edit as LibEdit
import Pantograph.Library.Language.Shallow (buildExprShallowSyntax, buildRuleSortShallowSyntax, buildSortChangeShallowSyntax, buildSortShallowSyntax)
import Text.Pretty (class Pretty, parens, pretty, quotes, (<+>))
import Todo (todo)
import Type.Proxy (Proxy(..))

-- types

type Expr = PL.Expr SN EL
type StepExpr = PL.StepExpr SN EL
type ExprTooth = PL.ExprTooth SN EL
type SortChange = PL.SortChange SN
type RuleSort = PL.RuleSort SN
type Sort = PL.Sort SN
type Edit = PL.Edit SN EL
type Edits = PL.Edits SN EL
type SteppingRule = PL.SteppingRule SN EL
type ChangingRule = PL.ChangingRule SN
type SortingRule = PL.SortingRule SN
type SplitExprPathChanges = LibEdit.SplitExprPathChanges SN EL

-- SN

data SN
  -- StrInner
  = StrInner String
  -- Str
  | Str -- StrInner
  -- Jg
  | VarJg -- Ctx Str Ty Loc
  | TmJg -- Ctx Ty
  | NeJg -- Ctx Ty
  | TyJg -- Ty
  -- Ctx
  | NilCtx
  | ConsCtx -- StrInner Ty Ctx
  -- Ty
  | DataTySN DataTy
  | ArrowTySN -- Ty Ty
  -- Loc
  | LocalLoc
  | NonlocalLoc

derive instance Generic SN _
instance Show SN where show = genericShow
instance Eq SN where eq = genericEq
instance Ord SN where compare = genericCompare

instance TreeNode SN where
  kidsCount = case _ of
    StrInner _ -> 0
    Str -> 1
    VarJg -> 4
    TmJg -> 2
    NeJg -> 2
    TyJg -> 1
    NilCtx -> 0
    ConsCtx -> 3
    DataTySN _ -> 0
    ArrowTySN -> 2
    LocalLoc -> 0
    NonlocalLoc -> 0

instance PrettyTreeNode SN where
  prettyTreeNode sn = 
    let ass = assertValidTreeKids "prettyTreeNode" sn in
    case sn of
      StrInner s -> ass \[] -> s
      Str -> ass \[str] -> quotes str
      VarJg -> ass \[γ, x, α, loc] -> γ <+> "⊢" <+> loc <+> x <+> ":" <+> α
      TmJg -> ass \[γ, α] -> γ <+> "⊢" <+> α
      NeJg -> ass \[γ, α] -> γ <+> "⊢" <+> α
      TyJg -> ass \[α] -> α
      NilCtx -> ass \[] -> "∅"
      ConsCtx -> ass \[x, α, γ] -> parens (x <+> ":" <+> α) <> "," <+> γ
      DataTySN dt -> ass \[] -> pretty dt
      ArrowTySN -> ass \[α, β] -> α <+> "→" <+> β
      LocalLoc -> ass \[] -> "[local]"
      NonlocalLoc -> ass \[] -> "[nonlocal]"

-- DataTy

data DataTy = BoolDataTy

derive instance Generic DataTy _
instance Show DataTy where show = genericShow
instance Eq DataTy where eq = genericEq
instance Ord DataTy where compare = genericCompare

instance Pretty DataTy where
  pretty = case _ of
    BoolDataTy -> "bool"

-- EL

data EL
  -- Str
  = StrEL
  -- Var
  | ZeroVar
  | SucVar -- Var
  | FreeVar
  -- Term
  | LamTm -- Var Ty Term
  | LetTm -- Var Ty Term Term
  | IfTm -- Term Term Term
  | CallTm -- Ne
  | ErrorCallTm -- Ne
  | HoleTm -- Ty
  | ErrorBoundaryTm -- Term
  -- Ne
  | VarNe -- Var
  | AppNe -- Ne Term
  | GrayAppNe -- Ne Term
  -- Ty
  | HoleTy
  | DataTyEL DataTy
  | ArrowTyEL -- Ty Ty
  -- Format
  | Format Format

derive instance Generic EL _
instance Show EL where show = genericShow
instance Eq EL where eq = genericEq
instance Ord EL where compare = genericCompare

instance TreeNode EL where
  kidsCount = case _ of
    StrEL -> 0
    ZeroVar -> 0
    SucVar -> 1
    FreeVar -> 0
    LamTm -> 3
    LetTm -> 4
    VarNe -> 1
    IfTm -> 3
    CallTm -> 1
    ErrorCallTm -> 1
    HoleTm -> 1
    ErrorBoundaryTm -> 1
    AppNe -> 2
    GrayAppNe -> 2
    HoleTy -> 0
    DataTyEL _ -> 0
    ArrowTyEL -> 2
    Format _ -> 1

instance PrettyTreeNode EL where
  prettyTreeNode el =
    let ass = assertValidTreeKids "prettyTreeNode" el in
    case el of
      StrEL -> ass \[] -> "STR"
      ZeroVar -> ass \[] -> "Z"
      SucVar -> ass \[x] -> "S" <> x
      FreeVar -> ass \[] -> "F"
      LamTm -> ass \[x, α, b] -> parens $ "λ" <+> x <+> ":" <+> α <+> "." <+> b
      LetTm -> ass \[x, α, a, b] -> parens $ "let" <+> x <+> ":" <+> α <+> "=" <+> a <+> "in" <+> b
      VarNe -> ass \[x] -> "#" <> x
      IfTm -> ass \[a, b, c] -> parens $ "if" <+> a <+> "then" <+> b <+> "else" <+> c
      CallTm -> ass \[n] -> "Call" <> parens n
      ErrorCallTm -> ass \[n] -> "ErroCall" <> parens n
      HoleTm -> ass \[α] -> parens $ "? : " <> α
      ErrorBoundaryTm -> ass \[a] -> "ErrorBoundaryTm" <> parens a
      AppNe -> ass \[f, a] -> f <+> a
      GrayAppNe -> ass \[f, a] -> f <+> a
      HoleTy -> ass \[] -> "?"
      DataTyEL dt -> ass \[] -> pretty dt
      ArrowTyEL -> ass \[α, β] -> parens $ α <+> "→" <+> β
      Format fmt -> ass \[] -> pretty fmt

-- Format

data Format = Newline

derive instance Generic Format _
instance Show Format where show = genericShow
instance Eq Format where eq = genericEq
instance Ord Format where compare = genericCompare

instance Pretty Format where
  pretty = case _ of
    Newline -> "<newline>"

-- Language

instance PL.Language SN EL where
  getSortingRule el = getSortingRule el
  getChangingRule el = getChangingRule el

  topSort = sr_jg_tm sr_ctx_nil (todo "sort metavar")

  getDefaultExpr = todo "getDefaultExpr"

  steppingRules = steppingRules

  getEdits sr ori = getEdits sr ori

  specialEdits = todo "specialEdits"

  validGyro = todo "validGyro"

getSortingRule :: EL -> SortingRule
-- getSortingRule _ = todo "getSortingRule"
getSortingRule =
  case _ of
    StrEL -> PL.buildSortingRule (Proxy :: Proxy (x::_)) \{x {- : StrInner -}} ->
      []
      /\
      ( rs_str x )

    ZeroVar -> PL.buildSortingRule (Proxy :: Proxy (γ::_, x::_, α::_)) \{γ, x, α} ->
      []
      /\
      ( rs_jg_var γ x α rs_loc_local )

    SucVar -> PL.buildSortingRuleFromStrings ["γ", "x", "α", "y", "β", "loc"] \[γ, x, α, y, β, loc] ->
      [ rs_jg_var γ x α loc ]
      /\
      ( rs_jg_var (rs_ctx_cons y β γ) x α loc )

    FreeVar -> PL.buildSortingRuleFromStrings ["x", "α"] \[x, α] ->
      []
      /\
      ( rs_jg_var rs_ctx_nil x α rs_loc_nonlocal )

    LamTm -> PL.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
      [ rs_str x
      , rs_jg_ty α
      , rs_jg_tm (rs_ctx_cons x α γ) β ]
      /\
      ( rs_jg_tm γ (rs_ty_arrow α β) )

    LetTm -> PL.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
      [ rs_str x
      , rs_jg_ty α
      , rs_jg_tm (rs_ctx_cons x α γ) α
      , rs_jg_tm (rs_ctx_cons x α γ) β
      ]
      /\
      ( rs_jg_tm γ β )

    VarNe -> PL.buildSortingRuleFromStrings ["γ", "x", "α", "loc"] \[γ, x, α, loc] ->
      [ rs_jg_var γ x α loc ]
      /\
      ( rs_jg_ne γ α )

    IfTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_tm γ rs_ty_bool
      , rs_jg_tm γ α ]
      /\
      ( rs_jg_tm γ α )

    CallTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_ne γ α ]
      /\
      ( rs_jg_tm γ α )

    ErrorCallTm -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ α ]
      /\
      ( rs_jg_tm γ β )

    HoleTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_ty α ]
      /\
      ( rs_jg_tm γ α )

    ErrorBoundaryTm -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_tm γ α ]
      /\
      ( rs_jg_tm γ β )

    AppNe -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ (rs_ty_arrow α β)
      , rs_jg_tm γ α ]
      /\
      ( rs_jg_ne γ β )

    GrayAppNe -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ β
      , rs_jg_tm γ α ]
      /\
      ( rs_jg_ne γ β )

    HoleTy -> PL.buildSortingRuleFromStrings ["α"] \[α] ->
      [] 
      /\
      ( rs_jg_ty α )

    DataTyEL dt -> PL.buildSortingRuleFromStrings [] \[] ->
      []
      /\
      ( rs_jg_ty (rs_ty_dt dt) )

    ArrowTyEL -> PL.buildSortingRuleFromStrings ["α", "β"] \[α, β] ->
      [ rs_jg_ty α
      , rs_jg_ty β ]
      /\
      ( rs_jg_ty (rs_ty_arrow α β) )

    Format _ -> PL.buildSortingRuleFromStrings ["a"] \[a] -> 
      []
      /\
      ( a )

getChangingRule :: EL -> ChangingRule
getChangingRule el = getDiffChangingRule {getSortingRule} el

-- BEGIN SteppingRules

steppingRules :: Array SteppingRule
steppingRules = 
  [ insertSucSteppingRule 
  , localBecomesNonlocalSteppingRule
  , removeSuc
  , nonlocalBecomesLocal 
  , passThroughArrow 
  , typeBecomesRhsOfChange
  , wrapLambda
  , unWrapLambda
  , wrapApp
  , unWrapApp
  , makeAppGray
  , removeGrayHoleArg
  , rehydrateApp
  , replaceCallWithError
  , replaceErrorWithCall
  , wrapCallInErrorUp
  , wrapCallInErrorDown
  , removeError
  , mergeErrors
  ]

-- | {e}↓{Var (+ <{ y : β, {> γ <}}>) x α loc}  ~~>  Suc {e}↓{Var γ x α loc}
insertSucSteppingRule :: SteppingRule
insertSucSteppingRule = PL.SteppingRule case _ of
  PL.Boundary (PL.Down /\ (PL.SN VarJg %! [Shift (Plus /\ (PL.SN ConsCtx %- 2 /\ [y, β])) γ, x, α, loc])) e -> Just $
    se_var_suc (epR γ) (epR x) (epR α) y β (epR loc) $
      PL.Boundary (PL.Down /\ InjectChange (PL.SN VarJg) [γ, x, α, loc]) e
  _ -> Nothing


-- | {Zero}↓{Var (- <{ x : α, {> γ <}}>) x α Local} ~~> {Free}↑{Var id x α (Local ~> Nonlocal)}
localBecomesNonlocalSteppingRule :: SteppingRule
localBecomesNonlocalSteppingRule = PL.SteppingRule case _ of
  PL.Down /\ (PL.SN VarJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, x', α', PL.SN LocalLoc %! []]) %.|
  (PL.EN ZeroVar _ _ %. [])
  | true -> Just $
    PL.Boundary 
      (PL.Up /\ (PL.SN VarJg %! [inject (epR γ), x', α', Replace sr_loc_local sr_loc_nonlocal]))
      (inject $ freeVarTerm {γ: (epR γ), x, α})
  _ -> Nothing

-- | {Var (- <{ y : β , {> γ <}}>) x α loc}↓{Suc pred} ~~> {Var γ x α loc}↓{pred}
removeSuc :: SteppingRule
removeSuc = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SN VarJg %! [Minus /\ (PL.SN ConsCtx %- 1 /\ [_y, _β]) %!/ γ, x, α, loc])) %.| (PL.EN SucVar _sigma _ %. [pred]) -> Just $
    PL.Down /\ (PL.SN VarJg %! [γ, x, α, loc]) %.| pred
  _ -> Nothing


-- | {Var (+ <{ x : α , {> γ< } }>) x α Nonlocal}↓{_} ~~> Z
nonlocalBecomesLocal :: SteppingRule
nonlocalBecomesLocal = PL.SteppingRule case _ of
  (PL.Down /\ (Plus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ)) %.| a -> Just $
    se_var_zero (epR γ) x α
  _ -> Nothing


-- | {α! -> β!}↓{α -> β} ~~> {α}↓{α!} -> {β}↓{β!}
passThroughArrow :: SteppingRule
passThroughArrow = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SN ArrowTySN %! [α, β])) %.| (PL.EN ArrowTyEL sigma _ %. [αCh, βCh]) -> Just $
    PL.EN ArrowTyEL sigma {} %. 
      [ PL.Down /\ α %.| αCh
      , PL.Down /\ β %.| βCh ]
  _ -> Nothing

-- | {_ : Type α!}↓{_} ~~> α
typeBecomesRhsOfChange :: SteppingRule
typeBecomesRhsOfChange = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SN TyJg %! [α])) %.| _ -> Just $ inject (typeSortToTypeExpr (epR α))
  _ -> Nothing


-- | {Term γ (+ <{α -> {> β<}}>)}↓{b} ~~> lam ~ : α . {Term (+ <{ ~ : α, {> γ <}}>) β}↓{b}
wrapLambda :: SteppingRule
wrapLambda = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SN TmJg %! [γ, Plus /\ (PL.SN TmJg %- 1 /\ [α]) %!/ β])) %.| b -> Just $
    let x = sr_strInner "" in
    se_tm_lam x α (epR β) (epR γ) (se_str x) (inject (typeSortToTypeExpr α)) b
  _ -> Nothing

-- | {Term γ (- <{α -> {> β <}}>)}↓{lam x : α . b} ~~> {Term (- x : α, γ) β}↓{b}
unWrapLambda :: SteppingRule
unWrapLambda = PL.SteppingRule case _ of
  PL.Down /\ ch %.| (PL.EN LamTm _sigma {} %. [xExpr, _αExpr, b]) -> do
    let x = PL.getExprSort (PL.fromStepExprToExpr xExpr)
    ch' <- case ch of
      PL.SN TmJg %! [γ, PL.SN ArrowTySN %! [α, β]] -> Just $
        PL.SN TmJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, epR α]) %!/ γ, β]
      -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
      PL.SN TmJg %! [γ, (PL.SN ArrowTySN % [α, β]) %!~> mv@(PL.VarSN _ % [])] -> Just $ 
        PL.SN TmJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, β %!~> mv]
      _ -> Nothing
    Just $ PL.Boundary (PL.Down /\ ch') b
  _ -> Nothing


-- | {Term γ (+ α -> β)}↑{f} ~~> {Term γ β}↑{App f (? : α)}
wrapApp :: SteppingRule
wrapApp = PL.SteppingRule case _ of
  PL.Up /\ (PL.SN TmJg %! [γ, Plus /\ (PL.SN ArrowTySN %- (1 /\ [α])) %!/ β]) %.| f -> Just $ 
    PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| (PL.buildStepExpr AppNe {} [f, se_tm_hole α])
  _ -> Nothing


-- | App {Term γ (- α -> β)}↑{b} a ~~> {Term γ β}↑{b}
unWrapApp :: SteppingRule
unWrapApp = PL.SteppingRule case _ of
  PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN TmJg %! [γ, Minus /\ (PL.SN ArrowTySN %- 1 /\ [_α]) %!/ β]) %.| b, _a] -> Just $ 
    PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| b
  _ -> Nothing 


-- | App {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{GrayApp f a}
-- | App {Ne γ ((α -> β) ~> ?delta)}↑{f} a ~~> {Ne γ {β ~> ?delta}}↑{GrayApp f a}
makeAppGray :: SteppingRule
makeAppGray = PL.SteppingRule case _ of
  PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
    PL.Up /\ (PL.SN NeJg %! [γ, β]) %.| PL.buildStepExpr GrayAppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
  -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
  PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, (PL.SN ArrowTySN % [α, β]) %!~> mv@(PL.VarSN _ % [])]) %.| f, a] -> Just $
    PL.Up /\ (PL.SN NeJg %! [γ, β %!~> mv]) %.| PL.buildStepExpr GrayAppNe {γ: epR γ, α, β} [f, a]
  _ -> Nothing 

-- | GrayApp f (? : α) ~~> f
removeGrayHoleArg :: SteppingRule
removeGrayHoleArg = PL.SteppingRule case _ of
  PL.EN GrayAppNe _ _ %. [f, PL.EN HoleTm _ _ %. [_α]] -> Just $ 
    f
  _ -> Nothing


-- | GrayApp {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{App f a}
rehydrateApp :: SteppingRule
rehydrateApp = PL.SteppingRule case _ of
  PL.EN GrayAppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
    PL.Up /\ (PL.SN NeJg %! [γ, β]) %.| PL.buildStepExpr AppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
  _ -> Nothing

-- | {Ne γ! α!}↑{Call n} ~~> {γ, αL, αR}ErrorCall{n}
replaceCallWithError :: SteppingRule
replaceCallWithError = PL.SteppingRule case _ of
  PL.Up /\ (PL.SN NeJg %! [γ, α]) %.| (PL.EN CallTm _ _ %. [n]) -> Just $
    PL.buildStepExpr ErrorCallTm {γ: epR γ, α: epL α, β: epR α} [n]
  _ -> Nothing

-- | {γ, α, α}ErrorCall{n} ~~> Call n
replaceErrorWithCall :: SteppingRule
replaceErrorWithCall = PL.SteppingRule case _ of
  PL.EN ErrorCallTm sigma _ %. [n]
    | γ <- PL.applyRuleSortVarSubst sigma "γ"
    , α <- PL.applyRuleSortVarSubst sigma "α"
    , β <- PL.applyRuleSortVarSubst sigma "β"
    , α == β -> Just $
    PL.buildStepExpr CallTm {γ, α} [n]
  _ -> Nothing


-- | Call ({Ne γ (α -> β)}↑{n}) ~~> {Term γ β}↑{{α -> β, β}ErrorBoundary{Call n}}
wrapCallInErrorUp :: SteppingRule
wrapCallInErrorUp = PL.SteppingRule case _ of
  PL.EN CallTm _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| n] -> Just $
    PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| (PL.buildStepExpr ErrorBoundaryTm {γ: epL γ, α: PL.SN ArrowTySN % [epL α, epL β], β: epR β} [PL.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]])
  _ -> Nothing

-- | {Tm γ! α!}↓{Call n : Tm γL αL} ~~> {γR, αL, αR}ErrorBoundary{Call n : }
wrapCallInErrorDown :: SteppingRule
wrapCallInErrorDown = PL.SteppingRule case _ of
  PL.Down /\ (PL.SN TmJg %! [γ, α]) %.| (PL.EN CallTm _ _ %. [n]) -> Just $
    PL.buildStepExpr ErrorBoundaryTm {γ: epR γ, α: epL α, β: epR α} [PL.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]]
  _ -> Nothing


-- | {γ, α, α}ErrorBoundary{a : Tm γ α} ~~> a
removeError :: SteppingRule
removeError = PL.SteppingRule case _ of
  PL.EN ErrorBoundaryTm sigma _ %. [a] 
    | γ <- PL.applyRuleSortVarSubst sigma "γ" 
    , α <- PL.applyRuleSortVarSubst sigma "α" 
    , β <- PL.applyRuleSortVarSubst sigma "β" 
    , α == β -> Just $
    a
  _ -> Nothing


-- | {γ, β, delta}ErrorBoundary{{γ, α, β}ErrorBoundary{a : Tm γ α}} ~~> {γ, α, delta}ErrorBoundary{a : Tm γ α}
mergeErrors :: SteppingRule
mergeErrors = PL.SteppingRule case _ of
  PL.EN ErrorBoundaryTm sigma1 _ %. [PL.EN ErrorBoundaryTm sigma2 _ %. [a]] -> Just $
    let γ = PL.applyRuleSortVarSubst sigma1 "γ" in
    let α = PL.applyRuleSortVarSubst sigma1 "α" in
    let delta = PL.applyRuleSortVarSubst sigma2 "β" in
    PL.buildStepExpr ErrorBoundaryTm {γ, α, β: delta} [a]
  _ -> Nothing

-- END SteppingRules

-- utils

freeVarTerm {γ, x, α} = case γ of
  Tree (PL.SN ConsCtx) [y, β, γ'] -> 
    sucVar {y, β} $ freeVarTerm {γ: γ', x, α}
  Tree (PL.SN NilCtx) [] ->
    ex.var_free {x, α}
  _ -> bug "impossible: freeVarTerm"

sucVar {y, β} pred 
  | Tree (PL.SN VarJg) [γ, x, α, loc] <- PL.getExprSort pred =
  ex.var_suc {γ, x, α, y, β, loc, pred}
sucVar _ _ = bug "impossible: sucVar"

--

getEdits :: Sort -> Orientation -> Edits
-- -- getEdits (Tree (PL.SortNode (StrInner _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- -- getEdits (Tree (PL.SortNode Str) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- -- getEdits (Tree (PL.SN VarJg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits sr@(Tree (PL.SN TmJg) [γ, β]) Outside = PL.Edits $ StringQuery.fuzzy
--   { toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0
--   , getItems: const
--       [ "lambda" /\
--       let α = sr_freshVar "α" in
--       let x = sr_strInner "" in
--       LibEdit.buildEditsFromExpr {splitExprPathChanges} 
--         (ex_tm_lam x α (sr_freshVar "β") γ 
--           (ex.str {x}) (ex_ty_hole α) (ex_tm_hole β))
--         sr
--     ]
--   }
-- getEdits (Tree (PL.SortNode NeJg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SN TyJg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode (DataTySN _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode ArrowTySN) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits sort orientation = bug $ "invalid cursor position; sort = " <> show sort <> "; orientation = " <> show orientation

splitExprPathChanges :: SplitExprPathChanges
splitExprPathChanges = todo "type change goes up, context change goes down"

-- utility

typeSortToTypeExpr :: Sort -> Expr
typeSortToTypeExpr (PL.SN (DataTySN dt) % []) = PL.buildExpr (DataTyEL dt) {} []
typeSortToTypeExpr (PL.SN ArrowTySN % [α, β]) = PL.buildExpr ArrowTyEL {α, β} [typeSortToTypeExpr α, typeSortToTypeExpr β]
typeSortToTypeExpr sr = bug $ "invalid: " <> show sr

-- strStepExprToStrSort :: StepExpr -> Sort
-- strStepExprToStrSort (_ /\ n@(PL.EN StrEL _ _) %. _)
--   | PL.SortNode (StrInner str) % [] <- PL.getExprNodeSort n = ?a
-- strStepExprToStrSort _ = bug "invalid"

-- shallow

sortNodes = 
  ((Proxy :: Proxy "strInner") /\ \{string} -> PL.makeSort (StrInner string) []) :
  ((Proxy :: Proxy "str") /\ \{strInner} -> PL.makeSort Str [strInner]) :
  nil

sr = buildSortShallowSyntax (Proxy :: Proxy SN) sortNodes
rs = buildRuleSortShallowSyntax (Proxy :: Proxy SN) sortNodes
ch = buildSortChangeShallowSyntax (Proxy :: Proxy SN) sortNodes

-- shallow Sort

sr_freshVar string = PL.freshVarSort string

sr_strInner string = PL.makeSort (StrInner string) []
sr_str strInner = PL.makeSort Str [strInner]

sr_jg_var γ x α loc = PL.makeSort VarJg [γ, x, α, loc]
sr_jg_tm γ α = PL.makeSort TmJg [γ, α]
sr_jg_ne γ α = PL.makeSort NeJg [γ, α]
sr_jg_ty α = PL.makeSort TyJg [α]

sr_ctx_nil = PL.makeSort NilCtx []
sr_ctx_cons γ x α = PL.makeSort ConsCtx [γ, x, α]

sr_ty_dt dt = PL.makeSort DataTySN [dt]
sr_ty_arrow α β = PL.makeSort ArrowTySN [α, β]
sr_ty_bool = PL.makeSort (DataTySN BoolDataTy) []

sr_loc_local = PL.makeSort LocalLoc []
sr_loc_nonlocal = PL.makeSort NonlocalLoc []

-- shallow RuleSort

rs_freshVar var = PL.makeVarRuleSort var

rs_strInner string = PL.makeInjectRuleSort (StrInner string) []
rs_str strInner = PL.makeInjectRuleSort Str [strInner]

rs_jg_var γ x α loc = PL.makeInjectRuleSort VarJg [γ, x, α, loc]
rs_jg_tm γ α = PL.makeInjectRuleSort TmJg [γ, α]
rs_jg_ne γ α = PL.makeInjectRuleSort NeJg [γ, α]
rs_jg_ty α = PL.makeInjectRuleSort TyJg [α]

rs_ctx_nil = PL.makeInjectRuleSort NilCtx []
rs_ctx_cons γ x α = PL.makeInjectRuleSort ConsCtx [γ, x, α]

rs_ty_dt dt = PL.makeInjectRuleSort (DataTySN dt) []
rs_ty_arrow α β = PL.makeInjectRuleSort ArrowTySN [α, β]
rs_ty_bool = PL.makeInjectRuleSort (DataTySN BoolDataTy) []

rs_loc_local = PL.makeInjectRuleSort LocalLoc []
rs_loc_nonlocal = PL.makeInjectRuleSort NonlocalLoc []

-- shallow Expr

ex = buildExprShallowSyntax (Proxy :: Proxy SN) (Proxy :: Proxy ()) exprNodes

exprNodes =  
  ((Proxy :: Proxy "str") /\ \{x} -> PL.buildExpr StrEL {x} []) :
  ((Proxy :: Proxy "var_zero") /\ \{γ, x, α} -> PL.buildExpr ZeroVar {γ, x, α} []) :
  ((Proxy :: Proxy "var_suc") /\ \{γ, x, α, y, β, loc, pred} -> PL.buildExpr SucVar {γ, x, α, y, β, loc} [pred]) :
  ((Proxy :: Proxy "var_free") /\ \{x, α} -> PL.buildExpr FreeVar {x, α} []) :
  nil

-- ex_str x = PL.buildExpr StrEL {x} [] 

-- ex_var_zero γ x α = PL.buildExpr ZeroVar {γ, x, α} []
-- ex_var_suc γ x α y β loc pred = PL.buildExpr SucVar {γ, x, α, y, β, loc} [pred]
-- ex_var_free x α = PL.buildExpr FreeVar {x, α} []

ex_ty_hole α = PL.buildExpr HoleTy {α} []

ex_tm_lam x α β γ xExpr αExpr b = PL.buildExpr LamTm {x, α, β, γ} [xExpr, αExpr, b]
ex_tm_hole α = PL.buildExpr HoleTm {α} []

-- shallow StepExpr

se_str x = PL.buildStepExpr StrEL {x} [] 

se_var_zero γ x α = PL.buildStepExpr SucVar {γ, x, α, loc: sr_loc_nonlocal} []
se_var_suc γ x α y β loc pred = PL.buildStepExpr SucVar {γ, x, α, y, β, loc} [pred]

se_tm_lam x α β γ xExpr αExpr b = PL.buildStepExpr LamTm {x, α, β, γ} [xExpr, αExpr, b]
se_tm_hole α = PL.buildStepExpr HoleTm {α} []
