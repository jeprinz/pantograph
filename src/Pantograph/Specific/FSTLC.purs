module Pantograph.Specific.FSTLC where

import Data.Tree
import Data.Tuple.Nested
import Prelude
import Util

import Bug (bug)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Const (Const(..))
import Data.Eq.Generic (genericEq)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.HeteList ((:), nil)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.StringQuery as StringQuery
import Data.Subtype (inject)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen.HTML as HH
import Pantograph.Generic.Language ((%.), (%.|))
import Pantograph.Generic.Language as PL
import Pantograph.Generic.Rendering as PR
import Pantograph.Generic.Rendering.Html as PH
import Pantograph.Library.Language.Change (getDiffChangingRule)
import Pantograph.Library.Language.Edit as LibEdit
import Pantograph.Library.Language.Shallow (buildExprShallowSyntax, buildRuleSortShallowSyntax, buildSortChangeShallowSyntax, buildSortShallowSyntax)
import Pantograph.Library.Language.Step as LibStep
import Text.Pretty (class Pretty, parens, pretty, quotes, (<+>))
import Todo (todo)
import Type.Proxy (Proxy(..))

-- types

type Expr = PL.Expr SN EL
type ExprNode = PL.ExprNode SN EL
type StepExpr = PL.StepExpr SN EL
type ExprTooth = PL.ExprTooth SN EL
type SortChange = PL.SortChange SN
type RuleSort = PL.RuleSort SN
type Sort = PL.Sort SN
type Edit = PL.Edit SN EL
type AnnExprGyro er = PL.AnnExprGyro SN EL er
type AnnExprCursor er = PL.AnnExprCursor SN EL er
type Edits = PL.Edits SN EL
type SpecialEdits = PL.SpecialEdits SN EL
type ExprGyro = PL.ExprGyro SN EL
type SteppingRule = PL.SteppingRule SN EL
type ChangingRule = PL.ChangingRule SN
type SortingRule = PL.SortingRule SN
type SplitChange = LibEdit.SplitChange SN
type AnnExprNode er = PL.AnnExprNode SN EL er
type RenderM a = PR.RenderM SN EL CTX ENV a
type ArrangeKid a = PR.ArrangeKid SN EL a

-- SN

type StrInner = Sort
type Str = Sort
type Jg = Sort
type Ctx = Sort
type Ty = Sort
type Loc = Sort

type StrInnerR = RuleSort
type StrR = RuleSort
type JgR = RuleSort
type CtxR = RuleSort
type TyR = RuleSort
type LocR = RuleSort

data SN
  -- StrInner
  = StrInner String
  -- Str
  | Str -- StrInner
  -- Jg
  | VarJg -- Ctx StrInner Ty Loc
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

data Format = Newline | Indent

derive instance Generic Format _
instance Show Format where show = genericShow
instance Eq Format where eq = genericEq
instance Ord Format where compare = genericCompare
instance Pretty Format where
  pretty = case _ of
    Newline -> "<newline>"
    Indent -> "<indent>"

-- Language

instance PL.Language SN EL where
  getSortingRule = todo ""
  getChangingRule = todo ""
  topSort = todo ""
  getDefaultExpr = todo ""
  steppingRules = todo ""
  getEditsAtSort = todo ""
  specialEdits = todo ""
  validGyro gyro = todo ""

-- instance PL.Language SN EL where
--   getSortingRule el = getSortingRule el
--   getChangingRule el = getChangingRule el
--   topSort = topSort
--   getDefaultExpr sr = getDefaultExpr sr
--   steppingRules = steppingRules
--   getEditsAtSort sr ori = getEditsAtSort sr ori
--   specialEdits = specialEdits
--   validGyro gyro = validGyro gyro

-- topSort :: Sort
-- topSort = sr_jg_tm sr_ctx_nil (sr_freshVar "top")

-- getDefaultExpr :: Sort -> Maybe Expr
-- getDefaultExpr (PL.SN Str % [strInner]) = Just $ PL.buildExpr StrEL {x: strInner} []
-- getDefaultExpr (PL.SN TmJg % [γ, α]) = Just $ PL.buildExpr HoleTm {γ, α} [fromTypeSortToTypeExpr α]
-- getDefaultExpr (PL.SN TyJg % [α]) = Just $ fromTypeSortToTypeExpr α
-- getDefaultExpr _ = Nothing

-- specialEdits :: SpecialEdits
-- specialEdits = 
--   { deleteExpr: case _ of
--       -- when you delete a type-Expr, you need to push an outward change that
--       -- replaces the type-sort that is reflected in the type-Expr's sort
--       -- (otherwise the type-Expr would just be filled to correspond to its
--       -- type-Sort again)
--       PL.SN TyJg % [alpha] -> Just $ LibEdit.makeOuterChangeEdit $ PL.SN TyJg %! [alpha %!~> PL.freshVarSort "deleted"]
--       -- when you delete a string-Expr, you need to push an outward change the
--       -- replaces the StrInner-sort with the empty-string StrInner-sort
--       PL.SN Str % [strInner] -> Just $ LibEdit.makeOuterChangeEdit $ PL.SN Str %! [strInner %!~> (PL.SN (StrInner "") % [])]
--       _ -> Nothing
--   , copyExpr: const Nothing
--   , deleteExprPath: \ch -> Just $ LibEdit.makeOuterChangeEdit ch
--   , copyExprPath: const Nothing
--   -- TODO: 'enter' makes a newline
--   , enter: const Nothing
--   -- TODO: 'tab' makes an indentation
--   , tab: const Nothing
--   }

-- validGyro :: forall er. AnnExprGyro er -> Boolean
-- validGyro (RootGyro e) | PL.SN TmJg % [γ, α] <- PL.getExprSort e = true
-- validGyro (CursorGyro (Cursor cursor)) | PL.SN TmJg % [γ, α] <- PL.getExprSort cursor.inside = true
-- validGyro (SelectGyro (Select select)) | PL.SN TmJg % [γ, α] <- PL.getExprSort select.inside = true
-- validGyro _ = false

-- getSortingRule :: EL -> SortingRule
-- getSortingRule =
--   case _ of
--     StrEL -> PL.buildSortingRule (Proxy :: Proxy (x::StrInnerR)) \{x} ->
--       []
--       /\
--       ( rs_str x )

--     ZeroVar -> PL.buildSortingRule (Proxy :: Proxy (γ::CtxR, x::StrInnerR, α::TyR)) \{γ, x, α} ->
--       []
--       /\
--       ( rs_jg_var γ x α rs_loc_local )

--     SucVar -> PL.buildSortingRule (Proxy :: Proxy (γ::CtxR, x::StrInnerR, α::TyR, y::CtxR, β::TyR, loc::LocR)) \{γ, x, α, y, β, loc} ->
--       [ rs_jg_var γ x α loc ]
--       /\
--       ( rs_jg_var (rs_ctx_cons y β γ) x α loc )

--     FreeVar -> PL.buildSortingRuleFromStrings ["x", "α"] \[x, α] ->
--       []
--       /\
--       ( rs_jg_var rs_ctx_nil x α rs_loc_nonlocal )

--     LamTm -> PL.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
--       [ rs_str x
--       , rs_jg_ty α
--       , rs_jg_tm (rs_ctx_cons x α γ) β ]
--       /\
--       ( rs_jg_tm γ (rs_ty_arrow α β) )

--     LetTm -> PL.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
--       [ rs_str x
--       , rs_jg_ty α
--       , rs_jg_tm (rs_ctx_cons x α γ) α
--       , rs_jg_tm (rs_ctx_cons x α γ) β
--       ]
--       /\
--       ( rs_jg_tm γ β )

--     VarNe -> PL.buildSortingRuleFromStrings ["γ", "x", "α", "loc"] \[γ, x, α, loc] ->
--       [ rs_jg_var γ x α loc ]
--       /\
--       ( rs_jg_ne γ α )

--     IfTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
--       [ rs_jg_tm γ rs_ty_bool
--       , rs_jg_tm γ α ]
--       /\
--       ( rs_jg_tm γ α )

--     CallTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
--       [ rs_jg_ne γ α ]
--       /\
--       ( rs_jg_tm γ α )

--     ErrorCallTm -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
--       [ rs_jg_ne γ α ]
--       /\
--       ( rs_jg_tm γ β )

--     HoleTm -> PL.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
--       [ rs_jg_ty α ]
--       /\
--       ( rs_jg_tm γ α )

--     ErrorBoundaryTm -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
--       [ rs_jg_tm γ α ]
--       /\
--       ( rs_jg_tm γ β )

--     AppNe -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
--       [ rs_jg_ne γ (rs_ty_arrow α β)
--       , rs_jg_tm γ α ]
--       /\
--       ( rs_jg_ne γ β )

--     GrayAppNe -> PL.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
--       [ rs_jg_ne γ β
--       , rs_jg_tm γ α ]
--       /\
--       ( rs_jg_ne γ β )

--     HoleTy -> PL.buildSortingRuleFromStrings ["α"] \[α] ->
--       [] 
--       /\
--       ( rs_jg_ty α )

--     DataTyEL dt -> PL.buildSortingRuleFromStrings [] \[] ->
--       []
--       /\
--       ( rs_jg_ty (rs_ty_dt dt) )

--     ArrowTyEL -> PL.buildSortingRuleFromStrings ["α", "β"] \[α, β] ->
--       [ rs_jg_ty α
--       , rs_jg_ty β ]
--       /\
--       ( rs_jg_ty (rs_ty_arrow α β) )

--     Format _ -> PL.buildSortingRuleFromStrings ["a"] \[a] -> 
--       []
--       /\
--       ( a )

-- getChangingRule :: EL -> ChangingRule
-- getChangingRule el = getDiffChangingRule {getSortingRule} el

-- steppingRules :: Array SteppingRule
-- steppingRules =
--   [ localBecomesNonlocal
--   , nonlocalBecomesLocal
--   , insertSuc
--   , removeSuc
--   , passThroughArrow
--   , typeBecomesRhsOfChange
--   , wrapLambda
--   , unWrapLambda
--   , rehydrateApp
--   , wrapApp
--   -- , unWrapApp
--   , makeAppGray
--   , wrapCallInErrorUp
--   , wrapCallInErrorDown
--   , removeError
--   , mergeErrors
--   , LibStep.makeDefaultDownSteppingRule {getChangingRule}
--   , LibStep.unless isUpInCall $ LibStep.makeDefaultUpSteppingRule {getChangingRule}
--   ]
--   where
--   -- {e}↓{Var (+ <{ y : β, {> γ <}}>) x α loc}  ~~>  Suc {e}↓{Var γ x α loc}
--   insertSuc = PL.SteppingRule case _ of
--     PL.Boundary (PL.Down /\ (PL.SN VarJg %! [Shift (Plus /\ (PL.SN ConsCtx %- 2 /\ [y, β])) γ, x, α, loc])) e -> Just $
--       se_var_suc (epR γ) (epR x) (epR α) y β (epR loc) $
--         PL.Boundary (PL.Down /\ InjectChange (PL.SN VarJg) [γ, x, α, loc]) e
--     _ -> Nothing

--   -- {Zero}↓{Var (- <{ x : α, {> γ <}}>) x α Local} ~~> {Free}↑{Var id x α (Local ~> Nonlocal)}
--   localBecomesNonlocal = PL.SteppingRule case _ of
--     PL.Down /\ (PL.SN VarJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, x', α', PL.SN LocalLoc %! []]) %.|
--     (PL.EN ZeroVar _ _ %. [])
--     | true -> Just $
--       PL.Boundary 
--         (PL.Up /\ (PL.SN VarJg %! [injectTreeIntoChange (epR γ), x', α', Replace sr_loc_local sr_loc_nonlocal]))
--         (inject $ freeVarTerm {γ: (epR γ), x, α})
--     _ -> Nothing

--   -- {Var (- <{ y : β , {> γ <}}>) x α loc}↓{Suc pred} ~~> {Var γ x α loc}↓{pred}
--   removeSuc = PL.SteppingRule case _ of
--     (PL.Down /\ (PL.SN VarJg %! [Minus /\ (PL.SN ConsCtx %- 1 /\ [_y, _β]) %!/ γ, x, α, loc])) %.| (PL.EN SucVar _sigma _ %. [pred]) -> Just $
--       PL.Down /\ (PL.SN VarJg %! [γ, x, α, loc]) %.| pred
--     _ -> Nothing

--   -- {Var (+ <{ x : α , {> γ< } }>) x α Nonlocal}↓{_} ~~> Z
--   nonlocalBecomesLocal = PL.SteppingRule case _ of
--     (PL.Down /\ (Plus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ)) %.| _ -> Just $
--       se_var_zero (epR γ) x α
--     _ -> Nothing

--   -- {α! -> β!}↓{α -> β} ~~> {α}↓{α!} -> {β}↓{β!}
--   passThroughArrow = PL.SteppingRule case _ of
--     (PL.Down /\ (PL.SN ArrowTySN %! [α, β])) %.| (PL.EN ArrowTyEL sigma _ %. [αCh, βCh]) -> Just $
--       PL.EN ArrowTyEL sigma {} %. 
--         [ PL.Down /\ α %.| αCh
--         , PL.Down /\ β %.| βCh ]
--     _ -> Nothing

--   -- {_ : Type α!}↓{_} ~~> α
--   typeBecomesRhsOfChange = PL.SteppingRule case _ of
--     (PL.Down /\ (PL.SN TyJg %! [α])) %.| _ -> Just $ inject (fromTypeSortToTypeExpr (epR α))
--     _ -> Nothing

--   -- {Term γ (+ <{α -> {> β<}}>)}↓{b} ~~> lam ~ : α . {Term (+ <{ ~ : α, {> γ <}}>) β}↓{b}
--   wrapLambda = PL.SteppingRule case _ of
--     (PL.Down /\ (PL.SN TmJg %! [γ, Plus /\ (PL.SN TmJg %- 1 /\ [α]) %!/ β])) %.| b -> Just $
--       let x = sr_strInner "" in
--       se_tm_lam x α (epR β) (epR γ) (se_str x) (inject (fromTypeSortToTypeExpr α)) b
--     _ -> Nothing

--   -- {Term γ (- <{α -> {> β <}}>)}↓{lam x : α . b} ~~> {Term (- x : α, γ) β}↓{b}
--   unWrapLambda = PL.SteppingRule case _ of
--     PL.Down /\ ch %.| (PL.EN LamTm _sigma {} %. [xExpr, _αExpr, b]) -> do
--       let x = PL.getExprSort (PL.fromStepExprToExpr xExpr)
--       ch' <- case ch of
--         PL.SN TmJg %! [γ, PL.SN ArrowTySN %! [α, β]] -> Just $
--           PL.SN TmJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, epR α]) %!/ γ, β]
--         -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
--         PL.SN TmJg %! [γ, (PL.SN ArrowTySN % [α, β]) %!~> mv@(PL.VarSN _ % [])] -> Just $ 
--           PL.SN TmJg %! [Minus /\ (PL.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, β %!~> mv]
--         _ -> Nothing
--       Just $ PL.Boundary (PL.Down /\ ch') b
--     _ -> Nothing

--   -- {Term γ (+ α -> β)}↑{f} ~~> {Term γ β}↑{App f (? : α)}
--   wrapApp = PL.SteppingRule case _ of
--     PL.Up /\ (PL.SN TmJg %! [γ, Plus /\ (PL.SN ArrowTySN %- (1 /\ [α])) %!/ β]) %.| f -> Just $ 
--       PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| (PL.buildStepExpr AppNe {} [f, se_tm_hole α])
--     _ -> Nothing

--   -- App {Term γ (- α -> β)}↑{b} a ~~> {Term γ β}↑{b}
--   unWrapApp = PL.SteppingRule case _ of
--     PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN TmJg %! [γ, Minus /\ (PL.SN ArrowTySN %- 1 /\ [_α]) %!/ β]) %.| b, _a] -> Just $ 
--       PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| b
--     _ -> Nothing 

--   -- App {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{GrayApp f a}
--   -- App {Ne γ ((α -> β) ~> ?delta)}↑{f} a ~~> {Ne γ {β ~> ?delta}}↑{GrayApp f a}
--   makeAppGray = PL.SteppingRule case _ of
--     PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
--       PL.Up /\ (PL.SN NeJg %! [γ, β]) %.| PL.buildStepExpr GrayAppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
--     -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
--     PL.EN AppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, (PL.SN ArrowTySN % [α, β]) %!~> mv@(PL.VarSN _ % [])]) %.| f, a] -> Just $
--       PL.Up /\ (PL.SN NeJg %! [γ, β %!~> mv]) %.| PL.buildStepExpr GrayAppNe {γ: epR γ, α, β} [f, a]
--     _ -> Nothing 

--   -- GrayApp f (? : α) ~~> f
--   removeGrayHoleArg = PL.SteppingRule case _ of
--     PL.EN GrayAppNe _ _ %. [f, PL.EN HoleTm _ _ %. [_α]] -> Just $ 
--       f
--     _ -> Nothing

--   -- GrayApp {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{App f a}
--   rehydrateApp = PL.SteppingRule case _ of
--     PL.EN GrayAppNe _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
--       PL.Up /\ (PL.SN NeJg %! [γ, β]) %.| PL.buildStepExpr AppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
--     _ -> Nothing

--   -- {Ne γ! α!}↑{Call n} ~~> {γ, αL, αR}ErrorCall{n}
--   replaceCallWithError = PL.SteppingRule case _ of
--     PL.Up /\ (PL.SN NeJg %! [γ, α]) %.| (PL.EN CallTm _ _ %. [n]) -> Just $
--       PL.buildStepExpr ErrorCallTm {γ: epR γ, α: epL α, β: epR α} [n]
--     _ -> Nothing

--   -- {γ, α, α}ErrorCall{n} ~~> Call n
--   replaceErrorWithCall = PL.SteppingRule case _ of
--     PL.EN ErrorCallTm sigma _ %. [n]
--       | γ <- PL.applyRuleSortVarSubst sigma "γ"
--       , α <- PL.applyRuleSortVarSubst sigma "α"
--       , β <- PL.applyRuleSortVarSubst sigma "β"
--       , α == β -> Just $
--       PL.buildStepExpr CallTm {γ, α} [n]
--     _ -> Nothing

--   -- Call ({Ne γ (α -> β)}↑{n}) ~~> {Term γ β}↑{{α -> β, β}ErrorBoundary{Call n}}
--   wrapCallInErrorUp = PL.SteppingRule case _ of
--     PL.EN CallTm _ _ %. [PL.Up /\ (PL.SN NeJg %! [γ, PL.SN ArrowTySN %! [α, β]]) %.| n] -> Just $
--       PL.Up /\ (PL.SN TmJg %! [γ, β]) %.| (PL.buildStepExpr ErrorBoundaryTm {γ: epL γ, α: PL.SN ArrowTySN % [epL α, epL β], β: epR β} [PL.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]])
--     _ -> Nothing

--   -- {Tm γ! α!}↓{Call n : Tm γL αL} ~~> {γR, αL, αR}ErrorBoundary{Call n : }
--   wrapCallInErrorDown = PL.SteppingRule case _ of
--     PL.Down /\ (PL.SN TmJg %! [γ, α]) %.| (PL.EN CallTm _ _ %. [n]) -> Just $
--       PL.buildStepExpr ErrorBoundaryTm {γ: epR γ, α: epL α, β: epR α} [PL.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]]
--     _ -> Nothing

--   -- {γ, α, α}ErrorBoundary{a : Tm γ α} ~~> a
--   removeError = PL.SteppingRule case _ of
--     PL.EN ErrorBoundaryTm sigma _ %. [a] 
--       | γ <- PL.applyRuleSortVarSubst sigma "γ" 
--       , α <- PL.applyRuleSortVarSubst sigma "α" 
--       , β <- PL.applyRuleSortVarSubst sigma "β" 
--       , α == β -> Just $
--       a
--     _ -> Nothing

--   -- {γ, β, delta}ErrorBoundary{{γ, α, β}ErrorBoundary{a : Tm γ α}} ~~> {γ, α, delta}ErrorBoundary{a : Tm γ α}
--   mergeErrors = PL.SteppingRule case _ of
--     PL.EN ErrorBoundaryTm sigma1 _ %. [PL.EN ErrorBoundaryTm sigma2 _ %. [a]] -> Just $
--       let γ = PL.applyRuleSortVarSubst sigma1 "γ" in
--       let α = PL.applyRuleSortVarSubst sigma1 "α" in
--       let delta = PL.applyRuleSortVarSubst sigma2 "β" in
--       PL.buildStepExpr ErrorBoundaryTm {γ, α, β: delta} [a]
--     _ -> Nothing

-- getEditsAtSort :: Sort -> Orientation -> Edits
-- getEditsAtSort (Tree (PL.SN Str) []) Outside = PL.Edits $ StringQuery.fuzzy 
--   { toString: fst, maxPenalty
--   , getItems: \string -> 
--       [ Tuple string $
--         NonEmptyArray.singleton $
--           LibEdit.makeInsideChangeEdit $ ex_str (sr_strInner string) ]
--   }
-- getEditsAtSort (Tree (PL.SN VarJg) []) Outside = PL.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
-- getEditsAtSort (Tree (PL.SN TmJg) [γ0, α0]) Outside = PL.Edits $ StringQuery.fuzzy
--   { toString: fst, maxPenalty
--   , getItems: const
--       [ -- (b :: Tm γ β) ~~> (λ (x="" : ?α) (b :: Tm (x : ?α , γ) β) :: Tm γ (?α -> β))
--         Tuple "lambda" do
--         let γ = γ0
--         let β = α0
--         let α = sr_freshVar "α"
--         let x = sr_strInner ""
--         let xEx = ex_str x
--         let αEx = fromTypeSortToTypeExpr α
--         NonEmptyArray.singleton $ LibEdit.buildEditFromExprNonEmptyPath {splitExprPathChanges} $ PL.singletonExprNonEmptyPath $
--           PL.buildExprTooth LamTm {γ, x, α, β} [xEx, αEx] []
--     ]
--   }
-- getEditsAtSort (Tree (PL.SN NeJg) []) Outside = PL.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
-- getEditsAtSort (Tree (PL.SN TyJg) []) Outside = PL.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
-- getEditsAtSort sort orientation = bug $ "invalid cursor position; sort = " <> show sort <> "; orientation = " <> show orientation

-- maxPenalty :: Fuzzy.Distance
-- maxPenalty = Fuzzy.Distance 1 0 0 0 0 0

-- Renderer

type CTX = ()

type ENV = ()

instance PR.Rendering SN EL CTX ENV where
  topCtx = Proxy /\ topCtx
  topEnv = Proxy /\ topEnv
  arrangeExpr = todo ""
  getBeginsLine = todo ""
  getInitialQuery = todo ""

topCtx :: Record CTX
topCtx = {}

topEnv :: Record ENV
topEnv = {}

arrangeExpr :: forall er a. AnnExprNode er -> Array (RenderM (a /\ AnnExprNode er)) -> RenderM (Array (ArrangeKid a))

arrangeExpr node@(PL.EN StrEL _ _) [] | PL.SN Str % [PL.SN (StrInner string) % []] <- PL.getExprNodeSort node = do
  pure $ Array.fromFoldable $ string ⊕ Nil

arrangeExpr node@(PL.EN ZeroVar _ _) [] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  pure $ Array.fromFoldable $ "Z" ⊕ Nil

arrangeExpr node@(PL.EN SucVar _ _) [x] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  x /\ _ <- x
  pure $ Array.fromFoldable $ "S" ⊕ x ~⊕ Nil

arrangeExpr node@(PL.EN FreeVar _ _) [] | PL.SN _ % _ <- PL.getExprNodeSort node =
  pure $ Array.fromFoldable $ "F" ⊕ Nil

arrangeExpr node@(PL.EN LamTm _ _) [x, α, b] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  x /\ _ <- x
  α /\ _ <- α
  b /\ _ <- b
  pure $ Array.fromFoldable $ "λ " ⊕ x ~⊕ " : " ⊕ α ~⊕ " . " ⊕ b ~⊕ Nil

arrangeExpr node@(PL.EN LetTm _ _) [x, alpha, a, b] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  x /\ _ <- x
  α /\ _ <- alpha
  a /\ _ <- a
  b /\ _ <- b
  pure $ Array.fromFoldable $ "let " ⊕ x ~⊕ " : " ⊕ α ~⊕ " = " ⊕ a ~⊕ " in " ⊕ b ~⊕ Nil

arrangeExpr node@(PL.EN IfTm _ _) [a, b, c] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  a /\ _ <- a
  b /\ _ <- b
  c /\ _ <- c
  pure $ Array.fromFoldable $ "if " ⊕ a ~⊕ " then " ⊕ b ~⊕ " else " ⊕ c ~⊕ Nil

arrangeExpr node@(PL.EN CallTm _ _) [ne] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  ne /\ _ <- ne
  pure $ Array.fromFoldable $ ne ~⊕ Nil

arrangeExpr node@(PL.EN ErrorCallTm _ _) [ne] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  ne /\ _ <- ne
  pure $ Array.fromFoldable $ "ErrorCall " ⊕ ne ~⊕ Nil

arrangeExpr node@(PL.EN HoleTm _ _) [α] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  α /\ _ <- α
  pure $ Array.fromFoldable $ "(? : " ⊕ α ~⊕ ")" ⊕ Nil

arrangeExpr node@(PL.EN ErrorBoundaryTm _ _) [a] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  a /\ _ <- a
  pure $ Array.fromFoldable $ "ErrorBoundary " ⊕ a ~⊕ Nil

arrangeExpr node@(PL.EN VarNe _ _) [_x] | PL.SN VarJg % [γ, PL.SN (StrInner string) % [], α, PL.SN LocalLoc % []] <- PL.getExprNodeSort node = 
  pure $ Array.fromFoldable $ "#" ⊕ string ⊕ Nil

arrangeExpr node@(PL.EN VarNe _ _) [x] | PL.SN VarJg % [γ, PL.SN (StrInner string) % [], α, PL.SN NonlocalLoc % []] <- PL.getExprNodeSort node = 
  pure $ Array.fromFoldable $ "Nonlocal#" ⊕ string ⊕ Nil

arrangeExpr node@(PL.EN AppNe _ _) [f, a] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  f /\ _ <- f
  a /\ aNode <- a
  pure $ Array.fromFoldable $
    if argRequiresParens aNode
      then f ~⊕ " " ⊕ "(" ⊕ a ~⊕ ")" ⊕ Nil
      else f ~⊕ " " ⊕ a ~⊕ Nil
  where
  argRequiresParens :: AnnExprNode er -> Boolean
  argRequiresParens _ = false

arrangeExpr node@(PL.EN GrayAppNe _ _) [f, a] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  f /\ _ <- f
  a /\ _ <- a
  pure $ Array.fromFoldable $ f ~⊕ " " ⊕ "{" ⊕ a ~⊕ "}" ⊕ Nil

arrangeExpr node@(PL.EN HoleTy _ _) [] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  pure $ Array.fromFoldable $ "?" ⊕ Nil

arrangeExpr node@(PL.EN (DataTyEL dt) _ _) [] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  pure $ Array.fromFoldable $ pretty dt ⊕ Nil

arrangeExpr node@(PL.EN ArrowTyEL _ _) [alpha, beta] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  alpha /\ alphaNode <- alpha
  beta /\ _ <- beta
  pure $ Array.fromFoldable $
    if domainRequiresParens alphaNode
      then "(" ⊕ alpha ~⊕ ")" ⊕ " -> " ⊕ beta ~⊕ Nil
      else alpha ~⊕ " -> " ⊕ beta ~⊕ Nil
  where
  domainRequiresParens :: AnnExprNode er -> Boolean
  domainRequiresParens (PL.EN ArrowTyEL _ _) = true
  domainRequiresParens _ = false

arrangeExpr node@(PL.EN (Format fmt) _ _) [a] | PL.SN _ % _ <- PL.getExprNodeSort node = do
  a /\ _ <- a
  pure $ Array.fromFoldable $ fmt ⊕ a ~⊕ Nil

arrangeExpr node mkids = do
  kidNodes <- snd <$$> sequence mkids
  bug $ "invalid; node = " <> pretty node <> "; kidNodes = " <> pretty kidNodes

class Arrangable f where
  arrange :: f ~> ArrangeKid

instance Arrangable Identity where
  arrange (Identity a) = PR.ArrangeKid a
instance Arrangable (Const String) where
  arrange (Const string) = PR.ArrangeHtml [HH.span_ [HH.text string]]
instance Arrangable (Const Format) where 
  arrange (Const Newline) = PR.ArrangeHtml [PH.whitespace " ↪", HH.br_]
  arrange (Const Indent) = PR.ArrangeHtml [PH.whitespace "⇥ "]

consArrangable :: forall f a. Arrangable f => f a -> List (ArrangeKid a) -> List (ArrangeKid a)
consArrangable a aks = Cons (arrange a) aks

consIdentityArrangable :: forall a. a -> List (ArrangeKid a) -> List (ArrangeKid a)
consIdentityArrangable a = consArrangable (Identity a)

consConstArrangable :: forall a b. Arrangable (Const a) => a -> List (ArrangeKid b) -> List (ArrangeKid b)
consConstArrangable a = consArrangable (Const a)

infixr 6 consIdentityArrangable as ~⊕
infixr 6 consConstArrangable as ⊕

getBeginsLine :: forall er. AnnExprCursor er -> Boolean
getBeginsLine (Cursor {inside: PL.EN (Format Newline) _ _ % _}) = true
getBeginsLine _ = false

getInitialQuery :: forall er. AnnExprCursor er -> String
getInitialQuery (Cursor {inside: e@(PL.EN StrEL _ _ % _)}) | PL.SN Str % [PL.SN (StrInner string) % []] <- PL.getExprSort e = string
getInitialQuery _ = ""

-- utilities

splitExprPathChanges :: SplitChange
splitExprPathChanges (PL.SN TmJg %! [γ, α]) = 
-- | type change goes up/out, context change goes down/in
  { outerChange: PL.SN TmJg %! [injectTreeIntoChange $ epR γ, α]
  , innerChange: PL.SN TmJg %! [γ, injectTreeIntoChange $ epL α] }
-- | type change goes up/out, context change goes down/in
splitExprPathChanges (PL.SN NeJg %! [γ, α]) =
  { outerChange: PL.SN NeJg %! [injectTreeIntoChange $ epR γ, α]
  , innerChange: PL.SN NeJg %! [γ, injectTreeIntoChange $ epL α] }
-- | type change goes up/out
splitExprPathChanges ch@(PL.SN TyJg %! [_α]) =
  { outerChange: ch
  , innerChange: injectTreeIntoChange $ epL ch }
-- | meta var change goes up/out and down/in
splitExprPathChanges ch@(PL.VarSN _ %! []) =
  { outerChange: ch 
  , innerChange: ch }
splitExprPathChanges ch = bug $ "invalid: " <> pretty ch

freeVarTerm :: {x :: Sort, α :: Sort, γ :: Sort} -> Expr
freeVarTerm {γ, x, α} = case γ of
  Tree (PL.SN ConsCtx) [y, β, γ'] -> sucVar {y, β, pred: freeVarTerm {γ: γ', x, α}}
  Tree (PL.SN NilCtx) [] -> ex_var_free x α
  _ -> bug "impossible"

sucVar :: {y :: Sort, β :: Sort, pred :: Expr} -> Expr
sucVar {y, β, pred} | PL.SN VarJg % [γ, x, α, loc] <- PL.getExprSort pred = ex_var_suc γ x α y β loc pred
sucVar _ = bug "impossible"

-- | This is necessary because the wrapApp rule conflicts with the `defaultUp`,
-- | and the priority order of the list isn't enough because `defaultUp` happens
-- | on a term higher up in the tree.
isUpInCall :: StepExpr -> Boolean
isUpInCall (PL.EN CallTm _ _ %. [PL.Up /\ (PL.SN NeJg %! [_γ, PL.SN ArrowTySN %! [_α, _beta]]) %.| _]) = true
isUpInCall _ = false

fromTypeSortToTypeExpr :: Sort -> Expr
fromTypeSortToTypeExpr (PL.SN (DataTySN dt) % []) = PL.buildExpr (DataTyEL dt) {} []
fromTypeSortToTypeExpr (PL.SN ArrowTySN % [α, β]) = PL.buildExpr ArrowTyEL {α, β} [fromTypeSortToTypeExpr α, fromTypeSortToTypeExpr β]
fromTypeSortToTypeExpr α@(PL.VarSN x % []) = PL.buildExpr HoleTy {α} []
fromTypeSortToTypeExpr sr = bug $ "invalid: " <> show sr

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

-- ex = buildExprShallowSyntax (Proxy :: Proxy SN) (Proxy :: Proxy ()) exprNodes

-- exprNodes =  
--   ((Proxy :: Proxy "str") /\ \{x} -> PL.buildExpr StrEL {x} []) :
--   ((Proxy :: Proxy "var_zero") /\ \{γ, x, α} -> PL.buildExpr ZeroVar {γ, x, α} []) :
--   ((Proxy :: Proxy "var_suc") /\ \{γ, x, α, y, β, loc, pred} -> PL.buildExpr SucVar {γ, x, α, y, β, loc} [pred]) :
--   ((Proxy :: Proxy "var_free") /\ \{x, α} -> PL.buildExpr FreeVar {x, α} []) :
--   nil

ex_str x = PL.buildExpr StrEL {x} [] 

ex_var_zero γ x α = PL.buildExpr ZeroVar {γ, x, α} []
ex_var_suc γ x α y β loc pred = PL.buildExpr SucVar {γ, x, α, y, β, loc} [pred]
ex_var_free x α = PL.buildExpr FreeVar {x, α} []

ex_ty_hole α = PL.buildExpr HoleTy {α} []

ex_tm_lam {γ, x, α, β, xEx, αEx, b} = PL.buildExpr LamTm {γ, x, α, β} [xEx, αEx, b]
ex_tm_hole α = PL.buildExpr HoleTm {α} []

-- shallow StepExpr

se_str x = PL.buildStepExpr StrEL {x} [] 

se_var_zero γ x α = PL.buildStepExpr SucVar {γ, x, α, loc: sr_loc_nonlocal} []
se_var_suc γ x α y β loc pred = PL.buildStepExpr SucVar {γ, x, α, y, β, loc} [pred]

se_tm_lam x α β γ xExpr αExpr b = PL.buildStepExpr LamTm {x, α, β, γ} [xExpr, αExpr, b]
se_tm_hole α = PL.buildStepExpr HoleTm {α} []
