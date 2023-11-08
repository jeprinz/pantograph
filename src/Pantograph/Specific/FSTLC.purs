module Pantograph.Specific.FSTLC where

import Prelude

import Control.Monad.Reader (asks, local)
import Control.Monad.State (modify)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Display (Html, display)
import Data.Enum as Enum
import Data.Eq.Generic (genericEq)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Lazy (defer, force)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as String
import Data.StringQuery as StringQuery
import Data.Supertype (inject)
import Data.Supertype as Supertype
import Data.Traversable (sequence)
import Data.Tree (class DisplayTreeNode, class PrettyTreeNode, class TreeNode, Change(..), Cursor(..), Gyro(..), Orientation(..), Select(..), ShiftSign(..), Tree(..), assertValidTreeKids, epL, epR, invert, singletonNonEmptyPath, treeNode, unconsPath, (%), (%!), (%!/), (%!~>), (%-))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Elements (br, whitespace, ι, π) as El
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.App as App
import Pantograph.Generic.Dynamics ((%.), (%.|))
import Pantograph.Generic.Dynamics (class Dynamics, Direction(..), StepExpr(..), SteppingRule(..), buildStepExpr, fromStepExprToExpr) as P
import Pantograph.Generic.GlobalMessageBoard as GMB
import Pantograph.Generic.Language (class Language, AnnExprCursor, AnnExprGyro, AnnExprNode(..), ChangingRule, Edit(..), Edits(..), Expr, ExprGyro, ExprNode, ExprTooth, RuleSort, Sort, SortChange, SortNode(..), SortingRule, SpecialEdits, applyRuleSortVarSubst, buildExpr, buildExprTooth, buildSortingRule, buildSortingRuleFromStrings, freshVarSort, getExprNodeSort, getExprNonEmptyPathInnerSort, getExprNonEmptyPathOuterSort, getExprSort, makeInjectRuleSort, makeSort, makeVarRuleSort, singletonExprNonEmptyPath) as P
import Pantograph.Generic.Rendering (class Rendering, ArrangeKid, EditorInput(..), RenderM, displayAnnExpr) as P
import Pantograph.Library.Change (getDiffChangingRule)
import Pantograph.Library.Edit as LibEdit
import Pantograph.Library.Rendering (π, (˜⊕), (⊕))
import Pantograph.Library.Step as LibStep
import Record as R
import Text.Pretty (class Pretty, parens, pretty, quotes, quotes2, (<+>))
import Type.Proxy (Proxy(..))
import Util (fromJust, (<$$>))

instance P.Language SN EL where
  getSortingRule el = getSortingRule el
  getChangingRule el = getChangingRule el
  topSort = topSort
  getDefaultExpr sr = getDefaultExpr sr
  getEditsAtSort sr ori = getEditsAtSort sr ori
  specialEdits = specialEdits
  validGyro gyro = validGyro gyro

instance P.Rendering SN EL CTX ENV where
  topCtx = Proxy /\ topCtx
  topEnv = Proxy /\ topEnv
  arrangeExpr node kids = arrangeExpr node kids
  getBeginsLine cursor = getBeginsLine cursor
  getInitialQuery cursor = getInitialQuery cursor

instance P.Dynamics SN EL CTX ENV where
  steppingRules = steppingRules

instance App.App SN EL CTX ENV where
  editorInput = P.EditorInput 
    { proxy_sn: Proxy :: Proxy SN }

-- types

type Expr = P.Expr SN EL
type ExprNode = P.ExprNode SN EL
type StepExpr = P.StepExpr SN EL
type ExprTooth = P.ExprTooth SN EL
type SortChange = P.SortChange SN
type RuleSort = P.RuleSort SN
type Sort = P.Sort SN
type Edit = P.Edit SN EL
type AnnExprGyro er = P.AnnExprGyro SN EL er
type AnnExprCursor er = P.AnnExprCursor SN EL er
type Edits = P.Edits SN EL
type SpecialEdits = P.SpecialEdits SN EL
type ExprGyro = P.ExprGyro SN EL
type SteppingRule = P.SteppingRule SN EL
type ChangingRule = P.ChangingRule SN
type SortingRule = P.SortingRule SN
type SplitChange = LibEdit.SplitChange SN
type AnnExprNode er = P.AnnExprNode SN EL er
type RenderM a = P.RenderM SN EL CTX ENV a
type ArrangeKid a = P.ArrangeKid SN EL a

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
  validKidsCount sn = (go sn == _)
    where
    go = case _ of
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
      StrInner s -> ass \[] -> quotes s
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
instance DisplayTreeNode SN where
  displayTreeNode sn = 
    let ass = assertValidTreeKids "displayTreeNode" sn in
    case sn of
      StrInner s -> ass \[] -> El.ι [El.π $ quotes2 s]
      Str -> ass \[str] -> El.ι [El.π "String ", str]
      VarJg -> ass \[γ, x, α, loc] -> El.ι [El.π "Var ", γ, x, El.π " : ", α, El.π " ", loc]
      TmJg -> ass \[γ, α] -> El.ι [γ, El.π " ⊢ ", α]
      NeJg -> ass \[γ, α] -> El.ι [γ, El.π " ⊢ ", α, El.π " [neutral]"]
      TyJg -> ass \[α] -> El.ι [El.π"⊨ ", α]
      NilCtx -> ass \[] -> El.ι [El.π "∅"]
      ConsCtx -> ass \[x, α, γ] -> El.ι [El.π "(", x, El.π " : ", α, El.π ")", El.π ", ", γ]
      DataTySN dt -> ass \[] -> El.ι [El.π (pretty dt)]
      ArrowTySN -> ass \[α, β] -> El.ι [El.π "(", α, El.π " → ", β, El.π ")"]
      LocalLoc -> ass \[] -> El.ι [El.π "[local]"]
      NonlocalLoc -> ass \[] -> El.ι [El.π "[nonlocal]"]

-- DataTy

data DataTy = UnitDataTy | BoolDataTy

derive instance Generic DataTy _
instance Show DataTy where show = genericShow
instance Eq DataTy where eq = genericEq
instance Ord DataTy where compare = genericCompare
instance Pretty DataTy where
  pretty = case _ of
    UnitDataTy -> "unit"
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
  validKidsCount el = (go el == _)
    where
    go = case _ of
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
      Format fmt -> ass \[e] -> case fmt of
        Newline -> "<newline> " <> e
        Indent -> "<indent> " <> e

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

topSort :: Sort
topSort = sr_jg_tm sr_ctx_nil (P.SN (DataTySN UnitDataTy) % [])

getDefaultExpr :: Sort -> Maybe Expr
getDefaultExpr (P.SN Str % [strInner]) = Just $ P.buildExpr StrEL {x: strInner} []
getDefaultExpr (P.SN TmJg % [γ, α]) = Just $ P.buildExpr HoleTm {γ, α} [reifyTypeSortAsTypeExpr α]
getDefaultExpr (P.SN TyJg % [α]) = Just $ reifyTypeSortAsTypeExpr α
getDefaultExpr _ = Nothing

specialEdits :: SpecialEdits
specialEdits = 
  { deleteExpr: case _ of
      P.SN TmJg % [γ, α] -> Just $ LibEdit.makeInsideChangeEdit $ P.buildExpr HoleTm {γ, α} [reifyTypeSortAsTypeExpr α]
      -- When you delete a type-Expr, you need to push an outward change that
      -- replaces the type-sort that is reflected in the type-Expr's sort
      -- (otherwise the type-Expr would just be filled to correspond to its
      -- type-Sort again).
      P.SN TyJg % [α] -> 
        let α' = P.freshVarSort "deleted" in
        Just $ LibEdit.makeOuterAndInsideChangeEdit (P.SN TyJg %! [α %!~> α']) (P.buildExpr HoleTy {α: α'} [])
      -- When you delete a string-Expr, you need to push an outward change the
      -- replaces the StrInner-sort with the empty-string StrInner-sort.
      P.SN Str % [strInner] -> Just $ LibEdit.makeOuterChangeEdit $ P.SN Str %! [strInner %!~> (P.SN (StrInner "") % [])]
      _ -> Nothing
  , copyExpr: const Nothing
  , deleteExprPath: \ch -> 
      let {outerChange, innerChange} = splitExprPathChanges ch in
      Just $ LibEdit.makeOuterAndInnerChangeEdit outerChange innerChange
  , copyExprPath: const Nothing
  , enter: \sort -> case sort of
      P.SN TmJg % [_, _] -> Just $ LibEdit.makeMiddleChangeEdit $ singletonNonEmptyPath $ P.buildExprTooth (Format Newline) {sort} [] []
      _ -> Nothing
  , tab: \sort -> case sort of
      P.SN TmJg % [_, _] -> Just $ LibEdit.makeMiddleChangeEdit $ singletonNonEmptyPath $ P.buildExprTooth (Format Indent) {sort} [] []
      _ -> Nothing
  }

validGyro :: forall er. AnnExprGyro er -> Boolean
-- Cursor
validGyro (CursorGyro (Cursor cursor)) | P.SN TmJg % [γ, α] <- P.getExprSort cursor.inside = true
validGyro (CursorGyro (Cursor cursor)) | P.SN NeJg % [γ, α] <- P.getExprSort cursor.inside = true
validGyro (CursorGyro (Cursor cursor)) | P.SN TyJg % [α] <- P.getExprSort cursor.inside = true
validGyro (CursorGyro (Cursor cursor)) | P.SN Str % [s] <- P.getExprSort cursor.inside = true
-- Select
validGyro (SelectGyro (Select select)) = treeNode (P.getExprNonEmptyPathInnerSort select.middle) == treeNode (P.getExprNonEmptyPathOuterSort select.middle)
--
validGyro _ = false

getSortingRule :: EL -> SortingRule
getSortingRule =
  case _ of
    StrEL -> P.buildSortingRule (Proxy :: Proxy (x::StrInnerR)) \{x} ->
      [] /\
      ( rs_str x )

    ZeroVar -> P.buildSortingRule (Proxy :: Proxy (γ::CtxR, x::StrInnerR, α::TyR)) \{γ, x, α} ->
      [] /\
      ( rs_jg_var γ x α rs_loc_local )

    SucVar -> P.buildSortingRule (Proxy :: Proxy (γ::CtxR, x::StrInnerR, α::TyR, y::CtxR, β::TyR, loc::LocR)) \{γ, x, α, y, β, loc} ->
      [ rs_jg_var γ x α loc ] /\
      ( rs_jg_var (rs_ctx_cons y β γ) x α loc )

    FreeVar -> P.buildSortingRuleFromStrings ["x", "α"] \[x, α] ->
      [] /\
      ( rs_jg_var rs_ctx_nil x α rs_loc_nonlocal )

    LamTm -> P.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
      [ rs_str x
      , rs_jg_ty α
      , rs_jg_tm (rs_ctx_cons x α γ) β ] /\
      ( rs_jg_tm γ (rs_ty_arrow α β) )

    LetTm -> P.buildSortingRuleFromStrings ["x", "α", "β", "γ"] \[x, α, β, γ] ->
      [ rs_str x
      , rs_jg_ty α
      , rs_jg_tm (rs_ctx_cons x α γ) α
      , rs_jg_tm (rs_ctx_cons x α γ) β
      ] /\
      ( rs_jg_tm γ β )

    VarNe -> P.buildSortingRuleFromStrings ["γ", "x", "α", "loc"] \[γ, x, α, loc] ->
      [ rs_jg_var γ x α loc ] /\
      ( rs_jg_ne γ α )

    IfTm -> P.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_tm γ rs_ty_bool
      , rs_jg_tm γ α ] /\
      ( rs_jg_tm γ α )

    CallTm -> P.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_ne γ α ] /\
      ( rs_jg_tm γ α )

    ErrorCallTm -> P.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ α ] /\
      ( rs_jg_tm γ β )

    HoleTm -> P.buildSortingRuleFromStrings ["γ", "α"] \[γ, α] ->
      [ rs_jg_ty α ] /\
      ( rs_jg_tm γ α )

    ErrorBoundaryTm -> P.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_tm γ α ] /\
      ( rs_jg_tm γ β )

    AppNe -> P.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ (rs_ty_arrow α β)
      , rs_jg_tm γ α ] /\
      ( rs_jg_ne γ β )

    GrayAppNe -> P.buildSortingRuleFromStrings ["γ", "α", "β"] \[γ, α, β] ->
      [ rs_jg_ne γ β
      , rs_jg_tm γ α ] /\
      ( rs_jg_ne γ β )

    HoleTy -> P.buildSortingRuleFromStrings ["α"] \[α] ->
      [] /\
      ( rs_jg_ty α )

    DataTyEL dt -> P.buildSortingRuleFromStrings [] \[] ->
      [] /\
      ( rs_jg_ty (rs_ty_dt dt) )

    ArrowTyEL -> P.buildSortingRuleFromStrings ["α", "β"] \[α, β] ->
      [ rs_jg_ty α
      , rs_jg_ty β ] /\
      ( rs_jg_ty (rs_ty_arrow α β) )

    Format _ -> P.buildSortingRuleFromStrings ["sort"] \[sort] ->
      [ sort ] /\
      ( sort )

getChangingRule :: EL -> ChangingRule
getChangingRule el = case el of
  -- HoleTm -> P.buildChangingRule ["γ", "α"] \[γ, α] ->
  --   [ P.InjectRuleSortNode (P.SN TmJg) %! [Supertype.inject γ, Supertype.inject α] ] /\
  --   ?a
  --   -- [ Plus /\ (P.InjectRuleSortNode (P.SN TmJg) %- 1 /\ [γ]) %!/ Supertype.inject (rs_jg_ty α) ] /\
  --   -- [ Supertype.inject $ rs_jg_ty α ]
  --   -- /\
  --   -- ( Supertype.inject $ rs_jg_tm γ α )
  _ -> getDiffChangingRule el

steppingRules :: Array SteppingRule
steppingRules =
  [ localBecomesNonlocal
  , nonlocalBecomesLocal
  , insertSuc
  , removeSuc
  , passThroughArrow
  , typeBecomesRhsOfChange
  , wrapLambda
  , unWrapLambda
  , rehydrateApp
  , wrapApp
  -- , unWrapApp
  , makeAppGray
  , wrapCallInErrorUp
  , wrapCallInErrorDown
  , removeError
  , mergeErrors
  -- TODO: enable
  , LibStep.makeDefaultDownSteppingRule
  , LibStep.unless "isUpInCall" isUpInCall $ LibStep.makeDefaultUpSteppingRule
  ]
  where
  -- {e}↓{Var (+ <{ y : β, {> γ <}}>) x α loc}  ~~>  Suc {e}↓{Var γ x α loc}
  insertSuc = P.SteppingRule "insertSuc" case _ of
    P.Boundary (P.Down /\ (P.SN VarJg %! [Shift (Plus /\ (P.SN ConsCtx %- 2 /\ [y, β])) γ, x, α, loc])) e -> Just $
      se_var_suc (epR γ) (epR x) (epR α) y β (epR loc) $
        P.Boundary (P.Down /\ InjectChange (P.SN VarJg) [γ, x, α, loc]) e
    _ -> Nothing

  -- {Zero}↓{Var (- <{ x : α, {> γ <}}>) x α Local} ~~> {Free}↑{Var id x α (Local ~> Nonlocal)}
  localBecomesNonlocal = P.SteppingRule "localBecomesNonlocal" case _ of
    P.Down /\ (P.SN VarJg %! [Minus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, x', α', P.SN LocalLoc %! []]) %.|
    (P.EN ZeroVar _ _ %. [])
    | true -> Just $
      P.Boundary 
        (P.Up /\ (P.SN VarJg %! [Supertype.inject (epR γ), x', α', Replace sr_loc_local sr_loc_nonlocal]))
        (inject $ freeVarTerm {γ: (epR γ), x, α})
    _ -> Nothing

  -- {Var (- <{ y : β , {> γ <}}>) x α loc}↓{Suc pred} ~~> {Var γ x α loc}↓{pred}
  removeSuc = P.SteppingRule "removeSuc" case _ of
    (P.Down /\ (P.SN VarJg %! [Minus /\ (P.SN ConsCtx %- 1 /\ [_y, _β]) %!/ γ, x, α, loc])) %.| (P.EN SucVar _sigma _ %. [pred]) -> Just $
      P.Down /\ (P.SN VarJg %! [γ, x, α, loc]) %.| pred
    _ -> Nothing

  -- {Var (+ <{ x : α , {> γ< } }>) x α Nonlocal}↓{_} ~~> Z
  nonlocalBecomesLocal = P.SteppingRule "nonlocalBecomesLocal" case _ of
    (P.Down /\ (Plus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ γ)) %.| _ -> Just $
      se_var_zero (epR γ) x α
    _ -> Nothing

  -- {α! -> β!}↓{α -> β} ~~> {α}↓{α!} -> {β}↓{β!}
  passThroughArrow = P.SteppingRule "passThroughArrow" case _ of
    (P.Down /\ (P.SN ArrowTySN %! [α, β])) %.| (P.EN ArrowTyEL sigma _ %. [αCh, βCh]) -> Just $
      P.EN ArrowTyEL sigma {} %. 
        [ P.Down /\ α %.| αCh
        , P.Down /\ β %.| βCh ]
    _ -> Nothing

  -- {_ : Type α!}↓{_} ~~> α
  typeBecomesRhsOfChange = P.SteppingRule "typeBecomesRhsOfChange" case _ of
    (P.Down /\ (P.SN TyJg %! [α])) %.| _ -> Just $ inject (reifyTypeSortAsTypeExpr (epR α))
    _ -> Nothing

  -- {Term γ (+ <{α -> {> β<}}>)}↓{b} ~~> lam ~ : α . {Term (+ <{ ~ : α, {> γ <}}>) β}↓{b}
  wrapLambda = P.SteppingRule "wrapLambda" case _ of
    (P.Down /\ (P.SN TmJg %! [γ, Plus /\ (P.SN TmJg %- 1 /\ [α]) %!/ β])) %.| b -> Just $
      let x = sr_strInner "" in
      se_tm_lam x α (epR β) (epR γ) (se_str x) (inject (reifyTypeSortAsTypeExpr α)) b
    _ -> Nothing

  -- {Term γ (- <{α -> {> β <}}>)}↓{lam x : α . b} ~~> {Term (- x : α, γ) β}↓{b}
  unWrapLambda = P.SteppingRule "unWrapLambda" case _ of
    P.Down /\ ch %.| (P.EN LamTm _sigma {} %. [xExpr, _αExpr, b]) -> do
      let x = P.getExprSort (P.fromStepExprToExpr xExpr)
      ch' <- case ch of
        P.SN TmJg %! [γ, P.SN ArrowTySN %! [α, β]] -> Just $
          P.SN TmJg %! [Minus /\ (P.SN ConsCtx %- 2 /\ [x, epR α]) %!/ γ, β]
        -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
        P.SN TmJg %! [γ, (P.SN ArrowTySN % [α, β]) %!~> mv@(P.VarSN _ % [])] -> Just $ 
          P.SN TmJg %! [Minus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ γ, β %!~> mv]
        _ -> Nothing
      Just $ P.Boundary (P.Down /\ ch') b
    _ -> Nothing

  -- -- {Term γ (+ α -> β)}↑{f} ~~> {Term γ β}↑{App f (? : α)}
  -- wrapApp = P.SteppingRule "wrapApp" case _ of
  --   P.Up /\ (P.SN TmJg %! [γ, Plus /\ (P.SN ArrowTySN %- (1 /\ [α])) %!/ β]) %.| f -> Just $ 
  --     P.Up /\ (P.SN TmJg %! [γ, β]) %.| (P.buildStepExpr AppNe {γ: epL γ, α, β: epL β} [f, se_tm_hole (epR γ) α (inject (reifyTypeSortAsTypeExpr α))])
  --   _ -> Nothing

  -- {Ne γ (+ α -> β)}↑{f} ~~> {Ne γ β}↑{App f (? : α)}
  wrapApp = P.SteppingRule "wrapApp" case _ of
    P.Up /\ (P.SN NeJg %! [γ, Plus /\ (P.SN ArrowTySN %- (1 /\ [α])) %!/ β]) %.| f -> Just $ 
      P.Up /\ (P.SN NeJg %! [γ, β]) %.| (P.buildStepExpr AppNe {γ: epL γ, α, β: epL β} [f, se_tm_hole (epR γ) α (inject (reifyTypeSortAsTypeExpr α))])
    _ -> Nothing

  -- App {Term γ (- α -> β)}↑{b} a ~~> {Term γ β}↑{b}
  unWrapApp = P.SteppingRule "unWrapApp" case _ of
    P.EN AppNe _ _ %. [P.Up /\ (P.SN NeJg %! [γ, Minus /\ (P.SN ArrowTySN %- 1 /\ [_α]) %!/ β]) %.| b, _a] -> Just $ 
      P.Up /\ (P.SN NeJg %! [γ, β]) %.| b
    _ -> Nothing 

  -- App {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{GrayApp f a}
  -- App {Ne γ ((α -> β) ~> ?delta)}↑{f} a ~~> {Ne γ {β ~> ?delta}}↑{GrayApp f a}
  makeAppGray = P.SteppingRule "makeAppGray" case _ of
    P.EN AppNe _ _ %. [P.Up /\ (P.SN NeJg %! [γ, P.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
      P.Up /\ (P.SN NeJg %! [γ, β]) %.| P.buildStepExpr GrayAppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
    -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
    P.EN AppNe _ _ %. [P.Up /\ (P.SN NeJg %! [γ, (P.SN ArrowTySN % [α, β]) %!~> mv@(P.VarSN _ % [])]) %.| f, a] -> Just $
      P.Up /\ (P.SN NeJg %! [γ, β %!~> mv]) %.| P.buildStepExpr GrayAppNe {γ: epR γ, α, β} [f, a]
    _ -> Nothing 

  -- GrayApp f (? : α) ~~> f
  removeGrayHoleArg = P.SteppingRule "removeGrayHoleArg" case _ of
    P.EN GrayAppNe _ _ %. [f, P.EN HoleTm _ _ %. [_α]] -> Just $ 
      f
    _ -> Nothing

  -- GrayApp {Ne γ (α -> β)}↑{f} a ~~> {Ne γ β}↑{App f a}
  rehydrateApp = P.SteppingRule "rehydrateApp" case _ of
    P.EN GrayAppNe _ _ %. [P.Up /\ (P.SN NeJg %! [γ, P.SN ArrowTySN %! [α, β]]) %.| f, a] -> Just $
      P.Up /\ (P.SN NeJg %! [γ, β]) %.| P.buildStepExpr AppNe {γ: epR γ, α: epR α, β: epR β} [f, a]
    _ -> Nothing

  -- {Ne γ! α!}↑{Call n} ~~> {γ, αL, αR}ErrorCall{n}
  replaceCallWithError = P.SteppingRule "replaceCallWithError" case _ of
    P.Up /\ (P.SN NeJg %! [γ, α]) %.| (P.EN CallTm _ _ %. [n]) -> Just $
      P.buildStepExpr ErrorCallTm {γ: epR γ, α: epL α, β: epR α} [n]
    _ -> Nothing

  -- {γ, α, α}ErrorCall{n} ~~> Call n
  replaceErrorWithCall = P.SteppingRule "replaceErrorWithCall" case _ of
    P.EN ErrorCallTm sigma _ %. [n]
      | γ <- P.applyRuleSortVarSubst sigma "γ"
      , α <- P.applyRuleSortVarSubst sigma "α"
      , β <- P.applyRuleSortVarSubst sigma "β"
      , α == β -> Just $
      P.buildStepExpr CallTm {γ, α} [n]
    _ -> Nothing

  -- Call ({Ne γ (α -> β)}↑{n}) ~~> {Term γ β}↑{{α -> β, β}ErrorBoundary{Call n}}
  wrapCallInErrorUp = P.SteppingRule "wrapCallInErrorUp" case _ of
    P.EN CallTm _ _ %. [P.Up /\ (P.SN NeJg %! [γ, P.SN ArrowTySN %! [α, β]]) %.| n] -> Just $
      P.Up /\ (P.SN TmJg %! [γ, β]) %.| (P.buildStepExpr ErrorBoundaryTm {γ: epL γ, α: P.SN ArrowTySN % [epL α, epL β], β: epR β} [P.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]])
    _ -> Nothing

  -- {Tm γ! α!}↓{Call n : Tm γL αL} ~~> {γR, αL, αR}ErrorBoundary{Call n : }
  wrapCallInErrorDown = P.SteppingRule "wrapCallInErrorDown" case _ of
    P.Down /\ (P.SN TmJg %! [γ, α]) %.| (P.EN CallTm _ _ %. [n]) -> Just $
      P.buildStepExpr ErrorBoundaryTm {γ: epR γ, α: epL α, β: epR α} [P.buildStepExpr CallTm {γ: epL γ, α: epL α} [n]]
    _ -> Nothing

  -- {γ, α, α}ErrorBoundary{a : Tm γ α} ~~> a
  removeError = P.SteppingRule "removeError" case _ of
    P.EN ErrorBoundaryTm sigma _ %. [a] 
      | γ <- P.applyRuleSortVarSubst sigma "γ" 
      , α <- P.applyRuleSortVarSubst sigma "α" 
      , β <- P.applyRuleSortVarSubst sigma "β" 
      , α == β -> Just $
      a
    _ -> Nothing

  -- {γ, β, delta}ErrorBoundary{{γ, α, β}ErrorBoundary{a : Tm γ α}} ~~> {γ, α, delta}ErrorBoundary{a : Tm γ α}
  mergeErrors = P.SteppingRule "mergeErrors" case _ of
    P.EN ErrorBoundaryTm sigma1 _ %. [P.EN ErrorBoundaryTm sigma2 _ %. [a]] -> Just $
      let γ = P.applyRuleSortVarSubst sigma1 "γ" in
      let α = P.applyRuleSortVarSubst sigma1 "α" in
      let delta = P.applyRuleSortVarSubst sigma2 "β" in
      P.buildStepExpr ErrorBoundaryTm {γ, α, β: delta} [a]
    _ -> Nothing

getEditsAtSort :: Sort -> Orientation -> Edits
getEditsAtSort (P.SN Str % [_]) Outside = P.Edits $ StringQuery.fuzzy 
  { toString: fst, maxPenalty
  , getItems: \string ->
      [ Tuple string $
        NonEmptyArray.singleton $
          LibEdit.makeInsideChangeEdit $ ex_str (sr_strInner string) ]
  }
getEditsAtSort (Tree (P.SN VarJg) []) Outside = P.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
getEditsAtSort (Tree (P.SN TmJg) [γ0, α0]) Outside = P.Edits $ StringQuery.fuzzy
  { toString: fst, maxPenalty
  , getItems: const $ LibEdit.makeEditRows [
      "lambda" /\ [ 
      do
        let γ = γ0
            β = α0
            α = sr_freshVar "α"
            x = sr_strInner ""
            xEx = ex_str x
            αEx = reifyTypeSortAsTypeExpr α
        Just $ P.Edit
          { sigma: Nothing
          , outerChange: Just $ P.SN TmJg %! [Supertype.inject γ, Plus /\ (P.SN ArrowTySN %- 1 /\ [α]) %!/ Supertype.inject β]
          , middle: Just $ P.singletonExprNonEmptyPath $ P.buildExprTooth LamTm {γ, x, α, β} [xEx, αEx] []
          , innerChange: Just $ P.SN TmJg %! [Plus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ Supertype.inject γ, Supertype.inject β]
          , inside: Nothing } 
      ], 
      "let" /\ [
      do
        let x = sr_strInner ""
            α = sr_freshVar "α"
            β = α0
            γ = γ0
            xEx = ex_str x
            αEx = reifyTypeSortAsTypeExpr α
            a = ex_tm_hole (P.SN ConsCtx % [x, α, γ]) α αEx
        Just $ P.Edit
          { sigma: Nothing
          , outerChange: Nothing
          , middle: Just $ P.singletonExprNonEmptyPath $ P.buildExprTooth LetTm {x, α, β, γ} [xEx, αEx, a] []
          , innerChange: Just $ P.SN TmJg %! [Minus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ Supertype.inject γ, Supertype.inject β] 
          , inside: Nothing },
      do
        let x = sr_strInner ""
            α = α0
            γ = γ0
            xEx = ex_str x
            αEx = reifyTypeSortAsTypeExpr α
            a = ex_tm_hole (P.SN ConsCtx % [x, α, γ]) α αEx
        Just $ P.Edit
          { sigma: Nothing
          , outerChange: Nothing
          , middle: Just $ P.singletonExprNonEmptyPath $ P.buildExprTooth LetTm {x, α, β: α, γ} [xEx, αEx] [a]
          , innerChange: Just $ P.SN TmJg %! [Minus /\ (P.SN ConsCtx %- 2 /\ [x, α]) %!/ Supertype.inject γ, Supertype.inject α] 
          , inside: Nothing } 
      ]
    ]
  }
getEditsAtSort (Tree (P.SN NeJg) []) Outside = P.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
getEditsAtSort (Tree (P.SN TyJg) []) Outside = P.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }
getEditsAtSort sort orientation = 
  GMB.debugR (display "getEditsAtSort/default") {sort: display $ show sort, orientation: display $ show orientation} \_ ->
  P.Edits $ StringQuery.fuzzy { toString: fst, maxPenalty, getItems: const [] }

maxPenalty :: Fuzzy.Distance
maxPenalty = Fuzzy.Distance 1 0 0 0 0 0

-- Renderer

type CTX = (indentLevel :: Int)
topCtx :: Record CTX
topCtx = {indentLevel: 0}

type ENV = (countHoleTm :: Int, countHoleTy :: Int)
topEnv :: Record ENV
topEnv = {countHoleTm: 0, countHoleTy: 0}

arrangeExpr :: forall er a. AnnExprNode er -> Array (RenderM (a /\ AnnExprNode er)) -> RenderM (Array (ArrangeKid a))
arrangeExpr node@(P.EN StrEL _ _) [] | P.SN Str % [P.SN (StrInner string) % []] <- P.getExprNodeSort node = do
  if String.null string 
    then pure $ Array.fromFoldable $ π."~" ⊕ Nil
    else pure $ Array.fromFoldable $ string ⊕ Nil
arrangeExpr node@(P.EN ZeroVar _ _) [] | _ % _ <- P.getExprNodeSort node = do
  pure $ Array.fromFoldable $ π."Z" ⊕ Nil
arrangeExpr node@(P.EN SucVar _ _) [x] | _ % _ <- P.getExprNodeSort node = do
  x /\ _ <- x
  pure $ Array.fromFoldable $ π."S" ⊕ x ˜⊕ Nil
arrangeExpr node@(P.EN FreeVar _ _) [] | _ % _ <- P.getExprNodeSort node =
  pure $ Array.fromFoldable $ π."F" ⊕ Nil
arrangeExpr node@(P.EN LamTm _ _) [x, α, b] | _ % _ <- P.getExprNodeSort node = do
  x /\ _ <- x
  α /\ _ <- α
  b /\ _ <- b
  pure $ Array.fromFoldable $ π."λ" ⊕ " " ⊕ x ˜⊕ " " ⊕ π.":" ⊕ " " ⊕ α ˜⊕ " " ⊕ π."." ⊕ " " ⊕ b ˜⊕ Nil
arrangeExpr node@(P.EN LetTm _ _) [x, α, a, b] | _ % _ <- P.getExprNodeSort node = do
  x /\ _ <- x # local (R.modify (Proxy :: Proxy "indentLevel") (_ + 1))
  α /\ _ <- α # local (R.modify (Proxy :: Proxy "indentLevel") (_ + 1))
  a /\ aNode <- a # local (R.modify (Proxy :: Proxy "indentLevel") (_ + 1))
  b /\ _ <- b
  let
    parensYes = defer \_ -> Array.fromFoldable $ π."let" ⊕ " " ⊕ x ˜⊕ " " ⊕ π.":" ⊕ " " ⊕ α ˜⊕ " " ⊕ π."=" ⊕ " " ⊕ π."(" ⊕ a ˜⊕ π.")" ⊕ " " ⊕ π."in" ⊕ " " ⊕ b ˜⊕ Nil
    parensNo = defer \_ -> Array.fromFoldable $ π."let" ⊕ " " ⊕ x ˜⊕ " " ⊕ π.":" ⊕ " " ⊕ α ˜⊕ " " ⊕ π."=" ⊕ " " ⊕ a ˜⊕ " " ⊕ π."in" ⊕ " " ⊕ b ˜⊕ Nil
  pure <<< force $ case aNode of
    P.EN LetTm _ _ -> parensYes
    P.EN CallTm _ _ -> parensYes
    P.EN ErrorCallTm _ _ -> parensYes
    _ -> parensNo
arrangeExpr node@(P.EN IfTm _ _) [a, b, c] | _ % _ <- P.getExprNodeSort node = do
  a /\ _ <- a
  b /\ _ <- b
  c /\ _ <- c
  pure $ Array.fromFoldable $ π."if" ⊕ " " ⊕ a ˜⊕ " " ⊕ π."then" ⊕ " " ⊕ b ˜⊕ " " ⊕ π."else" ⊕ " " ⊕ c ˜⊕ Nil
arrangeExpr node@(P.EN CallTm _ _) [ne] | _ % _ <- P.getExprNodeSort node = do
  ne /\ neNode <- ne
  case neNode of
    -- P.EN VarNe _ _ -> pure $ Array.fromFoldable $ ne ˜⊕ Nil
    _ -> pure $ Array.fromFoldable $ π."(" ⊕ ne ˜⊕ π.")" ⊕ Nil
arrangeExpr node@(P.EN ErrorCallTm _ _) [ne] | _ % _ <- P.getExprNodeSort node = do
  ne /\ _ <- ne
  pure $ Array.fromFoldable $ "ErrorCall " ⊕ ne ˜⊕ Nil
arrangeExpr node@(P.EN HoleTm _ _) [α] | _ % _ <- P.getExprNodeSort node = do
  α /\ _ <- α
  _countHoleTm <- modify (R.modify (Proxy :: Proxy "countHoleTm") (_ + 1)) <#> (_.countHoleTm >>> (_ - 1))
  -- pure $ Array.fromFoldable $ π."(" ⊕ [HH.span [HP.classes [HH.ClassName "HoleTm"]] [HH.text "□", HH.sub_ [HH.text $ show countHoleTm]] :: Html] ⊕ " " ⊕ π.":" ⊕ " " ⊕ α ˜⊕ π.")" ⊕ Nil
  pure $ Array.fromFoldable $ ["HoleTm-anchor"] ⊕ π."{{" ⊕ " " ⊕ π."box" ⊕ " " ⊕ π.":" ⊕ " " ⊕ α ˜⊕ " " ⊕ π."}}" ⊕ Nil
arrangeExpr node@(P.EN ErrorBoundaryTm _ _) [a] | _ % _ <- P.getExprNodeSort node = do
  a /\ _ <- a
  pure $ Array.fromFoldable $ "ErrorBoundary " ⊕ a ˜⊕ Nil
arrangeExpr node@(P.EN VarNe _ _) [_x] | P.SN VarJg % [γ, P.SN (StrInner string) % [], α, P.SN LocalLoc % []] <- P.getExprNodeSort node = 
  pure $ Array.fromFoldable $ π."#" ⊕ string ⊕ Nil
arrangeExpr node@(P.EN VarNe _ _) [x] | P.SN VarJg % [γ, P.SN (StrInner string) % [], α, P.SN NonlocalLoc % []] <- P.getExprNodeSort node = 
  pure $ Array.fromFoldable $ "Nonlocal#" ⊕ string ⊕ Nil
arrangeExpr node@(P.EN AppNe _ _) [f, a] | _ % _ <- P.getExprNodeSort node = do
  f /\ _ <- f
  a /\ aNode <- a
  pure $ Array.fromFoldable $
    if argRequiresParens aNode
      then f ˜⊕ " " ⊕ π."(" ⊕ a ˜⊕ π.")" ⊕ Nil
      else f ˜⊕ " " ⊕ a ˜⊕ Nil
  where
  argRequiresParens :: AnnExprNode er -> Boolean
  argRequiresParens _ = false
arrangeExpr node@(P.EN GrayAppNe _ _) [f, a] | _ % _ <- P.getExprNodeSort node = do
  f /\ _ <- f
  a /\ _ <- a
  pure $ Array.fromFoldable $ f ˜⊕ " " ⊕ π."{" ⊕ a ˜⊕ π."}" ⊕ Nil

-- -- if the reflected sort-type is a SortVar
-- arrangeExpr node@(P.EN HoleTy _ _) [] | P.SN TyJg % [α@(P.VarSN _ % [])] <- P.getExprNodeSort node =
--   let doHoleTySplotch = true in
--   if doHoleTySplotch then do
--     _countHoleTy <- modify (R.modify (Proxy :: Proxy "countHoleTy") (_ + 1)) <#> (_.countHoleTy >>> (_ - 1))
--     pure $ Array.fromFoldable $ [HH.span [HP.classes [HH.ClassName "HoleTy"]] [display α]] ⊕ Nil
--   else do
--     countHoleTy <- modify (R.modify (Proxy :: Proxy "countHoleTy") (_ + 1)) <#> (_.countHoleTy >>> (_ - 1))
--     pure $ Array.fromFoldable $ [HH.span [HP.classes [HH.ClassName "HoleTy"]] [HH.text $ showCountHoleTy countHoleTy] :: Html] ⊕ Nil

-- if the reflected sort-type is a more specific type (this type hole has been refined)
arrangeExpr node@(P.EN HoleTy _ _) [] | P.SN TyJg % [α] <- P.getExprNodeSort node = do
  _countHoleTy <- modify (R.modify (Proxy :: Proxy "countHoleTy") (_ + 1)) <#> (_.countHoleTy >>> (_ - 1))
  pure $ Array.fromFoldable $ [HH.span [HP.classes [HH.ClassName "HoleTy"]] [display α] :: Html] ⊕ Nil

arrangeExpr node@(P.EN (DataTyEL dt) _ _) [] | _ % _ <- P.getExprNodeSort node = do
  pure $ Array.fromFoldable $ (["DataTy"] /\ pretty dt) ⊕ Nil
arrangeExpr node@(P.EN ArrowTyEL _ _) [alpha, beta] | _ % _ <- P.getExprNodeSort node = do
  alpha /\ alphaNode <- alpha
  beta /\ _ <- beta
  pure $ Array.fromFoldable $
    if domainRequiresParens alphaNode
      then π."(" ⊕ alpha ˜⊕ π.")" ⊕ " " ⊕ π."->" ⊕ " " ⊕ beta ˜⊕ Nil
      else alpha ˜⊕ " " ⊕ π."->" ⊕ " " ⊕ beta ˜⊕ Nil
  where
  domainRequiresParens :: AnnExprNode er -> Boolean
  domainRequiresParens (P.EN ArrowTyEL _ _) = true
  domainRequiresParens _ = false
arrangeExpr node@(P.EN (Format fmt) _ _) [a] | _ % _ <- P.getExprNodeSort node = do
  case fmt of
    Newline -> do
      a /\ _ <- a
      indentLevel <- asks _.indentLevel
      pure $ Array.fromFoldable $ ([El.whitespace " ↪", El.br :: Html] <> Array.replicate indentLevel (El.whitespace " ⇥" :: Html)) ⊕ a ˜⊕ Nil
    Indent -> do
      a /\ _ <- a # local (R.modify (Proxy :: Proxy "indentLevel") (_ + 1))
      pure $ Array.fromFoldable $ [El.whitespace " ⇥" :: Html] ⊕ a ˜⊕ Nil
arrangeExpr node mkids = do
  kidNodes <- snd <$$> sequence mkids
  GMB.errorR (display"[arrangeExpr] invalid") {node: display $ pretty node, kidNodes: display $ pretty kidNodes}

getBeginsLine :: forall er. AnnExprCursor er -> Boolean
getBeginsLine (Cursor {outside}) | Just {inner: P.EN (Format Newline) _ _ %- 0 /\ []} <- unconsPath outside = true
getBeginsLine _ = false

getInitialQuery :: forall er. AnnExprCursor er -> String
getInitialQuery (Cursor {inside: e}) | P.SN Str % [P.SN (StrInner string) % []] <- P.getExprSort e = string
getInitialQuery _ = ""

-- utilities

splitExprPathChanges :: SplitChange
splitExprPathChanges (P.SN TmJg %! [γ, α]) = 
-- | type change goes up/out, context change goes down/in
  { outerChange: P.SN TmJg %! [Supertype.inject $ epR γ, α]
  , innerChange: invert $ P.SN TmJg %! [γ, Supertype.inject $ epL α] }
-- | type change goes up/out, context change goes down/in
splitExprPathChanges (P.SN NeJg %! [γ, α]) =
  { outerChange: P.SN NeJg %! [Supertype.inject $ epR γ, α]
  , innerChange: invert $ P.SN NeJg %! [γ, Supertype.inject $ epL α] }
-- | type change goes up/out
splitExprPathChanges ch@(P.SN TyJg %! [_α]) =
  { outerChange: ch
  , innerChange: invert $ Supertype.inject $ epL ch }
-- | meta var change goes up/out and down/in
splitExprPathChanges ch@(P.VarSN _ %! []) =
  { outerChange: ch 
  , innerChange: ch }
splitExprPathChanges ch = GMB.errorR (display"[splitExprPathChanges] invalid") {ch: display ch}

freeVarTerm :: {x :: Sort, α :: Sort, γ :: Sort} -> Expr
freeVarTerm {γ, x, α} = case γ of
  Tree (P.SN ConsCtx) [y, β, γ'] -> sucVar {y, β, pred: freeVarTerm {γ: γ', x, α}}
  Tree (P.SN NilCtx) [] -> ex_var_free x α
  _ -> GMB.errorR (display"[freeVarTerm] impossible") {γ: display γ, x: display x, α: display α}

sucVar :: {y :: Sort, β :: Sort, pred :: Expr} -> Expr
sucVar {y, β, pred} | P.SN VarJg % [γ, x, α, loc] <- P.getExprSort pred = ex_var_suc γ x α y β loc pred
sucVar {y, β, pred} = GMB.errorR (display"[sucVar] impossible") {y: display y, β: display β, pred: P.displayAnnExpr pred}

-- | This is necessary because the wrapApp rule conflicts with the `defaultUp`,
-- | and the priority order of the list isn't enough because `defaultUp` happens
-- | on a term higher up in the tree.
isUpInCall :: StepExpr -> Boolean
isUpInCall (P.EN CallTm _ _ %. [P.Up /\ (P.SN NeJg %! [_γ, P.SN ArrowTySN %! [_α, _beta]]) %.| _]) = true
isUpInCall _ = false

reifyTypeSortAsTypeExpr :: Sort -> Expr
reifyTypeSortAsTypeExpr (P.SN (DataTySN dt) % []) = P.buildExpr (DataTyEL dt) {} []
reifyTypeSortAsTypeExpr (P.SN ArrowTySN % [α, β]) = P.buildExpr ArrowTyEL {α, β} [reifyTypeSortAsTypeExpr α, reifyTypeSortAsTypeExpr β]
reifyTypeSortAsTypeExpr α@(P.VarSN _ % []) = P.buildExpr HoleTy {α} []
reifyTypeSortAsTypeExpr s = GMB.errorR (display "[reifyTypeSortAsTypeExpr] invalid") {sort: display s}

showCountHoleTy :: Int -> String
showCountHoleTy i = if d == 0 then str else str <> "#" <> show d
  where
  n = Array.length alphabet
  i' = i `mod` n
  d = i `div` n
  cp = fromJust $ alphabet Array.!! i'
  str = String.singleton cp

alphabet :: Array CodePoint
alphabet = String.codePointFromChar <$> Enum.enumFromTo 'A' 'Z'

-- shallow Sort

sr_freshVar string = P.freshVarSort string

sr_strInner string = P.makeSort (StrInner string) []
sr_str strInner = P.makeSort Str [strInner]

sr_jg_var γ x α loc = P.makeSort VarJg [γ, x, α, loc]
sr_jg_tm γ α = P.makeSort TmJg [γ, α]
sr_jg_ne γ α = P.makeSort NeJg [γ, α]
sr_jg_ty α = P.makeSort TyJg [α]

sr_ctx_nil = P.makeSort NilCtx []
sr_ctx_cons γ x α = P.makeSort ConsCtx [γ, x, α]

sr_ty_dt dt = P.makeSort DataTySN [dt]
sr_ty_arrow α β = P.makeSort ArrowTySN [α, β]
sr_ty_bool = P.makeSort (DataTySN BoolDataTy) []

sr_loc_local = P.makeSort LocalLoc []
sr_loc_nonlocal = P.makeSort NonlocalLoc []

-- shallow RuleSort

rs_freshVar var = P.makeVarRuleSort var

rs_strInner string = P.makeInjectRuleSort (StrInner string) []
rs_str strInner = P.makeInjectRuleSort Str [strInner]

rs_jg_var γ x α loc = P.makeInjectRuleSort VarJg [γ, x, α, loc]
rs_jg_tm γ α = P.makeInjectRuleSort TmJg [γ, α]
rs_jg_ne γ α = P.makeInjectRuleSort NeJg [γ, α]
rs_jg_ty α = P.makeInjectRuleSort TyJg [α]

rs_ctx_nil = P.makeInjectRuleSort NilCtx []
rs_ctx_cons γ x α = P.makeInjectRuleSort ConsCtx [γ, x, α]

rs_ty_dt dt = P.makeInjectRuleSort (DataTySN dt) []
rs_ty_arrow α β = P.makeInjectRuleSort ArrowTySN [α, β]
rs_ty_bool = P.makeInjectRuleSort (DataTySN BoolDataTy) []

rs_loc_local = P.makeInjectRuleSort LocalLoc []
rs_loc_nonlocal = P.makeInjectRuleSort NonlocalLoc []

-- shallow Expr

-- ex = buildExprShallowSyntax (Proxy :: Proxy SN) (Proxy :: Proxy ()) exprNodes

-- exprNodes =  
--   ((Proxy :: Proxy "str") /\ \{x} -> P.buildExpr StrEL {x} []) :
--   ((Proxy :: Proxy "var_zero") /\ \{γ, x, α} -> P.buildExpr ZeroVar {γ, x, α} []) :
--   ((Proxy :: Proxy "var_suc") /\ \{γ, x, α, y, β, loc, pred} -> P.buildExpr SucVar {γ, x, α, y, β, loc} [pred]) :
--   ((Proxy :: Proxy "var_free") /\ \{x, α} -> P.buildExpr FreeVar {x, α} []) :
--   nil

ex_str x = P.buildExpr StrEL {x} [] 

ex_var_zero γ x α = P.buildExpr ZeroVar {γ, x, α} []
ex_var_suc γ x α y β loc pred = P.buildExpr SucVar {γ, x, α, y, β, loc} [pred]
ex_var_free x α = P.buildExpr FreeVar {x, α} []

ex_ty_hole α = P.buildExpr HoleTy {α} []

ex_tm_lam {γ, x, α, β, xEx, αEx, b} = P.buildExpr LamTm {γ, x, α, β} [xEx, αEx, b]
ex_tm_hole γ α αEx = P.buildExpr HoleTm {γ, α} [αEx]

-- shallow StepExpr

se_str x = P.buildStepExpr StrEL {x} [] 

se_var_zero γ x α = P.buildStepExpr SucVar {γ, x, α, loc: sr_loc_nonlocal} []
se_var_suc γ x α y β loc pred = P.buildStepExpr SucVar {γ, x, α, y, β, loc} [pred]

se_tm_lam x α β γ xExpr αExpr b = P.buildStepExpr LamTm {x, α, β, γ} [xExpr, αExpr, b]
se_tm_hole γ α αEx = P.buildStepExpr HoleTm {γ, α} [αEx]
