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
  -- Jdg
  | VarJdg -- Ctx Str Ty Loc
  | TermJdg -- Ctx Ty
  | NeutJdg -- Ctx Ty
  | TyJdg -- Ty
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
    VarJdg -> 4
    TermJdg -> 2
    NeutJdg -> 2
    TyJdg -> 1
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
      VarJdg -> ass \[gamma, x, alpha, loc] -> gamma <+> "⊢" <+> loc <+> x <+> ":" <+> alpha
      TermJdg -> ass \[gamma, alpha] -> gamma <+> "⊢" <+> alpha
      NeutJdg -> ass \[gamma, alpha] -> gamma <+> "⊢" <+> alpha
      TyJdg -> ass \[alpha] -> alpha
      NilCtx -> ass \[] -> "∅"
      ConsCtx -> ass \[x, alpha, gamma] -> parens (x <+> ":" <+> alpha) <> "," <+> gamma
      DataTySN dt -> ass \[] -> pretty dt
      ArrowTySN -> ass \[alpha, beta] -> alpha <+> "→" <+> beta
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
  | LamTerm -- Var Ty Term
  | LetTerm -- Var Ty Term Term
  | IfTerm -- Term Term Term
  | CallTerm -- Neut
  | ErrorCallTerm -- Neut
  | HoleTerm
  | ErrorBoundaryTerm -- Term
  -- Neut
  | VarNeut -- Var
  | AppNeut -- Neut Term
  | GrayedAppNeut -- Neut Term
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
    LamTerm -> 3
    LetTerm -> 4
    VarNeut -> 1
    IfTerm -> 3
    CallTerm -> 1
    ErrorCallTerm -> 1
    HoleTerm -> 0
    ErrorBoundaryTerm -> 1
    AppNeut -> 2
    GrayedAppNeut -> 2
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
      FreeVar -> ass \[x] -> "F"
      LamTerm -> ass \[x, alpha, b] -> parens $ "λ" <+> x <+> ":" <+> alpha <+> "." <+> b
      LetTerm -> ass \[x, alpha, a, b] -> parens $ "let" <+> x <+> ":" <+> alpha <+> "=" <+> a <+> "in" <+> b
      VarNeut -> ass \[x] -> "#" <> x
      IfTerm -> ass \[a, b, c] -> parens $ "if" <+> a <+> "then" <+> b <+> "else" <+> c
      CallTerm -> ass \[n] -> "Call" <> parens n
      ErrorCallTerm -> ass \[n] -> "ErroCall" <> parens n
      HoleTerm -> ass \[] -> "?"
      ErrorBoundaryTerm -> ass \[a] -> "ErrorBoundaryTerm" <> parens a
      AppNeut -> ass \[f, a] -> f <+> a
      GrayedAppNeut -> ass \[f, a] -> f <+> a
      HoleTy -> ass \[] -> "?"
      DataTyEL dt -> ass \[] -> pretty dt
      ArrowTyEL -> ass \[alpha, beta] -> parens $ alpha <+> "→" <+> beta
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
getSortingRule _ = todo "getSortingRule"
-- getSortingRule =
--   case _ of
--     StrEL -> PL.buildSortingRule (Proxy :: Proxy (x::_)) \{x {- : StrInner -}} ->
--       []
--       /\
--       ( rs_str x )

--     ZeroVar -> PL.buildSortingRule (Proxy :: Proxy (gamma::_, x::_, alpha::_)) \{gamma, x, alpha} ->
--       []
--       /\
--       ( rs_jg_var gamma x alpha rs_loc_local )

--     SucVar -> PL.buildSortingRuleFromStrings ["gamma", "x", "alpha", "y", "beta", "loc"] \[gamma, x, alpha, y, beta, loc] ->
--       [ rs_jg_var gamma x alpha loc ]
--       /\
--       ( rs_jg_var (rs_ctx_cons y beta gamma) x alpha loc )

--     FreeVar -> PL.buildSortingRuleFromStrings ["x", "alpha"] \[x, alpha] ->
--       []
--       /\
--       ( rs_jg_var rs_ctx_nil x alpha rs_loc_nonlocal )

--     LamTerm -> PL.buildSortingRuleFromStrings ["x", "alpha", "beta", "gamma"] \[x, alpha, beta, gamma] ->
--       [ rs_str x
--       , rs_jg_ty alpha
--       , rs_jg_tm (rs_ctx_cons x alpha gamma) beta ]
--       /\
--       ( rs_jg_tm gamma (rs_ty_arrow alpha beta) )

--     LetTerm -> PL.buildSortingRuleFromStrings ["x", "alpha", "beta", "gamma"] \[x, alpha, beta, gamma] ->
--       [ rs_str x
--       , rs_jg_ty alpha
--       , rs_jg_tm (rs_ctx_cons x alpha gamma) alpha
--       , rs_jg_tm (rs_ctx_cons x alpha gamma) beta
--       ]
--       /\
--       ( rs_jg_tm gamma beta )

--     VarNeut -> PL.buildSortingRuleFromStrings ["gamma", "x", "alpha", "loc"] \[gamma, x, alpha, loc] ->
--       [ rs_jg_var gamma x alpha loc ]
--       /\
--       ( rs_jg_ne gamma alpha )

--     IfTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
--       [ rs_jg_tm gamma rs_ty_bool
--       , rs_jg_tm gamma alpha ]
--       /\
--       ( rs_jg_tm gamma alpha )

--     CallTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
--       [ rs_jg_ne gamma alpha ]
--       /\
--       ( rs_jg_tm gamma alpha )

--     ErrorCallTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
--       [ rs_jg_ne gamma alpha ]
--       /\
--       ( rs_jg_tm gamma beta )

--     HoleTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
--       [] 
--       /\
--       ( rs_jg_tm gamma alpha )

--     ErrorBoundaryTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
--       [ rs_jg_tm gamma alpha ]
--       /\
--       ( rs_jg_tm gamma beta )

--     AppNeut -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
--       [ rs_jg_ne gamma (rs_ty_arrow alpha beta)
--       , rs_jg_tm gamma alpha ]
--       /\
--       ( rs_jg_ne gamma beta )

--     GrayedAppNeut -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
--       [ rs_jg_ne gamma beta
--       , rs_jg_tm gamma alpha ]
--       /\
--       ( rs_jg_ne gamma beta )

--     HoleTy -> PL.buildSortingRuleFromStrings ["alpha"] \[alpha] ->
--       [] 
--       /\
--       ( rs_jg_ty alpha )

--     DataTyEL dt -> PL.buildSortingRuleFromStrings [] \[] ->
--       []
--       /\
--       ( rs_jg_ty (rs_ty_dt dt) )

--     ArrowTyEL -> PL.buildSortingRuleFromStrings ["alpha", "beta"] \[alpha, beta] ->
--       [ rs_jg_ty alpha
--       , rs_jg_ty beta ]
--       /\
--       ( rs_jg_ty (rs_ty_arrow alpha beta) )

--     Format _ -> PL.buildSortingRuleFromStrings ["a"] \[a] -> 
--       []
--       /\
--       ( a )

getChangingRule :: EL -> ChangingRule
getChangingRule el = getDiffChangingRule {getSortingRule} el

-- BEGIN SteppingRules

steppingRules :: Array SteppingRule
steppingRules = []


-- | {e}↓{Var (+ <{ y : beta, {> gamma <}}>) x alpha loc}  ~~>  Suc {e}↓{Var gamma x alpha loc}
insertSucSteppingRule :: SteppingRule
insertSucSteppingRule = PL.SteppingRule case _ of
  PL.Boundary (PL.Down /\ (PL.SortNode VarJdg %! [Shift (Plus /\ (PL.SortNode ConsCtx %- 2 /\ [y, beta])) gamma, x, alpha, loc])) e -> Just $
    se_var_suc (epR gamma) (epR x) (epR alpha) y beta (epR loc) $
      PL.Boundary (PL.Down /\ InjectChange (PL.SortNode VarJdg) [gamma, x, alpha, loc]) e
  _ -> Nothing


-- | {Zero}↓{Var (- <{ x : alpha, {> gamma <}}>) x alpha Local} ~~> {Free}↑{Var id x alpha (Local ~> Nonlocal)}
localBecomesNonlocalSteppingRule :: SteppingRule
localBecomesNonlocalSteppingRule = PL.SteppingRule case _ of
  PL.Down /\ (PL.SortNode VarJdg %! [Minus /\ (PL.SortNode ConsCtx %- 2 /\ [x, alpha]) %!/ gamma, x', alpha', PL.SortNode LocalLoc %! []]) %.|
  (Nothing /\ PL.ExprNode ZeroVar _ _ %. []) 
  | true -> Just $
    PL.Boundary 
      (PL.Up /\ (PL.SortNode VarJdg %! [inject (epR gamma), x', alpha', Replace sr_loc_local sr_loc_nonlocal]))
      (inject $ freeVarTerm {gamma: (epR gamma), x, alpha})
  _ -> Nothing

-- | {Var (- <{ y : beta , {> gamma <}}>) x alpha loc}↓{Suc pred} ~~> {Var gamma x alpha loc}↓{pred}
removeSuc :: SteppingRule
removeSuc = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SortNode VarJdg %! [Minus /\ (PL.SortNode ConsCtx %- 1 /\ [_y, _beta]) %!/ gamma, x, alpha, loc])) %.| (_mrk /\ PL.ExprNode SucVar _sigma _ %. [pred]) -> Just $
    PL.Down /\ (PL.SortNode VarJdg %! [gamma, x, alpha, loc]) %.| pred
  _ -> Nothing


-- | {Var (+ <{ x : alpha , {> gamma< } }>) x alpha Nonlocal}↓{_} ~~> Z
nonlocalBecomesLocal :: SteppingRule
nonlocalBecomesLocal = PL.SteppingRule case _ of
  (PL.Down /\ (Plus /\ (PL.SortNode ConsCtx %- 2 /\ [x, alpha]) %!/ gamma)) %.| a -> Just $
    PL.inheritShallowMarker a $ se_var_zero (epR gamma) x alpha
  _ -> Nothing


-- | {alpha! -> beta!}↓{alpha -> beta} ~~> {alpha}↓{alpha!} -> {beta}↓{beta!}
passThroughArrow :: SteppingRule
passThroughArrow = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SortNode ArrowTySN %! [alpha, beta])) %.| (mrk /\ PL.ExprNode ArrowTyEL sigma _ %. [alphaCh, betaCh]) -> Just $
    mrk /\ PL.ExprNode ArrowTyEL sigma {} %. 
      [ PL.Down /\ alpha %.| alphaCh
      , PL.Down /\ beta %.| betaCh ]
  _ -> Nothing

-- | {_ : Type alpha!}↓{_} ~~> alpha
typeBecomesRhsOfChange :: SteppingRule
typeBecomesRhsOfChange = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SortNode TyJdg %! [alpha])) %.| _ -> Just $ inject (typeSortToTypeExpr (epR alpha))
  _ -> Nothing


-- | {Term gamma (+ <{alpha -> {> beta<}}>)}↓{b} ~~> lam ~ : alpha . {Term (+ <{ ~ : alpha, {> gamma <}}>) beta}↓{b}
wrapLambda :: SteppingRule
wrapLambda = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SortNode TermJdg %! [gamma, Plus /\ (PL.SortNode TermJdg %- 1 /\ [alpha]) %!/ beta])) %.| b -> Just $
    let x = sr_strInner "" in
    se_tm_lam x alpha (epR beta) (epR gamma) (se_str x) (inject (typeSortToTypeExpr alpha)) b
  _ -> Nothing

-- | {Term gamma (- <{alpha -> {> beta <}}>)}↓{lam x : alpha . b} ~~> {Term (- x : alpha, gamma) beta}↓{b}
unWrapLambda :: SteppingRule
unWrapLambda = PL.SteppingRule case _ of
  (PL.Down /\ (PL.SortNode TermJdg %! [gamma, Minus /\ (PL.SortNode ArrowTySN %- 1 /\ [alpha]) %!/ beta])) %.|
  (mrk /\ PL.ExprNode LamTerm sigma {} %. [x, alpha', b]) -> Just $
    PL.Down /\ (PL.SortNode TermJdg %! [Minus /\ (PL.SortNode ConsCtx %- 2 /\ [PL.getExprSort (PL.fromStepExprToExpr x), alpha]) %!/ gamma, beta]) %.|  b
  _ -> Nothing


-- | {f}↑{Term gamma (+ alpha -> beta)} ~~> {App f ?}↑{Term gamma beta}
wrapApp = PL.SteppingRule case _ of
  _ -> Nothing


-- | App {b}↑{Term gamma (- alpha -> beta)} a ~~> {b}_{Term gamma beta}
unWrapApp = PL.SteppingRule case _ of
  _ -> Nothing 


-- | App {f}↑{Neut gamma (alpha -> beta)} a ~~> {GrayedApp f a}↑{Neut gamma beta}
-- | App {f}↑{Neut gamma ((alpha -> beta) ~> ?delta)} a ~~> {GrayedApp f a}↑{Neut gamma {beta ~> ?delta}}
makeAppGrayed = PL.SteppingRule case _ of
  _ -> Nothing 


-- | GrayedApp f ? ~~> f
removeGrayedHoleArg = PL.SteppingRule case _ of
  _ -> Nothing


-- | GrayedApp {f}↑{Neut gamma (alpha -> beta)} a ~~> {App f a}↑{Neut gamma beta}
rehydrateApp = PL.SteppingRule case _ of
  _ -> Nothing


-- | {FunctionCall n}↑{Neut gamma alpha} ~~> {n}ErrorCall{Neut gamma alpha}
replaceCallWithError = PL.SteppingRule case _ of
  _ -> Nothing


-- | {n}ErrorCall{Neut gamma loop} ~~> FunctionCall n
replaceErrorWithCall = PL.SteppingRule case _ of
  _ -> Nothing


-- | FunctionCall ({n}↑{Neut gamma (alpha -> beta)}) ~~> {{FunctionCall n : alpha -> beta}ErrorBoundary{beta}}↑{Term gamma beta}
wrapCallInErrorUp = PL.SteppingRule case _ of
  _ -> Nothing

-- | {FunctionCall n}↓{Term gamma alpha} ~~> {FunctionCall gamma alpha}ErrorBoundary{...}
wrapCallInErrorDown = PL.SteppingRule case _ of
  _ -> Nothing


-- | {a : alpha}ErrorBoundary{alpha} ~~> a
removeError = PL.SteppingRule case _ of
  _ -> Nothing


-- | {{a : alpha}ErrorBoundary{beta}}ErrorBoundary{delta} ~~> {a : alpha}ErrorBoundary{delta}
mergeErrors = PL.SteppingRule case _ of
  _ -> Nothing

-- END SteppingRules

-- utils

freeVarTerm {gamma, x, alpha} = case gamma of
  Tree (PL.SortNode ConsCtx) [y, beta, gamma'] -> 
    sucVar {y, beta} $ freeVarTerm {gamma: gamma', x, alpha}
  Tree (PL.SortNode NilCtx) [] ->
    ex.var_free {x, alpha}
  _ -> bug "impossible: freeVarTerm"

sucVar {y, beta} pred 
  | Tree (PL.SortNode VarJdg) [gamma, x, alpha, loc] <- PL.getExprSort pred =
  ex.var_suc {gamma, x, alpha, y, beta, loc, pred}
sucVar _ _ = bug "impossible: sucVar"

--

getEdits :: Sort -> Orientation -> Edits
-- -- getEdits (Tree (PL.SortNode (StrInner _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- -- getEdits (Tree (PL.SortNode Str) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- -- getEdits (Tree (PL.SortNode VarJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits sr@(Tree (PL.SortNode TermJdg) [gamma, beta]) Outside = PL.Edits $ StringQuery.fuzzy
--   { toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0
--   , getItems: const
--       [ "lambda" /\
--       let alpha = sr_freshVar "alpha" in
--       let x = sr_strInner "" in
--       LibEdit.buildEditsFromExpr {splitExprPathChanges} 
--         (ex_tm_lam x alpha (sr_freshVar "beta") gamma 
--           (ex.str {x}) (ex_ty_hole alpha) (ex_tm_hole beta))
--         sr
--     ]
--   }
-- getEdits (Tree (PL.SortNode NeutJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode TyJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode (DataTySN _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode ArrowTySN) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits sort orientation = bug $ "invalid cursor position; sort = " <> show sort <> "; orientation = " <> show orientation

splitExprPathChanges :: SplitExprPathChanges
splitExprPathChanges = todo "type change goes up, context change goes down"

-- utility

typeSortToTypeExpr :: Sort -> Expr
typeSortToTypeExpr (PL.SortNode (DataTySN dt) % []) = PL.buildExpr (DataTyEL dt) {} []
typeSortToTypeExpr (PL.SortNode ArrowTySN % [alpha, beta]) = PL.buildExpr ArrowTyEL {alpha, beta} [typeSortToTypeExpr alpha, typeSortToTypeExpr beta]
typeSortToTypeExpr sr = bug $ "invalid: " <> show sr

-- strStepExprToStrSort :: StepExpr -> Sort
-- strStepExprToStrSort (_ /\ n@(PL.ExprNode StrEL _ _) %. _)
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

sr_jg_var gamma x alpha loc = PL.makeSort VarJdg [gamma, x, alpha, loc]
sr_jg_tm gamma alpha = PL.makeSort TermJdg [gamma, alpha]
sr_jg_ne gamma alpha = PL.makeSort NeutJdg [gamma, alpha]
sr_jg_ty alpha = PL.makeSort TyJdg [alpha]

sr_ctx_nil = PL.makeSort NilCtx []
sr_ctx_cons gamma x alpha = PL.makeSort ConsCtx [gamma, x, alpha]

sr_ty_dt dt = PL.makeSort DataTySN [dt]
sr_ty_arrow alpha beta = PL.makeSort ArrowTySN [alpha, beta]
sr_ty_bool = PL.makeSort (DataTySN BoolDataTy) []

sr_loc_local = PL.makeSort LocalLoc []
sr_loc_nonlocal = PL.makeSort NonlocalLoc []

-- shallow RuleSort

rs_freshVar var = PL.makeVarRuleSort var

rs_strInner string = PL.makeInjectRuleSort (StrInner string) []
rs_str strInner = PL.makeInjectRuleSort Str [strInner]

rs_jg_var gamma x alpha loc = PL.makeInjectRuleSort VarJdg [gamma, x, alpha, loc]
rs_jg_tm gamma alpha = PL.makeInjectRuleSort TermJdg [gamma, alpha]
rs_jg_ne gamma alpha = PL.makeInjectRuleSort NeutJdg [gamma, alpha]
rs_jg_ty alpha = PL.makeInjectRuleSort TyJdg [alpha]

rs_ctx_nil = PL.makeInjectRuleSort NilCtx []
rs_ctx_cons gamma x alpha = PL.makeInjectRuleSort ConsCtx [gamma, x, alpha]

rs_ty_dt dt = PL.makeInjectRuleSort (DataTySN dt) []
rs_ty_arrow alpha beta = PL.makeInjectRuleSort ArrowTySN [alpha, beta]
rs_ty_bool = PL.makeInjectRuleSort (DataTySN BoolDataTy) []

rs_loc_local = PL.makeInjectRuleSort LocalLoc []
rs_loc_nonlocal = PL.makeInjectRuleSort NonlocalLoc []

-- shallow Expr

ex = buildExprShallowSyntax (Proxy :: Proxy SN) (Proxy :: Proxy ()) exprNodes

exprNodes =  
  ((Proxy :: Proxy "str") /\ \{x} -> PL.buildExpr StrEL {x} []) :
  ((Proxy :: Proxy "var_zero") /\ \{gamma, x, alpha} -> PL.buildExpr ZeroVar {gamma, x, alpha} []) :
  ((Proxy :: Proxy "var_suc") /\ \{gamma, x, alpha, y, beta, loc, pred} -> PL.buildExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]) :
  ((Proxy :: Proxy "var_free") /\ \{x, alpha} -> PL.buildExpr FreeVar {x, alpha} []) :
  nil

-- ex_str x = PL.buildExpr StrEL {x} [] 

-- ex_var_zero gamma x alpha = PL.buildExpr ZeroVar {gamma, x, alpha} []
-- ex_var_suc gamma x alpha y beta loc pred = PL.buildExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]
-- ex_var_free x alpha = PL.buildExpr FreeVar {x, alpha} []

ex_ty_hole alpha = PL.buildExpr HoleTy {alpha} []

ex_tm_lam x alpha beta gamma xExpr alphaExpr b = PL.buildExpr LamTerm {x, alpha, beta, gamma} [xExpr, alphaExpr, b]
ex_tm_hole alpha = PL.buildExpr HoleTerm {alpha} []

-- shallow StepExpr

se_str x = PL.buildStepExpr StrEL {x} [] 

se_var_zero gamma x alpha = PL.buildStepExpr SucVar {gamma, x, alpha, loc: sr_loc_nonlocal} []
se_var_suc gamma x alpha y beta loc pred = PL.buildStepExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]

se_tm_lam x alpha beta gamma xExpr alphaExpr b = PL.buildStepExpr LamTerm {x, alpha, beta, gamma} [xExpr, alphaExpr, b]
