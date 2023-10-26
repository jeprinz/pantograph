module Pantograph.Specific.FSTLC where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Eq.Generic (genericEq)
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.StringQuery as StringQuery
import Data.Subtype (inject)
import Data.Tuple (fst)
import Pantograph.Generic.Language as PL
import Pantograph.Library.Language.Change (getDiffChangingRule)
import Pantograph.Library.Language.Edit as LibEdit
import Text.Pretty (class Pretty, parens, pretty, quotes, (<+>))
import Todo (todo)
import Type.Proxy (Proxy(..))

-- types

type Expr = PL.Expr SN EL
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

  topSort = sort.jdg.term sort.ctx.nil (todo "sort metavar")

  getDefaultExpr = todo "getDefaultExpr"

  steppingRules = steppingRules

  getEdits sr ori = getEdits sr ori

  specialEdits = todo "specialEdits"

  validGyro = todo "validGyro"

getSortingRule :: EL -> SortingRule
getSortingRule =
  let {str, jdg, ctx, ty, loc} = rsort in
  case _ of
    StrEL -> PL.buildSortingRule (Proxy :: Proxy (x::_)) \{x {- : StrInner -}} ->
      []
      /\
      ( str x )

    ZeroVar -> PL.buildSortingRule (Proxy :: Proxy (gamma::_, x::_, alpha::_)) \{gamma, x, alpha} ->
      []
      /\
      ( jdg.var gamma x alpha loc.local )

    SucVar -> PL.buildSortingRuleFromStrings ["gamma", "x", "alpha", "y", "beta", "loc"] \[gamma, x, alpha, y, beta, loc] ->
      [ jdg.var gamma x alpha loc ]
      /\
      ( jdg.var (ctx.cons y beta gamma) x alpha loc )

    FreeVar -> PL.buildSortingRuleFromStrings ["x", "alpha"] \[x, alpha] ->
      []
      /\
      ( jdg.var ctx.nil x alpha loc.nonlocal )

    LamTerm -> PL.buildSortingRuleFromStrings ["x", "alpha", "beta", "gamma"] \[x, alpha, beta, gamma] ->
      [ str x
      , jdg.ty alpha
      , jdg.term (ctx.cons x alpha gamma) beta ]
      /\
      ( jdg.term gamma (ty.arrow alpha beta) )

    LetTerm -> PL.buildSortingRuleFromStrings ["x", "alpha", "beta", "gamma"] \[x, alpha, beta, gamma] ->
      [ str x
      , jdg.ty alpha
      , jdg.term (ctx.cons x alpha gamma) alpha
      , jdg.term (ctx.cons x alpha gamma) beta
      ]
      /\
      ( jdg.term gamma beta )

    VarNeut -> PL.buildSortingRuleFromStrings ["gamma", "x", "alpha", "loc"] \[gamma, x, alpha, loc] ->
      [ jdg.var gamma x alpha loc ]
      /\
      ( jdg.neut gamma alpha )

    IfTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
      [ jdg.term gamma ty.bool
      , jdg.term gamma alpha ]
      /\
      ( jdg.term gamma alpha )

    CallTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
      [ jdg.neut gamma alpha ]
      /\
      ( jdg.term gamma alpha )

    ErrorCallTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
      [ jdg.neut gamma alpha ]
      /\
      ( jdg.term gamma beta )

    HoleTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha"] \[gamma, alpha] ->
      [] 
      /\
      ( jdg.term gamma alpha )

    ErrorBoundaryTerm -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
      [ jdg.term gamma alpha ]
      /\
      ( jdg.term gamma beta )

    AppNeut -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
      [ jdg.neut gamma (ty.arrow alpha beta)
      , jdg.term gamma alpha ]
      /\
      ( jdg.neut gamma beta )

    GrayedAppNeut -> PL.buildSortingRuleFromStrings ["gamma", "alpha", "beta"] \[gamma, alpha, beta] ->
      [ jdg.neut gamma beta
      , jdg.term gamma alpha ]
      /\
      ( jdg.neut gamma beta )

    HoleTy -> PL.buildSortingRuleFromStrings ["alpha"] \[alpha] ->
      [] 
      /\
      ( jdg.ty alpha )

    DataTyEL dt -> PL.buildSortingRuleFromStrings [] \[] ->
      []
      /\
      ( jdg.ty (ty.dt dt) )

    ArrowTyEL -> PL.buildSortingRuleFromStrings ["alpha", "beta"] \[alpha, beta] ->
      [ jdg.ty alpha
      , jdg.ty beta ]
      /\
      ( jdg.ty (ty.arrow alpha beta) )

    Format _ -> PL.buildSortingRuleFromStrings ["a"] \[a] -> 
      []
      /\
      ( a )

getChangingRule :: EL -> ChangingRule
getChangingRule el = getDiffChangingRule {getSortingRule} el

-- BEGIN SteppingRules

steppingRules :: Array SteppingRule
steppingRules = []

-- {e}↓{Var ( +<{ y : beta, {> gamma <} }> ) x alpha loc}  ~~>  Suc {e}↓{Var gamma x alpha loc}
insertSucSteppingRule :: SteppingRule
insertSucSteppingRule = PL.SteppingRule case _ of
  PL.Boundary PL.Down (InjectChange (PL.SortNode VarJdg) [Shift (Plus /\ (Tooth (PL.SortNode ConsCtx) 2 [y, beta])) gamma, x, alpha, loc]) e -> Just $
    step.var.suc (endpoints gamma).right (endpoints x).right (endpoints alpha).right y beta (endpoints loc).right $
      PL.Boundary PL.Down (InjectChange (PL.SortNode VarJdg) [gamma, x, alpha, loc]) e
  _ -> Nothing

-- {Zero}↓{Var ( -<{ x : alpha, {> gamma <} }> ) x alpha Local} ~~> {Free}↑{Var id x alpha (Local ~> Nonlocal)}
localBecomesNonlocal :: SteppingRule
localBecomesNonlocal = PL.SteppingRule case _ of
  PL.Boundary PL.Down (InjectChange (PL.SortNode VarJdg) [Shift (Minus /\ (Tooth (PL.SortNode ConsCtx) 2 [x, alpha])) gamma, x', alpha', InjectChange (PL.SortNode LocalLoc) []]) 
  (PL.StepExpr Nothing (PL.ExprNode ZeroVar _ _) []) 
  | true -> Just $
    PL.Boundary PL.Up
      (InjectChange (PL.SortNode VarJdg) [inject (endpoints gamma).right, x', alpha', Replace sort.loc.local sort.loc.nonlocal])
      (inject $ freeVarTerm {gamma: (endpoints gamma).right, x, alpha})
  _ -> Nothing

freeVarTerm {gamma, x, alpha} = case gamma of
  Tree (PL.SortNode ConsCtx) [y, beta, gamma'] -> 
    sucVar {y, beta} $ freeVarTerm {gamma: gamma', x, alpha}
  Tree (PL.SortNode NilCtx) [] ->
    expr.var.free x alpha
  _ -> todo ""

sucVar {y, beta} e | Tree (PL.SortNode VarJdg) [gamma, x, alpha, loc] <- PL.getExprSort e =
  expr.var.suc gamma x alpha y beta loc e
sucVar _ _ = bug "invalid"

-- END SteppingRules

getEdits :: Sort -> Orientation -> Edits
-- getEdits (Tree (PL.SortNode (StrInner _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode Str) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
-- getEdits (Tree (PL.SortNode VarJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: todo "", toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits sr@(Tree (PL.SortNode TermJdg) [gamma, beta]) Outside = PL.Edits $ StringQuery.fuzzy
  { toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0
  , getItems: const
      [ "lambda" /\
      let alpha = sort.freshVar "alpha" in
      let x = sort.strInner "" in
      LibEdit.buildEditsFromExpr {splitExprPathChanges} 
        (expr.term.lam x alpha (sort.freshVar "beta") gamma 
          (expr.str x) (expr.ty.hole alpha) (expr.term.hole beta))
        sr
    ]
  }
getEdits (Tree (PL.SortNode NeutJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits (Tree (PL.SortNode TyJdg) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits (Tree (PL.SortNode (DataTySN _)) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits (Tree (PL.SortNode ArrowTySN) []) Outside = PL.Edits $ StringQuery.fuzzy { getItems: const [], toString: fst, maxPenalty: Fuzzy.Distance 1 0 0 0 0 0 }
getEdits sort orientation = bug $ "invalid cursor position; sort = " <> show sort <> "; orientation = " <> show orientation

splitExprPathChanges :: SplitExprPathChanges
splitExprPathChanges = todo "type change goes up, context change goes down"

sort = {strInner, str, freshVar, jdg, ctx, ty, loc}
  where
  strInner = \string -> PL.makeSort (StrInner string) []
  str = \strInner -> PL.makeSort Str [strInner]
  freshVar = PL.freshVarSort
  jdg =
    { var: \gamma x alpha loc -> PL.makeSort VarJdg [gamma, x, alpha, loc]
    , term: \gamma alpha -> PL.makeSort TermJdg [gamma, alpha]
    , neut: \gamma alpha -> PL.makeSort NeutJdg [gamma, alpha]
    , ty: \alpha -> PL.makeSort TyJdg [alpha] }
  ctx =
    { nil: PL.makeSort NilCtx []
    , cons: \gamma x alpha -> PL.makeSort ConsCtx [gamma, x, alpha] }
  ty =
    { dt: \dt -> PL.makeSort DataTySN [dt]
    , arrow: \alpha beta -> PL.makeSort ArrowTySN [alpha, beta]
    , bool: PL.makeSort (DataTySN BoolDataTy) [] }
  loc =
    { local: PL.makeSort LocalLoc []
    , nonlocal: PL.makeSort NonlocalLoc [] }

rsort = {strInner, str, jdg, ctx, ty, loc}
  where
  strInner = \string -> PL.makeInjectRuleSort (StrInner string)
  str = \strInner -> PL.makeInjectRuleSort Str [strInner]
  jdg =
    { var: \gamma x alpha loc -> PL.makeInjectRuleSort VarJdg [gamma, x, alpha, loc]
    , term: \gamma alpha -> PL.makeInjectRuleSort TermJdg [gamma, alpha]
    , neut: \gamma alpha -> PL.makeInjectRuleSort NeutJdg [gamma, alpha]
    , ty: \alpha -> PL.makeInjectRuleSort TyJdg [alpha] }
  ctx =
    { nil: PL.makeInjectRuleSort NilCtx []
    , cons: \gamma x alpha -> PL.makeInjectRuleSort ConsCtx [gamma, x, alpha] }
  ty =
    { dt: \dt -> PL.makeInjectRuleSort (DataTySN dt) []
    , arrow: \alpha beta -> PL.makeInjectRuleSort ArrowTySN [alpha, beta]
    , bool: PL.makeInjectRuleSort (DataTySN BoolDataTy) [] }
  loc =
    { local: PL.makeInjectRuleSort LocalLoc []
    , nonlocal: PL.makeInjectRuleSort NonlocalLoc [] }

expr = {var, str, ty, term}
  where
  str x = PL.buildExpr StrEL {x} [] 
  var = 
    { zero: \gamma x alpha -> PL.buildExpr ZeroVar {gamma, x, alpha} []
    , suc: \gamma x alpha y beta loc pred -> PL.buildExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]
    , free: \x alpha -> PL.buildExpr FreeVar {x, alpha} [] }

  ty = 
    { hole: \alpha -> PL.buildExpr HoleTy {alpha} [] }

  term =
    { lam: \x alpha beta gamma xExpr alphaExpr b -> PL.buildExpr LamTerm {x, alpha, beta, gamma} [xExpr, alphaExpr, b]
    , hole: \alpha -> PL.buildExpr HoleTerm {alpha} []
    }

step = 
  { var:
      { suc: \gamma x alpha y beta loc pred -> PL.buildStepExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]
      }
  -- , str
  -- , ty
  -- , term 
  }
