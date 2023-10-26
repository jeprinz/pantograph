module Pantograph.Specific.FSTLC where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Pantograph.Generic.Language as PL
import Pantograph.Library.Language.Change (getDiffChangingRule)
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
  -- Var
  = ZeroVar
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

  getDefaultExpr = todo ""

  steppingRules = todo ""

  getEdits = todo ""

  specialEdits = todo ""

  validGyro = todo ""

getSortingRule :: EL -> SortingRule
getSortingRule =
  let {str, jdg, ctx, ty, loc} = rsort in
  case _ of
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

    LetTerm -> PL.buildSortingRuleFromStrings ["s", "alpha", "beta", "gamma"] \[x, alpha, beta, gamma] ->
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

    DataTyEL dt -> PL.buildSortingRuleFromStrings ["alpha", "beta"] \[alpha, beta] ->
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

sort = {str, jdg, ctx, ty, loc}
  where
  str = \strInner -> PL.makeSort Str [strInner]
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

rsort = {str, jdg, ctx, ty, loc}
  where
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

expr = {var}
  where
  var = 
    { zero: \gamma x alpha -> PL.buildExpr ZeroVar {gamma, x, alpha} []
    , suc: \gamma x alpha y beta loc pred -> PL.buildExpr SucVar {gamma, x, alpha, y, beta, loc} [pred]
    , free: \x alpha -> PL.buildExpr FreeVar {x, alpha} [] }

  -- term =
  --   { lam: \x alpha beta gamma -> PL.buildExpr LamTerm {x, alpha, beta, gamma} [] 
  --   }
  