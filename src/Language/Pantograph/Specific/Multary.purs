module Language.Pantograph.Specific.Multary where

import Prelude

import Language.Pantograph.Generic.Grammar
import Data.Expr
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Smallstep (wrapBoundary, Direction(..))

import Data.Expr as Expr
import Data.List.Zip as Z
import Language.Pantograph.Generic.Grammar as Grammar
import Data.Tuple.Nested ((/\))
import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lazy (defer)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Map as Map
import Data.Set as Set
import Data.Variant (Variant)
import Debug (traceM, trace)
import Debug as Debug
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML as HH
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule, newTermFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Console (logConsole)
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep ((%+-), dPLUS, dMINUS, (%#))
import Language.Pantograph.Generic.Smallstep (StepExprLabel(..), cSlot, dTERM)
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Generic.Unification (unify)
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust)
import Util as Util
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Language.Pantograph.Lib.GreyedRules as GreyedRules
import Partial.Unsafe (unsafePartial)

data DataType
    = Bool
    | String
    | Int

derive instance Generic DataType _
instance Show DataType where show x = genericShow x
instance Eq DataType where eq x = genericEq x
instance Ord DataType where compare x y = genericCompare x y
instance Enum DataType where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded DataType where
  bottom = genericBottom
  top = genericTop
instance Pretty DataType where
  pretty = show

data PreSortLabel
  -- Judgements
  = VarSort {-Ctx-} {-String-} {-Type-} {-Local or NonLocal-}
  | TermSort {-Ctx-} {-Type-}
  | ArgListSort {-Ctx-} {-TypeList-}
  | VarListSort {-Ctx-} {-Ctx-} {-TypeList-}
  | TypeSort {-Type-}
  | TypeListSort
  -- Contexts
  | CtxConsSort {-String-} {-Type-} {-Ctx-}
  | CtxNilSort
  -- Types
  | DataType DataType
  | Arrow {-TypeList-} {-Type-}
  -- TypeList
  | TypeListNil
  | TypeListCons {-Type-} {-TypeList-}
  -- Locality
  | Local
  | NonLocal

derive instance Generic PreSortLabel _
instance Show PreSortLabel where show x = genericShow x
instance Eq PreSortLabel where eq x = genericEq x
instance Ord PreSortLabel where compare x y = genericCompare x y

instance Pretty PreSortLabel where
  pretty = show

instance IsExprLabel PreSortLabel where
  prettyExprF'_unsafe (VarSort /\ [gamma, x, ty, locality]) = "Var" <+> parens gamma <+> x <+> ty <+> "(" <> show locality <> ")"
  prettyExprF'_unsafe (TermSort /\ [gamma, ty]) = "Term" <+> parens gamma <+> ty
  prettyExprF'_unsafe (ArgListSort /\ [gamma, typeList]) = "ArgList" <+> parens gamma <+> typeList
  prettyExprF'_unsafe (VarListSort /\ [gammaIn, gammaOut, typeList]) = "VarList" <+> parens gammaIn <+> parens gammaOut <+> typeList
  prettyExprF'_unsafe (TypeListSort  /\ [ts]) = "TypeList" <+> parens ts
  prettyExprF'_unsafe (TypeSort /\ [t]) = "Type" <+> parens t
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, "∅"]) = x <> ":" <> ty
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, gamma]) = x <> ":" <> ty <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"
  prettyExprF'_unsafe (Local /\ []) = "Local"
  prettyExprF'_unsafe (NonLocal /\ []) = "NonLocal"
  prettyExprF'_unsafe (DataType ty /\ []) = show ty
  prettyExprF'_unsafe (Arrow  /\ [a, b]) = "(" <> a <> ") -> " <> b
  prettyExprF'_unsafe (TypeListNil  /\ []) = "[]"
  prettyExprF'_unsafe (TypeListCons  /\ [ty, tylist]) = ty <> " : " <> tylist
  prettyExprF'_unsafe sort = bug ("prettySort: pattern match failed " <> pretty sort)


  expectedKidsCount VarSort = 4
  expectedKidsCount TermSort = 2
  expectedKidsCount ArgListSort = 2
  expectedKidsCount VarListSort = 3
  expectedKidsCount TypeSort = 1
  expectedKidsCount TypeListSort = 1
  expectedKidsCount CtxConsSort = 3
  expectedKidsCount CtxNilSort = 0
  expectedKidsCount Local = 0
  expectedKidsCount NonLocal = 0
  expectedKidsCount (DataType _) = 0
  expectedKidsCount Arrow = 2
  expectedKidsCount TypeListNil = 0
  expectedKidsCount TypeListCons = 2

--------------------------------------------------------------------------------
-- Shorter Aliases
--------------------------------------------------------------------------------

-- Expr
type Expr = Expr.Expr PreSortLabel
type MetaExpr = Expr.MetaExpr PreSortLabel
type Zipper = Expr.Zipper PreSortLabel
type Tooth = Expr.Tooth PreSortLabel
type Sort = Grammar.Sort PreSortLabel

-- Grammar
type DerivTerm = Grammar.DerivTerm PreSortLabel RuleLabel
type DerivLabel = Grammar.DerivLabel PreSortLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir PreSortLabel RuleLabel
type DerivZipper = Grammar.DerivZipper PreSortLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp PreSortLabel RuleLabel
type SSTerm = Smallstep.SSTerm PreSortLabel RuleLabel
type LanguageChanges = Grammar.LanguageChanges PreSortLabel RuleLabel
type SortChange = Grammar.SortChange PreSortLabel
type ChangeRule = Grammar.ChangeRule PreSortLabel

-- Rendering
type Query = Base.Query
type Output = Base.Output PreSortLabel RuleLabel
type HoleyDerivZipper = Base.HoleyDerivZipper PreSortLabel RuleLabel

type Edit = Edit.Edit PreSortLabel RuleLabel

-- SmallStep
type StepRule = Smallstep.StepRule PreSortLabel RuleLabel
--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

-- | Naming convention: <title>_<output sort>
data RuleLabel
  = Zero
  | Suc
  | Lam
  | VarListNil
  | VarListCons
  | Let
  | ArgListNil
  | ArgListCons
  | GreyedArgListCons
  | FunctionCall
  | LocalVar
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
  | TypeListNilRule
  | TypeListConsRule
  | Newline -- TODO: is this really an acceptable way for newlines to work? Its broken for applications, isn't it?
  | If -- TODO: should this be generalized in any way? Maybe for any type? For now I'll just do if.
  -- TODO: how exactly do I want errors to work here with n-ary functions?
  | ErrorCall

derive instance Generic RuleLabel _
derive instance Eq RuleLabel
derive instance Ord RuleLabel
instance Show RuleLabel where show x = genericShow x
instance Enum RuleLabel where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded RuleLabel where
  bottom = genericBottom
  top = genericTop

instance Pretty RuleLabel where
  pretty = show

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language PreSortLabel RuleLabel
type Rule = Grammar.Rule PreSortLabel

sortToType :: Sort -> DerivTerm
sortToType (Expr (MInj (SInj (DataType dt))) []) =
    makeLabel (DataTypeRule dt) [] % []
sortToType (Expr (MInj (SInj Arrow)) [a, b]) =
    makeLabel ArrowRule ["a" /\ a, "b" /\ b] % [sortToTypeList a, sortToType b]
sortToType ty =
    makeLabel TypeHole ["type" /\ ty] % []

sortToArgList :: {-Ctx-}Sort -> {-TyList-}Sort -> DerivTerm
sortToArgList gamma ts
    | Just _ <- unify ts (MInj (SInj TypeListNil) % [])
    = makeLabel ArgListNil ["gamma" /\ gamma] % []
sortToArgList gamma (MInj (SInj TypeListCons) % [t, ts])
    = makeLabel ArgListCons ["gamma" /\ gamma, "t" /\ t, "ts" /\ ts] % [sortToType t, sortToArgList gamma ts]
sortToArgList _ _ = bug "invalid input to sortToArgList"

sortToVarList :: {-Ctx-}Sort -> {-Ctx-}Sort -> {-TyList-}Sort -> DerivTerm
sortToVarList gammaLess gammaMore ts
    | Just _ <- unify gammaLess gammaMore
    , Just _ <- unify ts (MInj (SInj TypeListNil) % [])
    = makeLabel VarListNil ["gamma" /\ gammaLess] % []
sortToVarList gammaLess (MInj (SInj CtxConsSort) % [t', gammaMore]) (MInj (SInj TypeListCons) % [t, ts])
    =
        makeLabel VarListCons ["name" /\ StringSortLabel "" %* [], "gammaLess" /\ gammaLess
        , "gammaMore" /\ gammaMore, "t" /\ t, "ts" /\ ts] % [sortToType t, sortToVarList gammaLess gammaMore ts]
sortToVarList gammaLess gammaMore ts = bug ("invalid input to sortToVarList: " <> pretty gammaLess <> " " <> pretty gammaMore <> " " <> pretty ts <> " " <> pretty (unify gammaLess gammaMore))

sortToTypeList :: {-TyList-}Sort -> DerivTerm
sortToTypeList ts
    | Just _ <- unify ts (MInj (SInj TypeListNil) % [])
    = makeLabel TypeListNilRule [] % []
sortToTypeList (MInj (SInj TypeListCons) % [t, ts])
    = makeLabel TypeListConsRule ["t" /\ t, "ts" /\ ts] % [sortToType t, sortToTypeList ts]
sortToTypeList _ = bug "invalid input to sortToTypeList"

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = pretty Zero
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = pretty Suc <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [ts, b]) = P.parens $ "λ" <+> ts <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (Let /\ [x, ty, a, b]) = P.parens $ "let" <+> x <+> ":" <+> ty <+> "=" <+> a <+> b
  prettyExprF'_unsafe_RuleLabel (ArrowRule /\ [a, b]) = P.parens $ a <+> "->" <+> b
  prettyExprF'_unsafe_RuleLabel (DataTypeRule dataType /\ []) = pretty dataType
  prettyExprF'_unsafe_RuleLabel (ArgListNil /\ []) = "[]"
  prettyExprF'_unsafe_RuleLabel (ArgListCons /\ [arg, rest]) = arg <+> ":" <+> rest
  prettyExprF'_unsafe_RuleLabel (VarListNil /\ []) = "[]"
  prettyExprF'_unsafe_RuleLabel (VarListCons /\ [name, rest]) = name <+> ":" <+> rest
  prettyExprF'_unsafe_RuleLabel (TypeListNilRule /\ []) = "[]"
  prettyExprF'_unsafe_RuleLabel (TypeListConsRule /\ [t, ts]) = t <+> ":" <+> ts
  prettyExprF'_unsafe_RuleLabel (GreyedArgListCons /\ [arg, rest]) = arg <+> ":" <+> rest
  prettyExprF'_unsafe_RuleLabel (LocalVar /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ [ty]) = "(? : " <> ty <> ")"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel (FunctionCall /\ [neu]) = "call" <+> P.parens neu
  prettyExprF'_unsafe_RuleLabel (If /\ [c, t, e]) = "if" <+> c <+> "then" <+> t <+> "else" <+> e
  prettyExprF'_unsafe_RuleLabel (ErrorCall /\ [t]) = "{{" <+> t <+> "}}"
  prettyExprF'_unsafe_RuleLabel other = bug ("[prettyExprF'...] the input was: " <> show other)

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    TypeHole -> true
    _ -> false

  defaultDerivTerm' (MInj (SInj TermSort) % [gamma, ty])
    = pure (makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [sortToType ty])
  defaultDerivTerm' (MInj (SInj VarSort) % [_gamma, _x, _ty, _locality]) = Nothing
  defaultDerivTerm' (MInj (SInj TypeSort) % [ty]) =
    pure $ sortToType ty
  defaultDerivTerm' (MInj (SInj ArgListSort) % [gamma, ts]) = pure $ sortToArgList gamma ts
  defaultDerivTerm' (MInj (SInj VarListSort) % [gammaLess, gammaMore, ts]) = pure $ sortToVarList gammaLess gammaMore ts
  defaultDerivTerm' (MInj (SInj TypeListSort) % [ts]) = pure $ sortToTypeList ts
  -- TODO: This case should probably be in Generic.
  defaultDerivTerm' (MInj (NameSortLabel) % [_]) = pure $ DerivString "" % [] -- TODO: this case should be in generic rather than here. In other words, the defaultDerivTerm in Grammar should do this case, and only hand the language specific cases to this function.
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

--appRule :: Rule
--appRule = makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
--    [ NeutralSort %|-* [gamma, Arrow %|-* [a, b]]
--    , TermSort %|-* [gamma, a] ]
--    /\ --------
--    ( NeutralSort %|-* [gamma, b] )

language :: Language
language = TotalMap.makeTotalMap case _ of

  Zero -> Grammar.makeRule ["gamma", "x", "type"] \[gamma, x, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxConsSort %|-* [x, ty, gamma], x, ty, Local %|-* []] )

  Suc -> Grammar.makeRule ["gamma", "x", "typeX", "y", "typeY", "locality"] \[gamma, x, typeX, y, typeY, locality] ->
    [ VarSort %|-* [gamma, x, typeX, locality] ]
    /\ --------
    ( VarSort %|-* [CtxConsSort %|-* [y, typeY, gamma], x, typeX, locality] )

  FunctionCall -> Grammar.makeRule ["gamma", "args", "out"] \[gamma, args, out] ->
    [ TermSort %|-* [gamma, Arrow %|-* [args, out]]
    , ArgListSort %|-* [gamma, args]]
    /\ -------
    ( TermSort %|-* [gamma, out] )

  ErrorCall -> Grammar.makeRule ["gamma", "args", "outVar", "outRes"] \[gamma, args, outVar, outRes] ->
    [ TermSort %|-* [gamma, Arrow %|-* [args, outVar]]
    , ArgListSort %|-* [gamma, args]]
    /\ -------
    ( TermSort %|-* [gamma, outRes] )

  Lam -> Grammar.makeRule ["a", "b", "gammaInside", "gammaOutside"] \[a, b, gammaInside, gammaOutside] ->
    [ VarListSort %|-* [gammaOutside, gammaInside, a]
    , TermSort %|-* [gammaInside, b] ]
    /\ --------
    ( TermSort %|-* [gammaOutside, Arrow %|-* [a, b]])

  VarListNil -> Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    ( VarListSort %|-* [gamma, gamma, TypeListNil %|-* []] )

  VarListCons -> Grammar.makeRule ["name", "gammaLess", "gammaMore", "t", "ts"] \[name, gammaLess, gammaMore, t, ts] ->
    [ Grammar.NameSortLabel %* [name]
    , VarListSort %|-* [gammaLess, gammaMore, ts] ]
    /\ -------
    ( VarListSort %|-* [gammaLess, CtxConsSort %|-* [name, t, gammaMore] , TypeListCons %|-* [t, ts]] )

  ArgListNil -> Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    ( ArgListSort %|-* [gamma, TypeListNil %|-* []] )

  ArgListCons -> Grammar.makeRule ["gamma", "t", "ts"] \[gamma, t, ts] ->
    [ TermSort %|-* [gamma, t]
    , ArgListSort %|-* [gamma, ts]]
    /\ -------
    ( ArgListSort %|-* [gamma, TypeListCons %|-* [t, ts]] )

  GreyedArgListCons -> Grammar.makeRule ["gamma", "t", "ts"] \[gamma, t, ts] ->
    [ TermSort %|-* [gamma, t]
    , ArgListSort %|-* [gamma, ts]]
    /\ -------
    ( ArgListSort %|-* [gamma, TypeListCons %|-* [ts]] )

  FreeVar -> Grammar.makeRule ["name", "type"] \[name, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, ty, NonLocal %|-* []] )

  LocalVar -> Grammar.makeRule ["gamma", "x", "type", "locality"] \[gamma, x, ty, locality] ->
    [ VarSort %|-* [gamma, x, ty, locality] ]
    /\ --------
    ( TermSort %|-* [gamma, ty] )

  TermHole -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ TypeSort %|-* [ty] ]
    /\ --------
    ( TermSort %|-* [gamma, ty] )

  Newline -> Grammar.makeRule ["s"] \[s] ->
    [ s ]
    /\ --------
    ( s )

  Let -> Grammar.makeRule ["x", "a", "b", "gamma"] \[x, a, b, gamma] ->
    [ Grammar.NameSortLabel %* [x]
    , TypeSort %|-* [a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], b] ]
    /\ --------
    ( TermSort %|-* [gamma, b])

  TypeHole -> Grammar.makeRule ["type"] \[ty] ->
    [ ]
    /\ --------
    ( TypeSort %|-* [ty] )

  (DataTypeRule dataType) -> Grammar.makeRule [] \[] ->
    []
    /\ --------
    ( TypeSort %|-* [DataType dataType %|-* []] )

  ArrowRule -> Grammar.makeRule ["a", "b"] \[a, b] ->
    [TypeListSort %|-* [a], TypeSort %|-* [b]]
    /\ --------
    ( TypeSort %|-* [Arrow %|-* [a, b]] )

  TypeListNilRule -> Grammar.makeRule [] \[] ->
    []
    /\ --------
    ( TypeListSort %|-* [TypeListNil %|-* []] )

  TypeListConsRule -> Grammar.makeRule ["t", "ts"] \[t, ts] ->
    [TypeSort %|-* [t], TypeListSort %|-* [ts]]
    /\ --------
    ( TypeListSort %|-* [TypeListCons %|-* [t, ts]] )

  If -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
      [ TermSort %|-* [gamma, DataType Bool %|-* []]
      , TermSort %|-* [gamma, ty]
      , TermSort %|-* [gamma, ty] ]
      /\
      ( TermSort %|-* [gamma, ty] )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx, rule, sort, sigma} = case rule /\ sort of
  _ /\ (MInj (SInj VarSort) %
    [ _gamma
    , MInj (StringSortLabel str) % []
    , _ty
    , locality ]) ->
    -- TODO: use locality in rendering?
    let postfix = if locality == sor Local % [] then "" else "!" in
    [pure [nameElem (str <> postfix)]]
  -- term
  FreeVar /\ _ ->
    [Left (renCtx /\ 0)]
  LocalVar /\ _ ->
    [Left (renCtx /\ 0)]
  Lam /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem, lambdaElem], Left (renCtx /\ 0), pure [mapstoElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
  Let /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [letElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem]
        , pure (if renCtx.isInlined then [] else newlineIndentElem (renCtx.indentationLevel))
        , Left (renCtx /\ 3)]
  ArgListNil /\ _ ->
    [pure [HH.text "[]"]]
  ArgListCons /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx /\ 0), pure [Rendering.commaElem], Left (renCtx' /\ 1)]
  VarListNil /\ _ ->
    [pure [HH.text "[]"]]
  VarListCons /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx /\ 0), pure [Rendering.commaElem], Left (renCtx' /\ 1)]
  TypeListNilRule /\ _ ->
    [pure [HH.text "[]"]]
  TypeListConsRule /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx /\ 0), pure [Rendering.commaElem], Left (renCtx' /\ 1)]
  GreyedArgListCons /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [HH.text "<"], Left (renCtx /\ 0), pure [HH.text ">", Rendering.commaElem], Left (renCtx' /\ 1)]
  FunctionCall /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx /\ 0), pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.rparenElem]]
  ErrorCall /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [errorLeftSide], Left (renCtx /\ 0), pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.rparenElem, errorRightSide]]
  DataTypeRule dataType /\ _ ->
    [pure [dataTypeElem (pretty dataType)]]
  ArrowRule /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx' /\ 0), pure [arrowElem], Left (renCtx' /\ 1)]
  -- format
  Newline /\ _ ->
    Array.concat
      [ if renCtx.isInlined then [] else [pure (newlineIndentElem renCtx.indentationLevel)]
      , [Left (renCtx /\ 0)] ]
  -- hole
  TermHole /\ (MInj (SInj TermSort) % [_gamma, ty])
    ->  [pure [Rendering.lbraceElem], Left (renCtx /\ 0), pure [colonElem]
        , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1)
        , pure [Rendering.rbraceElem]]
--  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [colonElem, typeElem]]
  -- only has inner hole? So messes up keyboard cursor movement. TODO: fix.
  TypeHole /\ _ | Just (MV mv % []) <- Map.lookup (RuleMetaVar "type") sigma ->
    [Left (renCtx /\ 0), pure [HH.text (show (Base.getMetavarNumber renCtx mv))]]
  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [HH.text ("error: " <> show (Map.lookup (RuleMetaVar "type") sigma))]]
  If /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [ifElem], Left (renCtx /\ 0), pure ((newlineIndentElem renCtx.indentationLevel) <> [thenElem]), Left (renCtx' /\ 1),
        pure ((newlineIndentElem renCtx.indentationLevel) <> [elseElem]), Left (renCtx' /\ 2)]
  _ -> bug $
    "[Multary.arrangeDerivTermSubs] no match" <> "\n" <>
    "  - rule = " <> pretty rule <> "\n" <>
    "  - sort = " <> show sort

lambdaElem = Rendering.makePuncElem "lambda" "λ"
mapstoElem = Rendering.makePuncElem "mapsto" "↦"
refElem = Rendering.makePuncElem "ref" "#"
zeroVarElem = Rendering.makePuncElem "zeroVar" "Z"
sucVarElem = Rendering.makePuncElem "sucVar" "S"
-- Am I doing this right?
colonElem = Rendering.makePuncElem "colon" ":"
equalsElem = Rendering.makePuncElem "equals" "="
inElem = Rendering.makePuncElem "inLet" "in"
letElem = Rendering.makePuncElem "let" "let"
arrowElem = Rendering.makePuncElem "arrow" "→"
ifElem = Rendering.makePuncElem "if" "if"
thenElem = Rendering.makePuncElem "then" "then"
elseElem = Rendering.makePuncElem "else" "else"
errorLeftSide = Rendering.makePuncElem "errorLeft" "{{"
errorRightSide = Rendering.makePuncElem "errorRight" "}}"

typeElem = Rendering.makePuncElem "Type" "Type"

nameElem str = HH.span [classNames ["name"]] [HH.text str]
dataTypeElem str = HH.span [classNames ["datatype"]] [HH.text str]

tabElem = Rendering.makePuncElem "indent" "    "

newlineIndentElem :: forall t1 t2. Int -> Array (HH.HTML t1 t2)
--newlineIndentElem n = [Rendering.fillRightSpace, Rendering.newlineElem] <> Array.replicate n tabElem
newlineIndentElem n = [Rendering.newlineElem] <> Array.replicate n tabElem

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

wrapInSuc :: {-name-}Sort -> {-ty-}Sort -> DerivTerm -> DerivTerm
wrapInSuc name ty t =
         matchExpr (Grammar.derivTermSort t) (sor VarSort %$ [slot, slot, slot, slot]) \[gamma, x, tyX, locality] ->
         Grammar.makeLabel Suc [ "gamma" /\ gamma , "x" /\ x, "typeX" /\ tyX, "y" /\ name, "typeY" /\ ty, "locality" /\ locality]
         % [t]

-- gets a free variable in a context
getFreeVar :: {-The context-}Sort -> {-The name-} Sort -> {-The type-} Sort -> DerivTerm
getFreeVar ctx fvName fvType = matchExpr2 ctx
    (sor CtxConsSort %$ [slot, slot, slot]) (\[name, ty, ctx'] ->
        wrapInSuc name ty (getFreeVar ctx' fvName fvType)
    )
    (sor CtxNilSort %$ []) (\[] ->
        Grammar.makeLabel FreeVar ["name" /\ fvName, "type" /\ fvType] % []
    )

-- returns a list of all indices in the context
getIndices :: Sort -> List DerivTerm
getIndices ctx = matchExpr2 ctx
    (sor CtxConsSort %$ [slot, slot, slot]) (\[name, ty, ctx'] ->
        -- new var
        (Grammar.makeLabel Zero ["gamma" /\ ctx', "x" /\ name, "type" /\ ty] % [])
        -- wrap vars from ctx' in a Suc
        : map (wrapInSuc name ty) (getIndices ctx')
    )
    (sor CtxNilSort %$ []) (\[] ->
        Nil
    )

getVarEdits :: {-sort-}Sort -> List Edit
getVarEdits sort =
    matchExpr2 sort (sor TermSort %$ [slot, slot]) (\[ctx, ty] ->
            let wrapInRef index =
                 matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot , slot, slot, slot]) \[gamma, x, ty, locality] ->
                 Grammar.makeLabel LocalVar [ "gamma" /\ gamma , "x" /\ x, "type" /\ ty, "locality" /\ locality]
                 % [index]
            in
            let indices = getIndices ctx in
            let makeEdit index =
                    matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot, slot]) \[_ctx2, name, _varTy, _locality] ->
                    do
                        let var = wrapInRef index
                        [_gamma, varTy] <- matchExprImpl (Grammar.derivTermSort var) (sor TermSort %$ [slot, slot])
                        _newTy /\ sub <- unify varTy ty -- think about order
                        pure {
                            label: Grammar.matchStringLabel name
                            , action: defer \_ -> Edit.FillAction {
                                sub , dterm: var
                            }
                        }
            in
            List.mapMaybe makeEdit indices
        )
        -- If its not a TermSort, then there are no var edits
        slot \[_] -> Nil

splitChange ::
  SortChange ->
  {downChange :: SortChange, upChange :: SortChange, cursorSort :: Sort}
splitChange c =
    case ChangeAlgebra.eliminateReplaces c of
        c | Maybe.Just ([] /\ [ctx, ty]) <- matchChange c (TermSort %+- [cSlot, cSlot])
        ->
            let _ctx1 /\ ctx2 = ChangeAlgebra.endpoints ctx in
            let ty1 /\ _ty2 = ChangeAlgebra.endpoints ty in
            {upChange: csor TermSort % [ChangeAlgebra.inject ctx2, ty]
            , cursorSort: sor TermSort % [ctx2, ty1]
            , downChange: csor TermSort % [ctx, ChangeAlgebra.inject ty1]}
        c | Maybe.Just ([] /\ [ctx, ts]) <- matchChange c (ArgListSort %+- [cSlot, cSlot])
        ->
            let _ctx1 /\ ctx2 = ChangeAlgebra.endpoints ctx in
            let ts1 /\ _ts2 = ChangeAlgebra.endpoints ts in
            {upChange: csor ArgListSort % [ChangeAlgebra.inject ctx2, ts]
            , cursorSort: sor ArgListSort % [ctx2, ts1]
            , downChange: csor ArgListSort % [ctx, ChangeAlgebra.inject ts1]}
        c | Maybe.Just ([] /\ [ts]) <- matchChange c (TypeListSort %+- [cSlot, cSlot])
        ->
            let ts1 /\ _ts2 = ChangeAlgebra.endpoints ts in
            {upChange: csor TypeListSort % [ts]
            , cursorSort: sor TypeListSort % [ts1]
            , downChange: csor TypeListSort % [ChangeAlgebra.inject ts1]}
        c | Maybe.Just ([] /\ [gammaLess, gammaMore, ts]) <- matchChange c (VarListSort %+- [cSlot, cSlot, cSlot])
        ->
            let _gammaLess1 /\ gammaLess2 = ChangeAlgebra.endpoints gammaLess in
            let gammaMore1 /\ _gammaMore2 = ChangeAlgebra.endpoints gammaMore in
            let ts1 /\ _ts2 = ChangeAlgebra.endpoints ts in
            {upChange: csor VarListSort % [ChangeAlgebra.inject gammaLess2, gammaMore, ts]
            , cursorSort: sor VarListSort % [gammaLess2, gammaMore1, ts1]
            , downChange: csor VarListSort % [gammaLess, ChangeAlgebra.inject gammaMore1, ChangeAlgebra.inject ts1]}
        c | Maybe.Just ([] /\ [_ty]) <- matchChange c (TypeSort %+- [cSlot])
        -> {upChange: c, cursorSort: lEndpoint c, downChange: ChangeAlgebra.inject (lEndpoint c)}
        -- TODO: maybe could just generalize this to when c is the identity?
        ((Expr.CInj (MV _)) % []) ->
            {upChange: c, cursorSort: rEndpoint c, downChange: c}
        c -> bug ("splitChange - got c = " <> pretty c)

makeEditFromPath = DefaultEdits.makeEditFromPath forgetSorts splitChange

editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> Array.mapMaybe identity [
        DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Int)) "Int" cursorSort
        , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule String)) "String" cursorSort
        , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Bool)) "Bool" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule If) "If" cursorSort
    ]

editsAtCursor cursorSort = Array.mapMaybe identity
    [
    makeEditFromPath (newPathFromRule Lam 1) "lambda" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
    , makeEditFromPath (newPathFromRule TypeListConsRule 1) "cons" cursorSort
    , makeEditFromPath (newPathFromRule VarListCons 1) "cons" cursorSort
    , makeEditFromPath (newPathFromRule ArrowRule 1) "arrow" cursorSort
    , makeEditFromPath (newPathFromRule FunctionCall 0) "app" cursorSort
    , makeEditFromPath (newPathFromRule Newline 0 )"newline" cursorSort
    ]

--------------------------------------------------------------------------------
-- StepRules
--------------------------------------------------------------------------------

startCtx :: Sort
startCtx =
    sor CtxConsSort % [nameSort "ten", sor (DataType Int) % []
    , sor CtxNilSort %[]]

-- down{i}_(VarSort (+ y : Y, ctx) x X locality) -> Suc down{i}_(VarSort ctx x X locality)
insertSucRule :: StepRule
insertSucRule = Smallstep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-y-}slot, {-typeY-}slot] {-ctx-}cSlot [], {-x-}cSlot, {-ty-}cSlot,{-locality-}cSlot])
    {-i-}slot
    (\[y, typeY] [ctx, x, typeX, locality] [i] ->
        pure $
            dTERM Suc ["gamma" /\ rEndpoint ctx, "x" /\ rEndpoint x, "typeX" /\ rEndpoint typeX,
                "y" /\ y, "typeY" /\ typeY, "locality" /\ rEndpoint locality] [
                    Smallstep.wrapBoundary Smallstep.Down (csor VarSort % [ctx, x, typeX, locality]) $
                        i
                ])
        -- x is type of var, y is type of thing added to ctx

-- diff 0 (A, B, 0) = (+A, +B, 0)
-- i.e. adds to the LHS context in order to produce the RHS context i.e. what
-- change required of the first context to get second context

-- down{Z}_(VarSort (- A : ty, Gamma) A ty Local) ~> up{down{FreeVar}_(Var (diff 0 Gamma) A ty NonLocal))}_(VarSort id A ty (Replace Local NonLocal))
localBecomesNonlocal :: StepRule
localBecomesNonlocal = Smallstep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-a-}slot, {-ty-}slot] {-ctx-} cSlot [], {-a'-}cSlot, {-ty'-}cSlot, Local %+- []])
    (Zero %# [])
    (\[a, ty] [ctx, a', ty'] [] ->
        if not (ChangeAlgebra.inject a == a' && ChangeAlgebra.inject ty == ty') then Maybe.Nothing else
        pure $ Smallstep.wrapBoundary Smallstep.Up (csor VarSort % [ChangeAlgebra.inject (rEndpoint ctx), a', ty', Expr.Replace (sor Local % []) (sor NonLocal % []) % []])
            (Smallstep.termToSSTerm (getFreeVar (rEndpoint ctx) a ty)))
--            (Smallstep.wrapBoundary Smallstep.Down (csor VarSort % [ChangeAlgebra.diff (CtxNilSort %|-* []) (rEndpoint ctx), ChangeAlgebra.inject a, ChangeAlgebra.inject ty, csor NonLocal % []])
--                (dTERM FreeVar ["name" /\ a, "type" /\ ty] [])))

-- down{Suc i}_(VarSort (- y : TypeY, ctx) x typeX locality) -> down{i}_(VarSort ctx x typeX locality)
removeSucRule :: StepRule
removeSucRule = Smallstep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-y-}slot, {-typeY-}slot] {-ctx-} cSlot [], {-x-}cSlot, {-typeX-}cSlot, {-locality-}cSlot])
    (Suc %# [{-i-}slot])
    (\[_y, _typeY] [ctx, x, typeX, locality] [i] -> pure $
        Smallstep.wrapBoundary Smallstep.Down (csor VarSort % [ctx, x, typeX, locality]) $
        i)

--- down{i}_(Var (+ A : ty , ctx) A ty NonLocal) ~~> Z
-- NOTE: the unify and diff is to deal with the situation where a FreeVar is really the same type, but has different metavariables. Its a bit hacky maybe.
nonlocalBecomesLocal :: StepRule
nonlocalBecomesLocal = Smallstep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-a-}slot, {-ty-}slot] {-ctx-} cSlot [], {-a'-}cSlot, {-ty'-}cSlot, NonLocal %+- []])
    {-i-}slot
    (\[a, ty] [ctx, a', ty'] [_i] ->
        -- NOTE: the unification stuff can cause (Replace ?x t) to exist in the output. This is probably OK, but I'll need to think this through more carefully.
        if not (ChangeAlgebra.inject a == a' && Maybe.isJust (unify ty (rEndpoint ty'))) then Maybe.Nothing else
        pure $ Smallstep.wrapBoundary Smallstep.Up (csor VarSort % [(csor CtxConsSort % [a', ChangeAlgebra.inject ty, ChangeAlgebra.inject (rEndpoint ctx)])
            , a'
            , ChangeAlgebra.diff (rEndpoint ty') ty
            , Expr.replaceChange (sor NonLocal % []) (sor Local % [])])
            (dTERM Zero ["gamma" /\ rEndpoint ctx, "x" /\ a, "type" /\ ty] []))

-- PROBLEM WITH THIS: it deletes the cursor if its inside the type. This happens on let ?0 -> ?0 = lam x . x, insert Int into first hole.
typeBecomeRhsOfChange :: StepRule
typeBecomeRhsOfChange = Smallstep.makeDownRule
    (TypeSort %+- [{-c-}cSlot])
    {-t-}slot
    (\[] [c] [_t] -> pure (Smallstep.termToSSTerm (sortToType (rEndpoint c))))

---- Two kinds of boundaries that I am thinking of. The first is ErrorCall which replaces FunctionCall:
--replaceCallWithError :: StepRule
--replaceCallWithError = Smallstep.makeUpRule
--    (NeutralSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    (FunctionCall %# [{-t-}slot])
--    (\[t] -> t /\ (\[] [gamma, ty] inside -> pure $
--            dTERM ErrorCall ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
--                [inside]))

--replaceErrorWithCall :: StepRule
--replaceErrorWithCall (Expr.Expr (CInj (Grammar.DerivLabel ErrorCall sigma)) [t])
--    =
--    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
--    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
--    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
--    if insideType == outsideType then
--        pure $ dTERM FunctionCall ["gamma" /\ gamma, "type" /\ insideType] [t]
--    else
--        Nothing
--replaceErrorWithCall _ = Nothing

-- The second is Error which wraps a term

--wrapCallInErrorUp :: StepRule
--wrapCallInErrorUp ((CInj (Grammar.DerivLabel FunctionCall sigma)) % [
--        (Smallstep.Boundary Smallstep.Up ch) % [inside]
--    ])
--    | Just ([] /\ [gamma, ty]) <- Expr.matchChange ch (NeutralSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    =
--    if Maybe.isJust (Expr.matchChange ty (dPLUS Arrow [{-a-}slot] {-b-}cSlot [])) || ChangeAlgebra.isMerelyASubstitution ty
--        then Nothing
--        else pure $
--            let outsideType = Util.lookup' (Expr.RuleMetaVar "type") sigma in
--            Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, ChangeAlgebra.inject outsideType]) $
--            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
--                [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ rEndpoint ty] [inside ]]
--wrapCallInErrorUp _ = Nothing

--wrapCallInErrorDown :: StepRule
--wrapCallInErrorDown ((Smallstep.Boundary Smallstep.Down ch) % [
--        (CInj (Grammar.DerivLabel FunctionCall sigma)) % [inside]
--    ])
--    | Just ([] /\ [gamma, ty]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    =
--            if ChangeAlgebra.isMerelyASubstitution ty then Nothing else
--            let insideType = Util.lookup' (Expr.RuleMetaVar "type") sigma in
--            pure $
--            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint ty, "outsideType" /\ rEndpoint ty]
--                [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ lEndpoint ty] [
--                    Smallstep.wrapBoundary Smallstep.Down (csor NeutralSort % [gamma, ChangeAlgebra.inject insideType]) inside
--                ]]
--wrapCallInErrorDown _ = Nothing

--removeError :: StepRule
--removeError (Expr.Expr (CInj (Grammar.DerivLabel ErrorBoundary sigma)) [t])
--    =
--    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
--    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
--    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
--    if insideType == outsideType then
--        pure t
--    else
--        Nothing
--removeError _ = Nothing

--mergeErrors :: StepRule
--mergeErrors (Expr.Expr (CInj (Grammar.DerivLabel ErrorBoundary sigma1)) [
--        Expr.Expr (CInj (Grammar.DerivLabel ErrorBoundary sigma2)) [t]
--    ])
--    =
--    let insideInside = Util.lookup' (Expr.RuleMetaVar "insideType") sigma2 in
--    let outsideOutside = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma1 in
--    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma1 in
--    pure $ dTERM ErrorBoundary ["gamma" /\ gamma, "insideType" /\ insideInside, "outsideType" /\ outsideOutside] [t]
--mergeErrors _ = Nothing

wrapVarCons :: StepRule
wrapVarCons ((Boundary Smallstep.Down ch) % [
--        (SSInj (Grammar.DerivLabel VarListCons sigma)) % [inside]
        inside
    ])
    | Just ([t] /\ [gammaLess, gammaMore, ts]) <- matchChange ch (VarListSort %+- [{-gammaLess-}cSlot, {-gammaMore-}cSlot,
        dPLUS TypeListCons [{-t-}slot] {-ts-}cSlot []])
     =
        let varName = (Expr.MInj (Grammar.StringSortLabel "") % []) in
        pure $
            wrapBoundary Up (csor VarListSort % [inject (rEndpoint gammaLess)
                , plusChange (sor CtxConsSort) [varName, t] gammaLess []
                , csor TypeListCons % [inject t, inject (rEndpoint ts)]]) $
            dTERM VarListCons ["name" /\ varName, "gammaLess" /\ rEndpoint gammaLess
            , "gammaMore" /\ rEndpoint gammaMore, "t" /\ t, "ts" /\ rEndpoint ts]
        [
            Smallstep.termToSSTerm $ Util.fromJust $ (defaultDerivTerm (Grammar.NameSortLabel %* [varName]))
            , wrapBoundary Down (csor VarListSort % [gammaLess, gammaMore, ts]) inside
        ]
wrapVarCons _ = Nothing

unWrapVarCons :: StepRule
unWrapVarCons ((Boundary Smallstep.Down ch) % [
        (SSInj (Grammar.DerivLabel VarListCons sigma)) % [_name, inside]
    ])
    | Just ([t] /\ [gammaLess, _name, _t, gammaMore, ts])
        <- matchChange ch (VarListSort %+- [{-gammaLess-}cSlot, (CtxConsSort %+- [{-name-}cSlot, {-t-}cSlot, {-gammaMore-}cSlot]),
        dMINUS TypeListCons [{-t-}slot] {-ts-}cSlot []])
     =
        let varName = Util.lookup' (RuleMetaVar "name") sigma in
        pure $
            wrapBoundary Up (csor VarListSort % [inject (rEndpoint gammaLess)
                , minusChange (sor CtxConsSort) [varName, t] gammaMore []
                , inject (rEndpoint ts)]) $
            (wrapBoundary Down (csor VarListSort % [gammaLess, gammaMore, ts]) inside)
unWrapVarCons _ = Nothing

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

stepRules :: List StepRule
stepRules = do
  let chLang = Smallstep.langToChLang language
  List.fromFoldable (
    [
    localBecomesNonlocal
    , nonlocalBecomesLocal
    , insertSucRule
    , removeSucRule
--    , passThroughArrow
--    , wrapLambda
--    , unWrapLambda
--    , rehydrateApp
--    , wrapApp
--    , makeAppGreyed
--    , wrapCallInErrorUp
--    , wrapCallInErrorDown
--    , removeError
--    , mergeErrors
    , wrapVarCons
    , unWrapVarCons
    ]
    <>
    GreyedRules.createGreyedRules 1 ArrowRule Nothing splitChange forgetSorts languageChanges -- WORKS!
    <>
    GreyedRules.createGreyedRules 1 TypeListConsRule Nothing splitChange forgetSorts languageChanges -- WORKS!
    <>
    GreyedRules.createGreyedRules 1 VarListCons Nothing splitChange forgetSorts languageChanges -- Does not work
    <>
    [
    typeBecomeRhsOfChange
    , Smallstep.defaultDown chLang
    , Smallstep.defaultUp chLang
    ])

--createGreyedRules :: forall l r. Grammar.IsRuleLabel l r =>
--    Int -- what'th child will be effectively the value of this rule
--    -> r -- label for the regular construct
--    -> Maybe r -- what rule label to use for the greyed construct
--    -> Base.SplitChangeType l
--    -> LanguageChanges l r
--    -> Array (Smallstep.StepRule l r)

onDelete :: Sort -> SortChange
onDelete cursorSort
    | Maybe.Just [ty] <- Expr.matchExprImpl cursorSort (sor TypeSort %$ [slot])
    = csor TypeSort % [Expr.replaceChange ty (Expr.fromMetaVar (Expr.freshMetaVar "deleted"))]
onDelete (Expr.MInj Grammar.NameSortLabel % [s]) -- TODO: this should really be in generic
    = Expr.CInj (Expr.MInj Grammar.NameSortLabel) %
        [Expr.replaceChange s (Expr.MInj (Grammar.StringSortLabel "") % [])]
onDelete cursorSort = ChangeAlgebra.inject cursorSort

generalizeDerivation :: Sort -> SortChange
generalizeDerivation sort
    | Maybe.Just [ctx, ty] <- Expr.matchExprImpl sort (sor TermSort %$ [slot, slot])
    = csor TermSort % [ChangeAlgebra.diff ctx startCtx, ChangeAlgebra.inject ty]
generalizeDerivation other = ChangeAlgebra.inject other

specializeDerivation :: Sort -> Sort -> SortChange
specializeDerivation clipboard cursor
    | Maybe.Just [clipCtx, clipTy] <- Expr.matchExprImpl clipboard (sor TermSort %$ [slot, slot])
    , Maybe.Just [cursorCtx, _cursorTy] <- Expr.matchExprImpl cursor (sor TermSort %$ [slot, slot])
    = csor TermSort % [ChangeAlgebra.diff clipCtx cursorCtx, ChangeAlgebra.inject clipTy]
specializeDerivation clipboard _cursor = ChangeAlgebra.inject clipboard

forgetSorts :: DerivLabel -> Maybe DerivLabel
forgetSorts r@(Grammar.DerivLabel FreeVar sigma) = pure r
forgetSorts _ = Maybe.Nothing

clipboardSort :: Sort -> Sort
clipboardSort s
    | Maybe.Just [gamma, ty] <- Expr.matchExprImpl s (sor TermSort %$ [slot , slot])
    = sor TermSort % [startCtx, Expr.fromMetaVar (Expr.freshMetaVar "anyType")]
clipboardSort s
    | Maybe.Just [gammaLess, gammaMore, ts] <- Expr.matchExprImpl s (sor VarListSort %$ [slot, slot, slot])
    = sor VarListSort % [startCtx, Expr.fromMetaVar (Expr.freshMetaVar "anyCtx"), Expr.fromMetaVar (freshMetaVar "anyTypes")]
clipboardSort _other = Expr.fromMetaVar (Expr.freshMetaVar "anySort")

isValidCursorSort :: Sort -> Boolean
isValidCursorSort (Expr.MInj (Grammar.SInj VarSort) % _) = false
isValidCursorSort _ = true

isValidSelectionSorts :: {bottom :: Sort, top :: Sort} -> Boolean
isValidSelectionSorts {
        bottom: (Expr.MInj (Grammar.SInj TermSort) % _)
        , top: (Expr.MInj (Grammar.SInj TermSort) % _)
    } = true
isValidSelectionSorts {
        bottom: (Expr.MInj (Grammar.SInj TypeSort) % _)
        , top: (Expr.MInj (Grammar.SInj TypeSort) % _)
    } = true
isValidSelectionSorts {
        bottom: (Expr.MInj (Grammar.SInj TypeListSort) % _)
        , top: (Expr.MInj (Grammar.SInj TypeListSort) % _)
    } = true
isValidSelectionSorts {
        bottom: (Expr.MInj (Grammar.SInj VarListSort) % _)
        , top: (Expr.MInj (Grammar.SInj VarListSort) % _)
    } = true
isValidSelectionSorts _ = false

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "SULC dterm" $
      Grammar.defaultDerivTerm (TermSort %|-* [startCtx, Expr.fromMetaVar (Expr.freshMetaVar "tyhole")])
  , splitChange
  , editsAtCursor
  , editsAtHoleInterior
  , arrangeDerivTermSubs
  , stepRules
  , isValidCursorSort
  , isValidSelectionSorts
  , onDelete
  , generalizeDerivation
  , specializeDerivation
  , forgetSorts
  , clipboardSort
  }

