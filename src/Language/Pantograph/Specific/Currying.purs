module Language.Pantograph.Specific.Currying where

import Language.Pantograph.Generic.Grammar
import Data.Expr
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Smallstep (wrapBoundary, Direction(..))
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.List.Zip as ZipList
import Data.Enum (class Enum)
import Data.List.Rev as RevList
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, (%), (%*), slot, (%$))
import Data.Expr as Expr
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
import Language.Pantograph.Generic.ChangeAlgebra (rEndpoint, lEndpoint)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule, newTermFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar ((%|-), (%|-*), sor, csor, nameSort)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Console (logConsole)
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep ((%+-), dPLUS, dMINUS, (%#))
import Language.Pantograph.Generic.Smallstep (StepExprLabel(..), cSlot, dTERM)
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Generic.Unification (unify)
import Language.Pantograph.Generic.Unification as Unification
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust)
import Util as Util
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Language.Pantograph.Lib.GreyedRules as GreyedRules
import Data.Lazy as Lazy
import Data.Tuple (fst)

{-
This file is the start of the specific langauge that we will try to have working for the user study.
It should be STLC with no partial applications, some built in types, and thats about it.

We could maybe consider having a top level of the program and a main function.
We should think about how it will be run.
-}

--------------------------------------------------------------------------------
-- PreSortLabel
--------------------------------------------------------------------------------

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
  | TypeSort {-Type-}
  -- Contexts
  | CtxConsSort {-String-} {-Type-} {-Ctx-}
  | CtxNilSort
  -- Types
  | DataType DataType
  | Arrow {-Type-} {-Type-}
  | List {-Type-}
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
  prettyExprF'_unsafe (TypeSort /\ [t]) = "Type" <+> parens t
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, "∅"]) = x <> ":" <> ty
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, gamma]) = x <> ":" <> ty <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"
  prettyExprF'_unsafe (Local /\ []) = "Local"
  prettyExprF'_unsafe (NonLocal /\ []) = "NonLocal"
  prettyExprF'_unsafe (DataType ty /\ []) = show ty
  prettyExprF'_unsafe (Arrow  /\ [a, b]) = a <> " -> " <> b
  prettyExprF'_unsafe (List  /\ [t]) = "List" <+> t


  expectedKidsCount VarSort = 4
  expectedKidsCount TermSort = 2
  expectedKidsCount TypeSort = 1
  expectedKidsCount CtxConsSort = 3
  expectedKidsCount CtxNilSort = 0
  expectedKidsCount Local = 0
  expectedKidsCount NonLocal = 0
  expectedKidsCount (DataType _) = 0
  expectedKidsCount Arrow = 2
  expectedKidsCount List = 1

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
type SortSub = Grammar.SortSub PreSortLabel

-- Rendering
type Query = Base.Query
type Output = Base.Output PreSortLabel RuleLabel
type HoleyDerivZipper = Base.HoleyDerivZipper PreSortLabel RuleLabel

type Edit = Edit.Edit PreSortLabel RuleLabel
type Action = Edit.Action PreSortLabel RuleLabel

-- SmallStep
type StepRule = Smallstep.StepRule PreSortLabel RuleLabel

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

data Constant
    = ConstTrue
    | ConstFalse
    | ConstNot

derive instance Generic Constant _
instance Show Constant where show x = genericShow x
instance Eq Constant where eq x = genericEq x
instance Ord Constant where compare x y = genericCompare x y
instance Enum Constant where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded Constant where
  bottom = genericBottom
  top = genericTop
instance Pretty Constant where
  pretty = show

constantType :: Constant -> Sort {-type-}
constantType = case _ of
    ConstTrue -> DataType Bool %|-* []
    ConstFalse -> DataType Bool %|-* []
    ConstNot -> Arrow %|-* [DataType Bool %|-* [], DataType Bool %|-* []]

constantName :: Constant -> String
constantName = case _ of
    ConstTrue -> "true"
    ConstFalse -> "false"
    ConstNot -> "not"

data InfixOperator
    = OpPlus
    | OpMinus
    | OpTimes
    | OpDivide
    | OpPow
    | OpLess
    | OpGreater
    | OpLessEq
    | OpGreaterEq
    | OpAnd
    | OpOr

derive instance Generic InfixOperator _
instance Show InfixOperator where show x = genericShow x
instance Eq InfixOperator where eq x = genericEq x
instance Ord InfixOperator where compare x y = genericCompare x y
instance Enum InfixOperator where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded InfixOperator where
  bottom = genericBottom
  top = genericTop
instance Pretty InfixOperator where
  pretty = show

infixTypes :: InfixOperator -> {left :: Sort, right :: Sort, output :: Sort}
infixTypes op =
    let int = DataType Int %|-* [] in
    let bool = DataType Bool %|-* [] in
    case op of
    OpPlus -> {left : int, right : int, output : int}
    OpMinus -> {left : int, right : int, output : int}
    OpTimes -> {left : int, right : int, output : int}
    OpDivide -> {left : int, right : int, output : int}
    OpPow -> {left : int, right : int, output : int}
    OpLess -> {left : int, right : int, output : bool}
    OpGreater -> {left : int, right : int, output : bool}
    OpLessEq -> {left : int, right : int, output : bool}
    OpGreaterEq -> {left : int, right : int, output : bool}
    OpAnd -> {left : bool, right : bool, output : bool}
    OpOr -> {left : bool, right : bool, output : bool}

infixName :: InfixOperator -> String
infixName op =
    case op of
    OpPlus -> "+"
    OpMinus -> "-"
    OpTimes -> "*"
    OpDivide -> "/"
    OpPow -> "^"
    OpLess -> "<"
    OpGreater -> ">"
    OpLessEq -> "<="
    OpGreaterEq -> ">="
    OpAnd -> "and"
    OpOr -> "or"

-- | Naming convention: <title>_<output sort>
data RuleLabel
  = Zero
  | Suc
  | Lam
  | Let
  | App
  | Var
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
  | ListRule
  | Newline -- TODO: is this really an acceptable way for newlines to work? Its broken for applications, isn't it?
  | If -- TODO: should this be generalized in any way? Maybe for any type? For now I'll just do if.
  | ErrorBoundary
  | ConstantRule Constant
  | InfixRule InfixOperator
  | EqualsRule
  | NilRule
  | ConsRule
  | ListMatchRule

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
sortToType (Expr (MInj (Grammar.SInj (DataType dt))) []) =
    Grammar.makeLabel (DataTypeRule dt) [] % []
sortToType (Expr (MInj (Grammar.SInj Arrow)) [a, b]) =
    Grammar.makeLabel ArrowRule ["a" /\ a, "b" /\ b] % [sortToType a, sortToType b]
sortToType (Expr (MInj (Grammar.SInj List)) [ty]) =
    Grammar.makeLabel ListRule ["type" /\ ty] % [sortToType ty]
sortToType ty =
    Grammar.makeLabel TypeHole ["type" /\ ty] % []
--    Smallstep.termToSSTerm $ assertI $ just "sortToType: invalid sort"
--    $ Grammar.defaultDerivTerm (TypeSort %|-* [s])

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = pretty Zero
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = pretty Suc <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, ty, b]) = P.parens $ "λ" <+> x <+> ":" <+> ty <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (Let /\ [x, ty, a, b]) = P.parens $ "let" <+> x <+> ":" <+> ty <+> "=" <+> a <+> b
  prettyExprF'_unsafe_RuleLabel (ArrowRule /\ [a, b]) = P.parens $ a <+> "->" <+> b
  prettyExprF'_unsafe_RuleLabel (ListRule /\ [t]) = "List" <+> t
  prettyExprF'_unsafe_RuleLabel (DataTypeRule dataType /\ []) = pretty dataType
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Var /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ [ty]) = "(? : " <> ty <> ")"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel (If /\ [c, t, e]) = "if" <+> c <+> "then" <+> t <+> "else" <+> e
  prettyExprF'_unsafe_RuleLabel (ErrorBoundary /\ [t]) = "{{" <+> t <+> "}}"
  prettyExprF'_unsafe_RuleLabel (ConstantRule constant /\ []) = constantName constant
  prettyExprF'_unsafe_RuleLabel (InfixRule op /\ [left, right]) = P.parens $ left <+> infixName op <+> right
  prettyExprF'_unsafe_RuleLabel (NilRule /\ []) = "nil"
  prettyExprF'_unsafe_RuleLabel (ConsRule /\ []) = "cons"
  prettyExprF'_unsafe_RuleLabel (EqualsRule /\ [a, b]) = a <+> "==" <+> b
  prettyExprF'_unsafe_RuleLabel (ListMatchRule /\ [l, n, x, xs, c]) = "match" <+> l <+> "with Nil -> " <+> n <+> " cons" <+> x <+> " " <+> xs <+> " -> " <+> c
  prettyExprF'_unsafe_RuleLabel other = bug ("[prettyExprF'...] the input was: " <> show other)

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> Yes true
    TypeHole -> Yes false
    _ -> No

  defaultDerivTerm' (MInj (Grammar.SInj TermSort) % [gamma, ty])
    = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [sortToType ty])
  defaultDerivTerm' (MInj (Grammar.SInj VarSort) % [_gamma, _x, _ty, _locality]) = empty
  defaultDerivTerm' (MInj (Grammar.SInj TypeSort) % [ty]) =
    pure $ sortToType ty
  -- TODO: This case should probably be in Generic.
  defaultDerivTerm' (MInj (Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % [] -- TODO: this case should be in generic rather than here. In other words, the defaultDerivTerm in Grammar should do this case, and only hand the language specific cases to this function.
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

appRule :: Rule
appRule = Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %|-* [gamma, Arrow %|-* [a, b]]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( TermSort %|-* [gamma, b] )

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

  Lam -> Grammar.makeRule ["x", "a", "b", "gamma"] \[x, a, b, gamma] ->
    [ Grammar.NameSortLabel %* [x]
    , TypeSort %|-* [a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], b] ]
    /\ --------
    ( TermSort %|-* [gamma, Arrow %|-* [a, b]])

  App -> appRule

  FreeVar -> Grammar.makeRule ["name", "type"] \[name, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, ty, NonLocal %|-* []] )

  Var -> Grammar.makeRule ["gamma", "x", "type", "locality"] \[gamma, x, ty, locality] ->
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

  ListRule -> Grammar.makeRule ["type"] \[ty] ->
    [TypeSort %|-* [ty]]
    /\ -------
    ( TypeSort %|-* [List %|-* [ty]] )

  ArrowRule -> Grammar.makeRule ["a", "b"] \[a, b] ->
    [TypeSort %|-* [a], TypeSort %|-* [b]]
    /\ --------
    ( TypeSort %|-* [Arrow %|-* [a, b]] )

  If -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
      [ TermSort %|-* [gamma, DataType Bool %|-* []]
      , TermSort %|-* [gamma, ty]
      , TermSort %|-* [gamma, ty] ]
      /\ -------
      ( TermSort %|-* [gamma, ty] )

  ErrorBoundary -> Grammar.makeRule ["gamma", "insideType", "outsideType"] \[gamma, insideType, outsideType] ->
    [TermSort %|-* [gamma, insideType]]
    /\ -------
    ( TermSort %|-* [gamma, outsideType])

  ConstantRule c -> Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, constantType c])

  InfixRule op ->
    let {left, right, output} = infixTypes op in
    Grammar.makeRule ["gamma"] \[gamma] ->
    [ TermSort %|-* [gamma, left], TermSort %|-* [gamma, right] ]
    /\ -------
    ( TermSort %|-* [gamma, output])

  EqualsRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ TermSort %|-* [gamma, ty], TermSort %|-* [gamma, ty] ]
    /\ -------
    ( TermSort %|-* [gamma, DataType Bool %|-* []])

  NilRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, List %|-* [ty]])

  ConsRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [ty, Arrow %|-* [List %|-* [ty], List %|-* [ty]]]])

  ListMatchRule -> Grammar.makeRule ["gamma", "type", "outTy", "consElemArg", "consListArg"] \[gamma, ty, outTy, consElemArg, consListArg] ->
    [ TermSort %|-* [gamma, List %|-* [ty]]
    , TermSort %|-* [gamma, outTy]
    , Grammar.NameSortLabel %* [consElemArg]
    , Grammar.NameSortLabel %* [consListArg]
    , TermSort %|-* [CtxConsSort %|-* [consElemArg, ty, CtxConsSort %|-* [consListArg, List %|-* [ty], gamma]], outTy]]
    /\ -------
    ( TermSort %|-* [gamma, outTy])

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx, rule, sort, sigma, dzipper, mb_parent} =
  let dotOrNot unit = case mb_parent of -- Used in Var and App cases
          Just (Tooth (DerivLabel App _) (ZipList.Path {left, right})) | RevList.length left == 0
            -> [HH.div [classNames ["app-circle"]] [appCircle]]
          _ -> [] in
  case rule /\ sort of
  _ /\ (MInj (Grammar.SInj VarSort) %
    [ _gamma
    , MInj (Grammar.StringSortLabel str) % []
    , _ty
    , locality ]) ->
    -- TODO: use locality in rendering?
    let postfix = if locality == sor Local % [] then "" else "!" in
    [pure [nameElem (str <> postfix)]]
  -- term
  Var /\ _ ->
    [Left (renCtx /\ 0), pure (dotOrNot unit)]
  Lam /\ _ ->
    let maybeMapsTo = case dzipper of
            Just (Zipper _ (_ % [_, _, (DerivLabel Lam _) % _])) -> [Rendering.spaceElem]
            _ -> [mapstoElem] in
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [lambdaElem], Left (renCtx /\ 0), pure [colonElem]
        , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1), pure maybeMapsTo, Left (renCtx' /\ 2)]
  Let /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [letElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx' /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem]
        , pure (if renCtx.isInlined then [] else newlineIndentElem (renCtx.indentationLevel))
        , Left (renCtx /\ 3)]
  App /\ _ ->
    let leftParen /\ rightParen = case mb_parent of -- Used in Var and App cases
            Just (Tooth (DerivLabel App _) (ZipList.Path {left, right: _})) | RevList.length left == 0
              -> [] /\ []
            _ -> [Rendering.lparenElem] /\ [Rendering.rparenElem] in
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure leftParen, Left (renCtx' /\ 0), Left (renCtx' /\ 1), pure (dotOrNot unit), pure rightParen]
  -- types
  DataTypeRule dataType /\ _ ->
    [pure [dataTypeElem (pretty dataType)]]
  ArrowRule /\ _ ->
    let leftParen /\ rightParen = case mb_parent of -- Used in Var and App cases
            Just (Tooth (DerivLabel ArrowRule _) (ZipList.Path {left, right: _})) | RevList.length left == 0
              -> [Rendering.lparenElem] /\ [Rendering.rparenElem]
            _ -> [] /\ [] in
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure leftParen, Left (renCtx' /\ 0), pure [arrowElem], Left (renCtx' /\ 1), pure rightParen]
  -- format
  Newline /\ _ ->
    Array.concat
      [ if renCtx.isInlined then [] else [pure [HH.div [classNames ["newline-symbol"]] [HH.text " ↪"]], pure (newlineIndentElem renCtx.indentationLevel)]
      , [Left (renCtx /\ 0)] ]
  -- hole
  TermHole /\ (MInj (Grammar.SInj TermSort) % [_gamma, _ty])
    ->  [pure [Rendering.lbraceElem], Left (renCtx /\ 0), pure [colonElem]
        , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1)
        , pure [Rendering.rbraceElem]]
--  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [colonElem, typeElem]]
  -- only has inner hole? So messes up keyboard cursor movement. TODO: fix.
  TypeHole /\ _ | Just (MV mv % []) <- Map.lookup (RuleMetaVar "type") sigma ->
    [pure [HH.text "?", HH.text (show (Base.getMetavarNumber renCtx mv))]]
  TypeHole /\ _ -> [pure [HH.text ("error: " <> show (Map.lookup (RuleMetaVar "type") sigma))]]
  If /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [ifElem, Rendering.spaceElem], Left (renCtx' /\ 0), pure ((newlineIndentElem renCtx.indentationLevel) <> [thenElem, Rendering.spaceElem]), Left (renCtx' /\ 1),
        pure ((newlineIndentElem renCtx.indentationLevel) <> [elseElem, Rendering.spaceElem]), Left (renCtx' /\ 2)]
  ErrorBoundary /\ _ -> [pure [errorLeftSide], Left (renCtx /\ 0), pure [errorRightSide]]
  ConstantRule constant /\ _ -> [pure [HH.text (constantName constant)]]
  InfixRule op /\ _ ->
    [pure [Rendering.lparenElem], Left (renCtx /\ 0), pure [Rendering.spaceElem, HH.text (infixName op), Rendering.spaceElem], Left (renCtx /\ 1), pure [Rendering.rparenElem]]
  EqualsRule /\ _ ->
    [Left (renCtx /\ 0), pure [Rendering.spaceElem, HH.text "==", Rendering.spaceElem], Left (renCtx /\ 1)]
  ListRule /\ _ -> [pure [HH.text "List "], Left (renCtx /\ 0)]
  NilRule /\ _ -> [pure [HH.text "nil"]]
  ConsRule /\ _ -> [pure [HH.text "cons"]]
  ListMatchRule /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [HH.text "match "], Left (renCtx' /\ 0), pure [HH.text " with"], pure (newlineIndentElem renCtx.indentationLevel)
        , pure [HH.text "Nil -> "], Left (renCtx' /\ 1), pure (newlineIndentElem renCtx.indentationLevel)
        , pure [HH.text "Cons "], Left (renCtx' /\ 2), pure [HH.text " "], Left (renCtx' /\ 3)
        , pure [HH.text " -> "], Left (renCtx' /\ 4)]
  _ -> bug $
    "[STLC.Grammar.arrangeDerivTermSubs] no match" <> "\n" <>
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

appCircle = Rendering.makePuncElem "circle" " ⬤ "

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

wrapInRef :: DerivTerm -> DerivTerm /\ Sort {-the type of the var-}
wrapInRef index =
    matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot , slot, slot, slot]) \[gamma, x, ty, locality] ->
    Grammar.makeLabel Var [ "gamma" /\ gamma , "x" /\ x, "type" /\ ty, "locality" /\ locality]
    % [index] /\ ty

maximallyApplied :: {-cursorCtx-}Sort -> {-cursorType-}Sort -> {-type of dterm-}Sort -> DerivTerm -> Maybe (DerivTerm /\ SortSub)
maximallyApplied cursorCtx cursorTy tyOft toBeWrapped =
    let answerIfMe _ = do
         _ /\ sub <- unify cursorTy tyOft
         pure $ subDerivTerm sub toBeWrapped /\ sub
    in
    case matchExprImpl tyOft (sor Arrow %$ [slot, slot]) of
    Just [a, b] ->
        let wrapped = Grammar.makeLabel App ["gamma" /\ cursorCtx, "a" /\ a, "b" /\ b]
                % [toBeWrapped, Grammar.makeLabel TermHole ["gamma" /\ cursorCtx, "type" /\ a] % [sortToType a]] in
        case maximallyApplied cursorCtx cursorTy b wrapped of
            Just res -> Just res
            Nothing -> answerIfMe unit
    _ -> answerIfMe unit

getVarEdits :: {-sort-}Sort -> List Edit
getVarEdits sort =
    matchExpr2 sort (sor TermSort %$ [slot, slot]) (\[cursorCtx, cursorTy] ->
            let indices = getIndices cursorCtx in
            let makeEdit index =
                    matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot, slot]) \[_ctx2, name, _varTy, _locality] ->
                    do
                        let ref /\ ty = wrapInRef index
                        application /\ sub <- maximallyApplied cursorCtx cursorTy ty ref
                        pure {
                            label: Grammar.matchStringLabel name
                            , action: defer \_ -> Edit.FillAction {
                                sub , dterm: application
                            }
                        }
            in
            List.mapMaybe makeEdit indices
        )
        -- If its not a TermSort, then there are no var edits
        slot \[_] -> Nil

{-
TODO: the problem causing bugs is that this returns a thing with gamma just a metavariable when it should be a specific gamma
-}
getWrapInAppEdit :: String -> {-cursorSort-}Sort -> DerivTerm -> Maybe Edit
getWrapInAppEdit name cursorSort dterm =
    matchExpr2 cursorSort (sor TermSort %$ [slot, slot]) (\[cursorCtx, cursorTy] ->
            matchExpr (Grammar.derivTermSort dterm) (sor TermSort %$ [slot, slot]) \[gamma, ty] ->
            do
            _ /\ sub <- unify gamma cursorCtx
            let cursorCtx' = subMetaExprPartially sub cursorCtx
            let cursorTy' = subMetaExprPartially sub cursorTy
            let ty' = subMetaExprPartially sub ty
            let dterm' = subDerivTerm sub dterm
            application /\ sub2 <- maximallyApplied cursorCtx' cursorTy' ty' dterm'
            pure {
                label: name
                , action: defer \_ -> Edit.FillAction {
                    sub: Unification.composeSub sub sub2
                    , dterm: application
                }
            }
        )
        slot \[_] -> Nothing

getVarWraps :: {-cursorSort-}Sort -> List Edit
getVarWraps cursorSort
    | Just [cursorCtx, _cursorTy] <- matchExprImpl cursorSort (sor TermSort %$ [slot, slot]) =
    let meta = fromMetaVar (freshMetaVar "any") in
--    let edits = getVarEdits meta in
    let indices = getIndices cursorCtx in
    let edits = indices <#> \index -> do
            let ref /\ ty = wrapInRef index
            application /\ sub <- maximallyApplied cursorCtx meta ty ref
            pure $ matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot, slot]) \[_ctx2, name, _varTy, _locality] ->
                DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange (Grammar.matchStringLabel name) cursorSort application
    in List.concat (List.mapMaybe (\x -> x) edits)
getVarWraps _ = Nil
{-
If not Arrow type, Nil
If of arrow type, return path to that argument with rest of args applied, and getWraps of a single wrap

Or, maybe: I can write a more generic function which takes any term and gives all the wraps to anything which
unifies with the cursor sort?
-}

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
        c | Maybe.Just ([] /\ [_ty]) <- matchChange c (TypeSort %+- [cSlot])
        -> {upChange: c, cursorSort: lEndpoint c, downChange: ChangeAlgebra.inject (lEndpoint c)}
        -- TODO: maybe could just generalize this to when c is the identity?
        ((Expr.CInj (MV _)) % []) ->
            {upChange: c, cursorSort: rEndpoint c, downChange: c}
        c -> bug ("splitChange - got c = " <> pretty c)

makeEditFromPath = DefaultEdits.makeEditFromPath forgetSorts splitChange

editsAtHoleInterior :: Sort -> Array Edit
editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> Array.mapMaybe identity ([
        DefaultEdits.makeSubEditFromTerm (newTermFromRule If) "If" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule Lam) "lambda" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule Let) "let" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule App) "app" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule NilRule) "nil" cursorSort
        , getWrapInAppEdit "cons" cursorSort (newTermFromRule ConsRule)
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule ListMatchRule) "match" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule EqualsRule) "==" cursorSort
    ] <> ((Util.allPossible :: Array Constant) <#>
--        (\constant -> DefaultEdits.makeSubEditFromTerm (newTermFromRule (ConstantRule constant)) (constantName constant) cursorSort))
        (\constant -> getWrapInAppEdit (constantName constant) cursorSort (newTermFromRule (ConstantRule constant))))
       <> ((Util.allPossible :: Array InfixOperator) <#>
        (\op -> DefaultEdits.makeSubEditFromTerm (newTermFromRule (InfixRule op)) (infixName op) cursorSort))
    )

editsAtCursor cursorSort = Array.mapMaybe identity (
    [
    DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Int)) "Int" cursorSort
    , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule String)) "String" cursorSort
    , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Bool)) "Bool" cursorSort
    , DefaultEdits.makeChangeEditFromTerm (newTermFromRule ListRule) "List" cursorSort
    , makeEditFromPath (newPathFromRule Lam 2) "lambda" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
    , makeEditFromPath (newPathFromRule ArrowRule 1) "arrow" cursorSort
    , makeEditFromPath (newPathFromRule App 0) "app" cursorSort
--    , makeEditFromPath (newPathFromRule ErrorCall 0) "error" cursorSort

--    , makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort
--    , makeEditFromPath (newPathFromRule ArrowRule 1) "->" cursorSort
    ]) <> (Array.concat ((Util.allPossible :: Array InfixOperator) <#>
        (\op -> Array.fromFoldable $ DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange
            (infixName op) cursorSort (newTermFromRule (InfixRule op)))))
    <> Array.fromFoldable (getVarWraps cursorSort)

--    [fromJust $ makeEditFromPath (newPathFromRule Lam 1)] -- [makeEditFromPath (newPathFromRule Lam 1)] -- Edit.defaultEditsAtCursor
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
        pure $ Smallstep.wrapBoundary Smallstep.Up (csor VarSort % [ChangeAlgebra.inject (rEndpoint ctx), a', ty', Replace (sor Local % []) (sor NonLocal % []) % []])
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
            , replaceChange (sor NonLocal % []) (sor Local % [])])
            (dTERM Zero ["gamma" /\ rEndpoint ctx, "x" /\ a, "type" /\ ty] []))

passThroughArrow :: StepRule
passThroughArrow dterm =
    case dterm of
    ((Smallstep.Boundary Smallstep.Down ch) % [
        (SSInj (Grammar.DerivLabel ArrowRule _sigma)) % [da, db]
    ])
    | Just ([] /\ [ca, cb]) <- matchChange ch (TypeSort %+- [Arrow %+- [{-ca-}cSlot, {-cb-}cSlot]])
    -> pure (dTERM ArrowRule ["a" /\ rEndpoint ca, "b" /\ rEndpoint cb]
            [Smallstep.wrapBoundary Smallstep.Down (csor TypeSort % [ca]) da
            , Smallstep.wrapBoundary Smallstep.Down (csor TypeSort % [cb]) db])
    _ -> Nothing

-- PROBLEM WITH THIS: it deletes the cursor if its inside the type. This happens on let ?0 -> ?0 = lam x . x, insert Int into first hole.
typeBecomeRhsOfChange :: StepRule
typeBecomeRhsOfChange = Smallstep.makeDownRule
    (TypeSort %+- [{-c-}cSlot])
    {-t-}slot
    (\[] [c] [_t] -> pure (Smallstep.termToSSTerm (sortToType (rEndpoint c))))

-- To replace, I need:
-- wrapArrow, unwrapArrow
-- if c goes from t1 to t2, then


-- down{t}_(Term G (+ A -> B)) ~~> Lam ~ : A. down{t}_(Term (+ ~:A, G) B)
wrapLambda :: StepRule
wrapLambda = Smallstep.makeDownRule
    (TermSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    {-t-}slot
    (\[a] [gamma, b] [t] ->
        let varName = (MInj (Grammar.StringSortLabel "") % []) in
        pure $
            dTERM Lam ["x" /\ varName, "a" /\ a, "b" /\ rEndpoint b, "gamma" /\ rEndpoint gamma] [
                    Smallstep.termToSSTerm $ Util.fromJust' "wrapLambda" $ (Grammar.defaultDerivTerm (Grammar.NameSortLabel %* [varName]))
                    , Smallstep.termToSSTerm (sortToType a)
                    , Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [plusChange (sor CtxConsSort) [varName, a] gamma [] , b]) $
                        t
                ])

-- down{Lam x : A. t}_(Term G (- A -> B)) ~~> down{t}_(Term (-x : A, G) B)
unWrapLambda :: StepRule
unWrapLambda (Expr (Smallstep.Boundary Smallstep.Down ch) [
        Expr (SSInj (Grammar.DerivLabel Lam sigma)) [_name, _ty, body]
    ])
    = do
        let varName = Util.lookup' (RuleMetaVar "x") sigma
--        -- TODO: HERE IS WHERE I LEFT OFF: this match works, but match below fails. TODO: Get the second cslot, and debug matchChange on that!
--        -- TODO: Also, compare to what these output (including real case) when one Int is a metavar?!?
--        case (matchChange ch (TermSort %+- [{-gamma-}cSlot, cSlot])) of
--            Just ([] /\ [_, lad])  -> do
--                traceM ("here we go: " <> pretty lad)
--                traceM (pretty (matchChange lad (dMINUS Arrow [slot] cSlot [])))
--            _ -> pure unit
        restOfCh <- case unit of
                    _ | Just ([a] /\ [gamma, b]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
                        -> pure (csor TermSort % [minusChange (sor CtxConsSort) [varName, a] gamma [] , b])
--                    -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
--                    _ | Just ([a, b, x] /\ [gamma]) <- matchChange ch
--                            (TermSort %+- [{-gamma-}cSlot, replaceChange ((InjectMatchLabel (sor Arrow)) % [{-a-}slot, {-b-}slot]) {-x-}slot])
--                      , (MV _ % _) <- x
--                        -> pure (csor TermSort % [minusChange (sor CtxConsSort) [varName, a] gamma [], replaceChange b x])
                    _ -> Nothing
        pure $
            Smallstep.wrapBoundary Smallstep.Down restOfCh $
                body

unWrapLambda _ = Nothing

-- TODO: its not clear to me that I've correctly thought through how this will work when
-- there are boundaries inside the input term
isNeutralFormWithoutCursor :: SSTerm -> Boolean
isNeutralFormWithoutCursor t = case t of
    (SSInj (DerivLabel Var _)) % _ -> true
    (SSInj (DerivLabel App _)) % [left, _] -> isNeutralFormWithoutCursor left
    ((Boundary _ _) % [inside]) -> isNeutralFormWithoutCursor inside
    _ -> false

-- up{t}_(Term G (+ A -> B)) ~~> up{App t ?}_(Term G B)
wrapApp :: StepRule
wrapApp = Smallstep.makeUpRule
    (TermSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    {-t-}slot
    (\[t] -> t /\ (\[a] [gamma, b] inside ->
        if not (isNeutralFormWithoutCursor inside) then Nothing else
        pure $
        Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, b]) $
            dTERM App ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b]
                [inside, Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (sor TermSort % [rEndpoint gamma, a]))]))

-- App up{t1}_(Term G (- A -> B)) t2 ~~> up{t1}_(Term G B)
unWrapApp :: StepRule
unWrapApp = Smallstep.makeUpRule
    (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    (App %# [{-t1-}slot, {-t2-}slot])
    (\[t1, _t2] -> t1 /\ (\[_a] [gamma, b] inside -> pure $
        Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, b]) $
            inside))

-- TODO: I might want to make this do unification of inside and outside, and if is it a
-- "mereSubsituttion" change or something, then propagate it and unify the holes in the program.
removeError :: StepRule
removeError (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma)) [t])
    =
    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
    if insideType == outsideType then
        pure t
    else
        Nothing
removeError _ = Nothing

mergeErrors :: StepRule
mergeErrors (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma1)) [
        Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma2)) [t]
    ])
    =
    let insideInside = Util.lookup' (Expr.RuleMetaVar "insideType") sigma2 in
    let outsideOutside = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma1 in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma1 in
    pure $ dTERM ErrorBoundary ["gamma" /\ gamma, "insideType" /\ insideInside, "outsideType" /\ outsideOutside] [t]
mergeErrors _ = Nothing

introErrorDownVar :: StepRule
introErrorDownVar ((Smallstep.Boundary Down ch) % [
        inside @((SSInj (DerivLabel Var _)) % _)
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , not (isMerelyASubstitution c)
    =
        pure $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint c, "outsideType" /\ rEndpoint c]
            [wrapBoundary Down (csor TermSort % [gamma, inject (lEndpoint c)]) inside]
introErrorDownVar _ = Nothing

introErrorDown :: StepRule
introErrorDown ((Smallstep.Boundary Down ch) % [
        inside
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
        pure $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint c, "outsideType" /\ rEndpoint c]
            [wrapBoundary Down (csor TermSort % [gamma, inject (lEndpoint c)]) inside]
introErrorDown _ = Nothing

-- IDEA: a problem with this rule is that it might trigger before rules that propagate up through lets and stuff.
-- I might fix that by altering it so that it actually takes a form with the boundary as a child and
-- works on that.
introErrorUp :: StepRule
introErrorUp ((Smallstep.Boundary Up ch) % [
        inside
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
        pure $
            wrapBoundary Up (csor TermSort % [gamma, inject (lEndpoint c)]) $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint c, "outsideType" /\ lEndpoint c]
                [inside]
introErrorUp _ = Nothing

mergeErrorsUp :: StepRule
mergeErrorsUp (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma)) [
        Expr.Expr (Boundary Up ch) [t]
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
    let insideInside = rEndpoint c in
    let outsideOutside = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
    pure $
        wrapBoundary Up (csor TermSort % [gamma, inject outsideOutside]) $
        dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma,
            "insideType" /\ insideInside, "outsideType" /\ outsideOutside] [t]
mergeErrorsUp _ = Nothing

mergeErrorsDown :: StepRule
mergeErrorsDown (Expr.Expr (Boundary Down ch) [
        Expr.Expr (SSInj (DerivLabel ErrorBoundary sigma)) [t]
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
    let insideInside = Util.lookup' (RuleMetaVar "insideType") sigma in
    let outsideOutside = rEndpoint c in
    pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma,
            "insideType" /\ insideInside, "outsideType" /\ outsideOutside]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideInside]) t]
mergeErrorsDown _ = Nothing

fallbackDownError :: StepRule
fallbackDownError (Expr.Expr (Boundary Down ch) [ inside@(SSInj _ % _) ]) -- Only do the down rule if there is a regular derivterm below, if we did this on markers and Boundaries it might create a problem
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    = pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ rEndpoint c]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideTy]) inside]
fallbackDownError _ = Nothing

fallbackUpError :: StepRule
fallbackUpError sterm =
    case sterm of
    (SSInj label@(DerivLabel _ _) % kids)
--    | Just [_gamma, outsideTy] <- matchExprImpl (Smallstep.ssTermSort outside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    -> do
        (gamma /\ c /\ inside /\ insideTy) /\ i <- flip Util.findWithIndex kids case _ of
            (Boundary Up ch % [inside])
                | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
                , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
--                , not (isId c)
                -> pure $ gamma /\ c /\ inside /\ insideTy
            _ -> Nothing
--        traceM ("returning from fallbackUpError. i is: " <> pretty i <> " inside is: " <> pretty inside <> "and outside is: " <> pretty outside <> " and label is: " <> pretty label)
--        let insideTy = Smallstep.ssTermSort inside
        let outsideTy = lEndpoint c
        pure $
            (SSInj label % Util.fromJust (Array.updateAt i
                (wrapBoundary Up (csor TermSort % [gamma, inject outsideTy]) $
                    dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ outsideTy] [inside]) kids))
    _ -> Nothing

-- Don't allow a down boundary to propagate into a neutral form
introErrorNeutral :: StepRule
introErrorNeutral (Expr.Expr (Boundary Down ch) [ inside@(SSInj _ % _) ]) -- Only do the down rule if there is a regular derivterm below, if we did this on markers and Boundaries it might create a problem
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    , isNeutralFormWithoutCursor inside
    = pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ rEndpoint c]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideTy]) inside]
introErrorNeutral _ = Nothing

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
    , passThroughArrow
    , typeBecomeRhsOfChange
--    , introErrorNeutral
    , introErrorDownVar
    , wrapLambda
    , unWrapLambda
    , wrapApp
    , unWrapApp
    , mergeErrorsDown
    , mergeErrorsUp
    , removeError
    ]
--    <>
--    (GreyedRules.createGreyedRules 2 Lam Nothing splitChange forgetSorts languageChanges)
    <> [
    Smallstep.defaultDown chLang
    , Smallstep.defaultUp chLang
    ]
    <> [
--    wrapApp
--    , unWrapApp
--    introErrorDown
--    , introErrorUp
      fallbackDownError
    , fallbackUpError
    ]
    )

onDelete :: Sort -> SortChange
onDelete cursorSort
    | Maybe.Just [ty] <- matchExprImpl cursorSort (sor TypeSort %$ [slot])
    = csor TypeSort % [replaceChange ty (fromMetaVar (freshMetaVar "deleted"))]
onDelete (MInj Grammar.NameSortLabel % [s])
    = Expr.CInj (MInj Grammar.NameSortLabel) %
        [replaceChange s (MInj (Grammar.StringSortLabel "") % [])]
onDelete cursorSort = ChangeAlgebra.inject cursorSort

generalizeDerivation :: Sort -> SortChange
generalizeDerivation sort
    | Maybe.Just [ctx, ty] <- matchExprImpl sort (sor TermSort %$ [slot, slot])
    = csor TermSort % [ChangeAlgebra.diff ctx startCtx, ChangeAlgebra.inject ty]
generalizeDerivation other = ChangeAlgebra.inject other

specializeDerivation :: Sort -> Sort -> SortChange
specializeDerivation clipboard cursor
    | Maybe.Just [clipCtx, clipTy] <- matchExprImpl clipboard (sor TermSort %$ [slot, slot])
    , Maybe.Just [cursorCtx, _cursorTy] <- matchExprImpl cursor (sor TermSort %$ [slot, slot])
    = csor TermSort % [ChangeAlgebra.diff clipCtx cursorCtx, ChangeAlgebra.inject clipTy]
specializeDerivation clipboard _cursor = ChangeAlgebra.inject clipboard

forgetSorts :: DerivLabel -> Maybe DerivLabel
forgetSorts r@(Grammar.DerivLabel FreeVar sigma) = pure r
forgetSorts _ = Maybe.Nothing

clipboardSort :: Sort -> Sort
clipboardSort s
    | Maybe.Just [gamma, ty] <- matchExprImpl s (sor TermSort %$ [slot , slot])
    = sor TermSort % [startCtx, fromMetaVar (freshMetaVar "anyType")]
clipboardSort _other = fromMetaVar (freshMetaVar "anySort")

isValidCursorSort :: Sort -> Boolean
isValidCursorSort (MInj (Grammar.SInj VarSort) % _) = false
isValidCursorSort _ = true

isValidSelectionSorts :: {bottom :: Sort, top :: Sort} -> Boolean
isValidSelectionSorts {
        bottom: (MInj (Grammar.SInj TermSort) % _)
        , top: (MInj (Grammar.SInj TermSort) % _)
    } = true
isValidSelectionSorts {
        bottom: (MInj (Grammar.SInj TypeSort) % _)
        , top: (MInj (Grammar.SInj TypeSort) % _)
    } = true
isValidSelectionSorts _ = false

keyAction :: String -> Sort -> Maybe Action
keyAction _ (MInj (Grammar.NameSortLabel) % [_]) = Nothing -- Don't have newlines in strings!
keyAction "Enter" cursorSort =
        DefaultEdits.makeActionFromPath true forgetSorts splitChange (fst (newPathFromRule Newline 0))"newline" cursorSort
keyAction _ _ = Nothing

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "SULC dterm" $
      Grammar.defaultDerivTerm (TermSort %|-* [startCtx, fromMetaVar (freshMetaVar "tyhole")])
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
  , keyAction
  }

