module Language.Pantograph.Specific.Currying where

import Data.Expr
import Data.Tuple.Nested
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Grammar
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, (%), (%*), slot, (%$))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lazy (defer)
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Tuple (fst, uncurry)
import Data.Variant (Variant)
import Debug (traceM, trace)
import Debug as Debug
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
import Language.Pantograph.Generic.Rendering.Rendering (renderDerivTerm)
import Language.Pantograph.Generic.Smallstep ((%+-), dPLUS, dMINUS, (%#))
import Language.Pantograph.Generic.Smallstep (StepExprLabel(..), cSlot, dTERM)
import Language.Pantograph.Generic.Smallstep (wrapBoundary, Direction(..))
import Language.Pantograph.Generic.Smallstep as Smallstep
import Language.Pantograph.Generic.Unification (unify)
import Language.Pantograph.Generic.Unification as Unification
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Language.Pantograph.Lib.GreyedRules as GreyedRules
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust, index', lookup')
import Util as Util


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
instance EncodeJson DataType where encodeJson a = genericEncodeJson a
instance DecodeJson DataType where decodeJson a = genericDecodeJson a

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
instance EncodeJson PreSortLabel where encodeJson a = genericEncodeJson a
instance DecodeJson PreSortLabel where decodeJson a = genericDecodeJson a

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
  prettyExprF'_unsafe (Arrow  /\ [a, b]) = parens $ a <> " -> " <> b
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
instance EncodeJson Constant where encodeJson a = genericEncodeJson a
instance DecodeJson Constant where decodeJson a = genericDecodeJson a

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
    | OpMod
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
instance EncodeJson InfixOperator where encodeJson a = genericEncodeJson a
instance DecodeJson InfixOperator where decodeJson a = genericDecodeJson a

infixTypes :: InfixOperator -> {left :: Sort, right :: Sort, output :: Sort}
infixTypes op =
    let int = DataType Int %|-* [] in
    let bool = DataType Bool %|-* [] in
    case op of
    OpPlus -> {left : int, right : int, output : int}
    OpMinus -> {left : int, right : int, output : int}
    OpTimes -> {left : int, right : int, output : int}
    OpDivide -> {left : int, right : int, output : int}
    OpMod -> {left : int, right : int, output : int}
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
    OpMod -> "%"
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
  | GreyApp
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
  | LengthRule
  | AppendRule
  | HeadRule
  | TailRule
  | IndexRule
  | ListMatchRule
  | IntegerLiteral
  | Comment

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
instance EncodeJson RuleLabel where encodeJson a = genericEncodeJson a
instance DecodeJson RuleLabel where decodeJson a = genericDecodeJson a

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
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, ty, b]) = P.parens $ "fun" <+> x <+> ":" <+> ty <+> "=>" <+> b
  prettyExprF'_unsafe_RuleLabel (Let /\ [x, ty, a, b]) = P.parens $ "let" <+> x <+> ":" <+> ty <+> "=" <+> a <+> b
  prettyExprF'_unsafe_RuleLabel (ArrowRule /\ [a, b]) = P.parens $ a <+> "->" <+> b
  prettyExprF'_unsafe_RuleLabel (ListRule /\ [t]) = "List" <+> t
  prettyExprF'_unsafe_RuleLabel (DataTypeRule dataType /\ []) = pretty dataType
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (GreyApp /\ [f, a]) = "<" <> f <+> a <> ">"
  prettyExprF'_unsafe_RuleLabel (Var /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ [ty]) = "(? : " <> ty <> ")"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel (If /\ [c, t, e]) = "if" <+> c <+> "then" <+> t <+> "else" <+> e
  prettyExprF'_unsafe_RuleLabel (ErrorBoundary /\ [t]) = "{{" <+> t <+> "}}"
  prettyExprF'_unsafe_RuleLabel (ConstantRule constant /\ []) = constantName constant
  prettyExprF'_unsafe_RuleLabel (InfixRule op /\ [left, right]) = P.parens $ left <+> infixName op <+> right
  prettyExprF'_unsafe_RuleLabel (LengthRule /\ []) = "length"
  prettyExprF'_unsafe_RuleLabel (AppendRule /\ []) = "append"
  prettyExprF'_unsafe_RuleLabel (ConsRule /\ []) = "cons"
  prettyExprF'_unsafe_RuleLabel (NilRule /\ []) = "nil"
  prettyExprF'_unsafe_RuleLabel (HeadRule /\ []) = "head"
  prettyExprF'_unsafe_RuleLabel (TailRule /\ []) = "tail"
  prettyExprF'_unsafe_RuleLabel (LengthRule /\ []) = "tail"
  prettyExprF'_unsafe_RuleLabel (IndexRule /\ []) = "tail"
  prettyExprF'_unsafe_RuleLabel (EqualsRule /\ [a, b]) = a <+> "==" <+> b
  prettyExprF'_unsafe_RuleLabel (ListMatchRule /\ [l, n, x, xs, c]) = "match" <+> l <+> "with Nil -> " <+> n <+> " cons" <+> x <+> " " <+> xs <+> " -> " <+> c
  prettyExprF'_unsafe_RuleLabel (IntegerLiteral /\ [lit]) = lit
  prettyExprF'_unsafe_RuleLabel (Comment /\ [c, a]) = "/* " <> c <> " */ " <> a
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
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

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
    [ TypeOfLabel SortString %* [x]
    , TypeSort %|-* [a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], b] ]
    /\ --------
    ( TermSort %|-* [gamma, Arrow %|-* [a, b]])

  App -> Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %|-* [gamma, Arrow %|-* [a, b]]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( TermSort %|-* [gamma, b] )

  GreyApp -> Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %|-* [gamma, b]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( TermSort %|-* [gamma, b] )

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

  Newline -> Grammar.makeRule ["gamma", "type"] \[g, t] ->
    [ TermSort %|-* [g, t] ]
    /\ --------
    ( TermSort %|-* [g, t] )

  Let -> Grammar.makeRule ["x", "a", "b", "gamma"] \[x, a, b, gamma] ->
    [ TypeOfLabel SortString %* [x]
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

  HeadRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [List %|-* [ty], ty]])

  TailRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [List %|-* [ty], List %|-* [ty]]])

  IndexRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [List %|-* [ty], Arrow %|-* [DataType Int %|-* [], ty]]])

  LengthRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [List %|-* [ty], DataType Int %|-* []]])

  AppendRule -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %|-* [gamma, Arrow %|-* [List %|-* [ty], Arrow %|-* [List %|-* [ty], List %|-* [ty]]]])

  ListMatchRule -> Grammar.makeRule ["gamma", "type", "outTy", "consElemArg", "consListArg"] \[gamma, ty, outTy, consElemArg, consListArg] ->
    [ TermSort %|-* [gamma, List %|-* [ty]]
    , TermSort %|-* [gamma, outTy]
    , TypeOfLabel SortString %* [consElemArg]
    , TypeOfLabel SortString %* [consListArg]
    , TermSort %|-* [CtxConsSort %|-* [consElemArg, ty, CtxConsSort %|-* [consListArg, List %|-* [ty], gamma]], outTy]]
    /\ -------
    ( TermSort %|-* [gamma, outTy])

  IntegerLiteral -> Grammar.makeRule ["gamma", "n"] \[gamma, n] ->
    [ TypeOfLabel SortInt %* [n] ]
    /\ -------
    ( TermSort %|-* [gamma, DataType Int %|-* []] )

  Comment -> Grammar.makeRule ["c", "gamma", "type"] \[c, g, t] ->
    [ TypeOfLabel SortString %* [c]
    , TermSort %|-* [g, t] ]
    /\ --------
    ( TermSort %|-* [g, t] )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

isAppOrGreyApp :: RuleLabel -> Boolean
isAppOrGreyApp App = true
isAppOrGreyApp GreyApp = true
isAppOrGreyApp _ = false

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx: preRenCtx, rule, sort, sigma, dzipper, mb_parent, renderTerm} =
  let renCtx = preRenCtx{cssClasses = Set.delete "error" preRenCtx.cssClasses } :: Base.RenderingContext in -- remove some things from css classes
  let parentRule = case mb_parent of
        Just (Tooth (DerivLabel rule _) _) -> Just rule
        Nothing -> Nothing in
  let dotOrNot = case mb_parent of -- Used in Var and App cases
          Just (Tooth (DerivLabel label _) (ZipList.Path {left, right})) | RevList.length left == 0
            -> isAppOrGreyApp label
          _ -> false in
  let html = (
          case rule /\ sort of
          _ /\ (MInj (Grammar.SInj VarSort) %
            [ _gamma
            , MInj (Grammar.DataLabel (DataString str)) % []
            , _ty
            , locality ]) ->
            let classes = if locality == sor Local % [] then [] else ["error", "grey"] in
            [pure [HH.div [classNames (["inline"] <> classes)] [nameElem str]]]
--            [pure [nameElem (str <> postfix)]]
          -- term
          Var /\ _ ->
            [Left (renCtx /\ 0)]
          Lam /\ _ ->
            let maybeMapsTo = case dzipper of
                    Just (Zipper _ (_ % [_, _, (DerivLabel Lam _) % _])) -> [Rendering.spaceElem]
                    _ -> [mapstoElem] in
            let renCtx' = if parentRule == Just Lam then renCtx else Base.incremementIndentationLevel renCtx in
            [pure [lambdaElem], Left (renCtx /\ 0), pure [colonElem]
                , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1), pure maybeMapsTo, Left (renCtx' /\ 2)]
          Let /\ _ ->
            let renCtx' = Base.incremementIndentationLevel renCtx in
            [pure [letElem], Left (renCtx' /\ 0), pure [colonElem], Left (renCtx' /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem]
                , pure (if renCtx.isInlined then [] else newlineIndentElem (renCtx.indentationLevel))
                , Left (renCtx /\ 3)]
          label /\ _ | isAppOrGreyApp label -> -- App and GreyApp
            let isGrey = case label of
                    GreyApp -> true
                    _ -> false in
            let leftParen /\ rightParen = case mb_parent of -- Used in Var and App cases
                    Just (Tooth (DerivLabel label _) (ZipList.Path {left, right: _})) | RevList.length left == 0
                      , isAppOrGreyApp label -> [] /\ []
                    _ -> [Rendering.lparenElem] /\ [Rendering.rparenElem] in
            let renCtx' = Base.incremementIndentationLevel renCtx in
            let renCtx'' = if isGrey then renCtx'{cssClasses = Set.insert "grey" (Set.singleton "error")} else renCtx' in
            [pure leftParen, Left (renCtx' /\ 0) , Left (renCtx'' /\ 1), pure rightParen]
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
          -- comment
          -- Comment /\ _ | Just (MInj (DataLabel (DataString c)) % _) <- Map.lookup (RuleMetaVar "c") sigma ->
          Comment /\ _ ->
            Array.concat
              [ [ pure [HH.span [classNames ["Comment_anchor"]] []]
                , Left (renCtx /\ 0) ]
              , if renCtx.isInlined then [] else [pure (newlineIndentElem renCtx.indentationLevel)]
              , [Left (renCtx /\ 1)] ]
          -- hole
          TermHole /\ (MInj (Grammar.SInj TermSort) % [_gamma, _ty])
            ->  [pure [Rendering.lbraceElem], Left (renCtx /\ 0), pure [colonElem]
                , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1)
                , pure [Rendering.rbraceElem]]
        --  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [colonElem, typeElem]]
          -- only has inner hole? So messes up keyboard cursor movement. TODO: fix.
          TypeHole /\ _ | Just (MV mv % []) <- Map.lookup (RuleMetaVar "type") sigma ->
            [pure [HH.span [HP.classes [HH.ClassName "TypeHoleMV"]] [HH.text "?", HH.text (show (Base.getMetavarNumber renCtx mv))]]]
          TypeHole /\ _ -> [pure [HH.text ("error: " <> show (Map.lookup (RuleMetaVar "type") sigma))]]
          If /\ _ ->
            let renCtx' = Base.incremementIndentationLevel renCtx in
            [pure [ifElem, Rendering.spaceElem], Left (renCtx' /\ 0), pure ((newlineIndentElem renCtx.indentationLevel) <> [thenElem, Rendering.spaceElem]), Left (renCtx' /\ 1),
                pure ((newlineIndentElem renCtx.indentationLevel) <> [elseElem, Rendering.spaceElem]), Left (renCtx' /\ 2)]
          {-
            ErrorBoundary -> Grammar.makeRule ["gamma", "insideType", "outsideType"] \[gamma, insideType, outsideType] ->
            [TermSort %|-* [gamma, insideType]]
            /\ -------
            ( TermSort %|-* [gamma, outsideType])
          -}
          ErrorBoundary /\ _ -> 
            let 
              Grammar.Rule ruleParams ruleKids ruleParent = TotalMap.lookup rule language
              matchRuleMetaVarName y (RuleMetaVar x) = x == y
              matchRuleMetaVarName _ _ = false
              insideType = sigma # lookup' (ruleParams # Set.filter (matchRuleMetaVarName "insideType") >>> (Set.toUnfoldable :: _ -> Array _) >>> flip index' 0)
              outsideType = sigma # lookup' (ruleParams # Set.filter (matchRuleMetaVarName "outsideType") >>> (Set.toUnfoldable :: _ -> Array _) >>> flip index' 0)
            in
            [ pure 
                [ HH.div [HP.classes [HH.ClassName "error-info ErrorBoundary-info"]] 
                  [ HH.div_ [HH.text $ "[type boundary]"]
                  , HH.div_ [HH.text $ "  actual type: ", renderTerm (Zipper (Path Nil) (sortToType insideType)) renCtx]
                  , HH.div_ [HH.text $ "expected type: ", renderTerm (Zipper (Path Nil) (sortToType outsideType)) renCtx]
                  ]
                ]
            , pure [errorLeftSide]
            , Left (renCtx /\ 0)
            , pure [errorRightSide] ]
          ConstantRule constant /\ _ -> [pure [HH.text (constantName constant)]]
          InfixRule op /\ _ ->
            [pure [Rendering.lparenElem], Left (renCtx /\ 0), pure [Rendering.spaceElem, HH.text (infixName op), Rendering.spaceElem], Left (renCtx /\ 1), pure [Rendering.rparenElem]]
          EqualsRule /\ _ ->
            [Left (renCtx /\ 0), pure [Rendering.spaceElem, HH.text "==", Rendering.spaceElem], Left (renCtx /\ 1)]
          ListRule /\ _ -> [pure [HH.text "List "], Left (renCtx /\ 0)]
          NilRule /\ _ -> [pure [HH.text "nil"]]
          ConsRule /\ _ -> [pure [HH.text "cons"]]
          HeadRule /\ _ -> [pure [HH.text "head"]]
          TailRule /\ _ -> [pure [HH.text "tail"]]
          LengthRule /\ _ -> [pure [HH.text "length"]]
          AppendRule /\ _ -> [pure [HH.text "append"]]
          IndexRule /\ _ -> [pure [HH.text "index"]]
          ListMatchRule /\ _ ->
            let renCtx' = Base.incremementIndentationLevel renCtx in
            [pure [HH.text "match "], Left (renCtx' /\ 0), pure [HH.text " with"], pure (newlineIndentElem renCtx.indentationLevel)
                , pure [HH.text "nil => "], Left (renCtx' /\ 1), pure (newlineIndentElem renCtx.indentationLevel)
                , pure [HH.text "cons "], Left (renCtx' /\ 2), pure [HH.text " "], Left (renCtx' /\ 3)
                , pure [HH.text " => "], Left (renCtx' /\ 4)]
          IntegerLiteral /\ _ -> [Left (renCtx /\ 0)]
          _ -> bug $
            "[STLC.Grammar.arrangeDerivTermSubs] no match" <> "\n" <>
            "  - rule = " <> pretty rule <> "\n" <>
            "  - sort = " <> show sort
    )
    in
    html <> [pure (if dotOrNot then [HH.div [classNames ["app-circle"]] [appCircle]] else [])]

lambdaElem = Rendering.makePuncElem "lambda" "fun"
mapstoElem = Rendering.makePuncElem "mapsto" "=>"
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
errorLeftSide = HH.div [classNames ["subnode", "punctuation"], HP.style "color: red; margin: 2px; line-height: 12px"] [HH.text "⦃"] --⦃⦄
errorRightSide = HH.div [classNames ["subnode", "punctuation"], HP.style "color: red; margin: 2px; line-height: 12px"] [HH.text "⦄"]

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

getAppliedWrapEdits :: String -> {-cursorSort-}Sort -> {-Term to be wrapped-}DerivTerm -> List Edit
getAppliedWrapEdits name cursorSort dterm
    | Just [cursorCtx, _cursorTy] <- matchExprImpl cursorSort (sor TermSort %$ [slot, slot])
    , Just [dtermCtx, ty] <- matchExprImpl (derivTermSort dterm) (sor TermSort %$ [slot, slot])
    , Just (_ /\ sub1) <- unify cursorCtx dtermCtx =
    let meta = fromMetaVar (freshMetaVar "any") in do
    case maximallyApplied cursorCtx meta ty dterm of
        Just (application /\ sub) ->
            DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange name cursorSort
                (subDerivTerm (Unification.composeSub sub1 sub) application)
        Nothing -> Nil
getAppliedWrapEdits _ _ _ = Nil
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
        DefaultEdits.makeSubEditFromTerm (newTermFromRule If) "if" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule Lam) "fun" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule Let) "let" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule App) "(" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule NilRule) "nil" cursorSort
        , getWrapInAppEdit "cons" cursorSort (newTermFromRule ConsRule)
        , getWrapInAppEdit "head" cursorSort (newTermFromRule HeadRule)
        , getWrapInAppEdit "tail" cursorSort (newTermFromRule TailRule)
        , getWrapInAppEdit "index" cursorSort (newTermFromRule IndexRule)
        , getWrapInAppEdit "length" cursorSort (newTermFromRule LengthRule)
        , getWrapInAppEdit "append" cursorSort (newTermFromRule AppendRule)
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule ListMatchRule) "match" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule EqualsRule) "==" cursorSort
    ] <> ((Util.allPossible :: Array Constant) <#>
        (\constant -> getWrapInAppEdit (constantName constant) cursorSort (newTermFromRule (ConstantRule constant))))
       <> ((Util.allPossible :: Array InfixOperator) <#>
        (\op -> DefaultEdits.makeSubEditFromTerm (newTermFromRule (InfixRule op)) (infixName op) cursorSort))
    )

editsAtCursor :: Sort -> Array Edit
editsAtCursor cursorSort = Array.mapMaybe identity (
    [
    DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Int)) "Int" cursorSort
    , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Bool)) "Bool" cursorSort
    , DefaultEdits.makeChangeEditFromTerm (newTermFromRule ListRule) "List" cursorSort
    -- , DefaultEdits.makeChangeEditFromTerm (newTermFromRule Comment) "Comment" cursorSort
    , makeEditFromPath (newPathFromRule Lam 2) "fun" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
    , makeEditFromPath (newPathFromRule App 0) "(" cursorSort
--    , makeEditFromPath (newPathFromRule Comment 1) "comment" cursorSort
--    , makeEditFromPath (newPathFromRule ErrorCall 0) "error" cursorSort

--    , makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort
--    , makeEditFromPath (newPathFromRule ArrowRule 1) "->" cursorSort
    ]) <> (Array.concat ((Util.allPossible :: Array InfixOperator) <#>
        (\op -> Array.fromFoldable $ DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange
            (infixName op) cursorSort (newTermFromRule (InfixRule op)))))
    <> Array.fromFoldable (getVarWraps cursorSort)
    <> (Array.fromFoldable $ DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange "->" cursorSort (newTermFromRule ArrowRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "cons" cursorSort (newTermFromRule ConsRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "head" cursorSort (newTermFromRule HeadRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "tail" cursorSort (newTermFromRule TailRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "index" cursorSort (newTermFromRule IndexRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "length" cursorSort (newTermFromRule LengthRule))
    <> (Array.reverse $ Array.fromFoldable $ getAppliedWrapEdits "append" cursorSort (newTermFromRule AppendRule))
    <> (Array.fromFoldable $ getAppliedWrapEdits "match" cursorSort (newTermFromRule ListMatchRule))
    <> (Array.fromFoldable $ DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange "if" cursorSort (newTermFromRule If))
    <> (Array.fromFoldable $ DefaultEdits.makeWrapEdits isValidCursorSort isValidSelectionSorts forgetSorts splitChange "==" cursorSort (newTermFromRule EqualsRule))
    <> (Array.concat ((Util.allPossible :: Array Constant) <#>
        (\constant -> Array.fromFoldable (getAppliedWrapEdits (constantName constant) cursorSort (newTermFromRule (ConstantRule constant))))))


--    [fromJust $ makeEditFromPath (newPathFromRule Lam 1)] -- [makeEditFromPath (newPathFromRule Lam 1)] -- Edit.defaultEditsAtCursor
--------------------------------------------------------------------------------
-- StepRules
--------------------------------------------------------------------------------

startCtx :: Sort
startCtx = sor CtxNilSort % [] -- NOTE: If this was changed, the interpreter would have to reflect that

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
passThroughArrow _ dterm =
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
        let varName = (MInj (Grammar.DataLabel (DataString "")) % []) in
        pure $
            dTERM Lam ["x" /\ varName, "a" /\ a, "b" /\ rEndpoint b, "gamma" /\ rEndpoint gamma] [
                    Smallstep.termToSSTerm $ Util.fromJust' "wrapLambda" $ (Grammar.defaultDerivTerm (Grammar.TypeOfLabel SortString %* [varName]))
                    , Smallstep.termToSSTerm (sortToType a)
                    , Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [plusChange (sor CtxConsSort) [varName, a] gamma [] , b]) $
                        t
                ])

-- down{Lam x : A. t}_(Term G (- A -> B)) ~~> down{t}_(Term (-x : A, G) B)
unWrapLambda :: StepRule
unWrapLambda _ (Expr (Smallstep.Boundary Smallstep.Down ch) [
        Expr (SSInj (Grammar.DerivLabel Lam sigma)) [_name, _ty, body]
    ])
    = do
        let varName = Util.lookup' (RuleMetaVar "x") sigma
        restOfCh <- case unit of
                    _ | Just ([a] /\ [gamma, b]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
                        -> pure (csor TermSort % [minusChange (sor CtxConsSort) [varName, a] gamma [] , b])
                    _ -> Nothing
        pure $
            Smallstep.wrapBoundary Smallstep.Down restOfCh $
                body

unWrapLambda _ _ = Nothing

-- TODO: its not clear to me that I've correctly thought through how this will work when
-- there are boundaries inside the input term
isNeutralFormWithoutCursor :: SSTerm -> Boolean
isNeutralFormWithoutCursor t = case t of
    (SSInj (DerivLabel Var _)) % _ -> true
    (SSInj (DerivLabel App _)) % [left, _] -> isNeutralFormWithoutCursor left
    (SSInj (DerivLabel GreyApp _)) % [left, _] -> isNeutralFormWithoutCursor left
    ((Boundary _ _) % [inside]) -> isNeutralFormWithoutCursor inside
    _ -> false

isNeutralFormWithCursor :: SSTerm -> Boolean
isNeutralFormWithCursor t = case t of
    (SSInj (DerivLabel Var _)) % _ -> true
    (SSInj (DerivLabel App _)) % [left, _] -> isNeutralFormWithCursor left
    (SSInj (DerivLabel GreyApp _)) % [left, _] -> isNeutralFormWithCursor left
    ((Boundary _ _) % [inside]) -> isNeutralFormWithCursor inside
    ((Marker 1) % [inside]) -> isNeutralFormWithCursor inside
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

wrapApp' :: StepRule
wrapApp' _ ((Smallstep.Boundary Smallstep.Up ch) % [inside])
    | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    =
    if not (isNeutralFormWithoutCursor inside) then Nothing else
    pure $
    Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, b]) $
        dTERM App ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b]
            [inside, Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (sor TermSort % [rEndpoint gamma, a]))]
wrapApp' _ _ = Nothing

-- If the argument to a greyed app is merely a hole, get rid of it
removeGreyedApp :: StepRule
removeGreyedApp _ dterm = case dterm of
    ((SSInj (DerivLabel GreyApp _)) % [
        t1,
        (SSInj (DerivLabel TermHole _)) % _
        ]) -> pure t1
    _ -> Nothing

-- App up{t1}_(Term G (- A -> B)) t2 ~~> up{t1}_(Term G B)
unWrapApp :: StepRule
unWrapApp = Smallstep.makeUpRule
    (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    (App %# [{-t1-}slot, {-t2-}slot])
    (\[t1, _t2] -> t1 /\ (\[_a] [gamma, b] inside -> pure $
        Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, b]) $
            inside))


makeAppGreyed :: StepRule
makeAppGreyed _ ((SSInj (Grammar.DerivLabel App _)) % [
        (Smallstep.Boundary Smallstep.Up ch) % [inside]
        , arg
    ])
    | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    = do
        pure $ Smallstep.wrapBoundary Up (csor TermSort % [gamma, b]) $
            (Smallstep.SSInj (makeLabel GreyApp ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b])) % [
                inside
                , Smallstep.wrapBoundary Down (csor TermSort % [gamma, inject a]) arg
            ]
makeAppGreyed _ _ = Nothing

--rehydrateApp :: StepRule
--rehydrateApp ((SSInj (Grammar.DerivLabel GreyApp sigma)) % [
--        (Smallstep.Boundary Smallstep.Up ch) % [inside]
--        , arg
--    ])
--    | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
--    =
--        trace "Go' here" \_ ->
--        if not (a == (Util.lookup' (Expr.RuleMetaVar "a") sigma)) then Nothing else pure $
--            trace "Not here" \_ ->
--            Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, b]) $
--                (Smallstep.SSInj (makeLabel App ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b])) % [
--                    inside
--                    , Smallstep.wrapBoundary Down (csor TermSort % [gamma, inject a]) arg
--                ]
--rehydrateApp _ = Nothing

-- this is to simulate re-hydration of greyed apps. The actually rehydrate rule had a priority order issue.
-- If an app is inside a grey app and the args are the same type and the App has a hole argument, merge them
mergeAppGreyApp :: StepRule
mergeAppGreyApp _ dterm = case dterm of
    ((SSInj (DerivLabel GreyApp greySigma)) % [
        (SSInj (DerivLabel App sigma)) % [
            t1
            , (SSInj (DerivLabel TermHole _) % _)
            ]
        , t2
        ])
    | Util.lookup' (RuleMetaVar "a") sigma == Util.lookup' (RuleMetaVar "a") greySigma
        -> pure (SSInj (DerivLabel App sigma) % [t1, t2])
    _ -> Nothing

-- The other way around
{-
NOTE: There is an issue with the cursor getting in between the grey app and the app.
This is not the only situation where something like that happens.
I'm not quite sure what I should do about it.
-}
mergeAppGreyApp2 :: StepRule
mergeAppGreyApp2 _ dterm = case dterm of
    ((SSInj (DerivLabel App sigma)) % [
        (SSInj (DerivLabel GreyApp greySigma)) % [
            t1
            , t2
            ]
        , (SSInj (DerivLabel TermHole _) % _)
        ])
    | Util.lookup' (RuleMetaVar "a") sigma == Util.lookup' (RuleMetaVar "a") greySigma
        -> pure (SSInj (DerivLabel App sigma) % [t1, t2])
    _ -> Nothing

removeError :: StepRule
removeError _ (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma)) [t])
    =
    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
--    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
    if insideType == outsideType then
        pure t
    else
        Nothing
removeError _ _ = Nothing

-- If the two sides of an error boundary unify, then remove the boundary automatically, propagating the diff
{-
This had some issues on the case of let f = lam x y . f x x, then wrap an app around the second x.

A problem: if I have something like
let f : Int -> ? = ...
let g : Int = ...
in f g
And I delete the type of g, it should put g in a type boundary. Its not acceptable if it just immediately
deletes the boundary.
-}
removeErrorPermissive :: StepRule
removeErrorPermissive _ (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma)) [t])
    =
    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
    trace "GONNA DO IT:" \_ ->
    if Maybe.isJust (unify insideType outsideType) then
        pure $ wrapBoundary Up (csor TermSort % [inject gamma, diff outsideType insideType]) t
    else
--        trace ("Didn't unify: insideType was: " <> pretty insideType <> " and outsideType was: " <> pretty outsideType) \_ ->
        Nothing
removeErrorPermissive _ _ = Nothing

mergeErrors :: StepRule
mergeErrors _ (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma1)) [
        Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma2)) [t]
    ])
    =
    let insideInside = Util.lookup' (Expr.RuleMetaVar "insideType") sigma2 in
    let outsideOutside = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma1 in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma1 in
    pure $ dTERM ErrorBoundary ["gamma" /\ gamma, "insideType" /\ insideInside, "outsideType" /\ outsideOutside] [t]
mergeErrors _ _ = Nothing

--introErrorDown :: StepRule
--introErrorDown ((Smallstep.Boundary Down ch) % [
--        inside
--    ])
--    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
--    =
--        pure $
--            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint c, "outsideType" /\ rEndpoint c]
--            [wrapBoundary Down (csor TermSort % [gamma, inject (lEndpoint c)]) inside]
--introErrorDown _ = Nothing

-- IDEA: a problem with this rule is that it might trigger before rules that propagate up through lets and stuff.
-- I might fix that by altering it so that it actually takes a form with the boundary as a child and
-- works on that.
introErrorUp :: StepRule
introErrorUp _ ((Smallstep.Boundary Up ch) % [
        inside
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
        pure $
            wrapBoundary Up (csor TermSort % [gamma, inject (lEndpoint c)]) $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint c, "outsideType" /\ lEndpoint c]
                [inside]
introErrorUp _ _ = Nothing

mergeErrorsUp :: StepRule
mergeErrorsUp _ (Expr.Expr (SSInj (Grammar.DerivLabel ErrorBoundary sigma)) [
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
mergeErrorsUp _ _ = Nothing

mergeErrorsDown :: StepRule
mergeErrorsDown _ (Expr.Expr (Boundary Down ch) [
        Expr.Expr (SSInj (DerivLabel ErrorBoundary sigma)) [t]
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    =
    let insideInside = Util.lookup' (RuleMetaVar "insideType") sigma in
    let outsideOutside = rEndpoint c in
    pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma,
            "insideType" /\ insideInside, "outsideType" /\ outsideOutside]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideInside]) t]
mergeErrorsDown _ _ = Nothing

fallbackDownError :: StepRule
fallbackDownError _ (Expr.Expr (Boundary Down ch) [ inside@(SSInj _ % _) ]) -- Only do the down rule if there is a regular derivterm below, if we did this on markers and Boundaries it might create a problem
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    = trace "fallbackDownError called" \_ ->
        pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ rEndpoint c]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideTy]) inside]
fallbackDownError _ _ = Nothing

-- Example of when this can be called: on Lesson 1, delete the type Int -> Int. Then, delete the boundary
-- around fibonacci in the definition.
fallbackUpError :: StepRule
fallbackUpError _ sterm =
    case sterm of
    (SSInj label@(DerivLabel _ _) % kids)
--    | Just [_gamma, outsideTy] <- matchExprImpl (Smallstep.ssTermSort outside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    -> do
        (gamma /\ c /\ inside /\ insideTy) /\ i <- flip Util.findWithIndex kids \_index -> case _ of
            (Boundary Up ch % [inside])
                | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
                , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
                -> pure $ gamma /\ c /\ inside /\ insideTy
            _ -> Nothing
        traceM ("fallbackUpError called")
        let outsideTy = lEndpoint c
        pure $
            (SSInj label % Util.fromJust (Array.updateAt i
                (wrapBoundary Up (csor TermSort % [gamma, inject outsideTy]) $
                    dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ outsideTy] [inside]) kids))
    _ -> Nothing

introErrorDownVar :: StepRule
introErrorDownVar _ ((Smallstep.Boundary Down ch) % [
        inside @((SSInj (DerivLabel Var _)) % _)
    ])
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , not (isMerelyASubstitution c)
    =
        pure $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint c, "outsideType" /\ rEndpoint c]
            [wrapBoundary Down (csor TermSort % [gamma, inject (lEndpoint c)]) inside]
introErrorDownVar _ _ = Nothing

-- Don't allow a down boundary to propagate into a neutral form
introDownErrorNeutral :: StepRule
introDownErrorNeutral parentTooth (Expr.Expr (Boundary Down ch) [ inside@(SSInj _ % _) ]) -- Only do the down rule if there is a regular derivterm below, if we did this on markers and Boundaries it might create a problem
    | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
    , isNeutralFormWithCursor inside
    , not (isMerelyASubstitution c)
    , case parentTooth of
        Just (SSInj (DerivLabel parentLabel _) /\ _ /\ 0) -> not (parentLabel == App || parentLabel == GreyApp)
        _ -> true
    , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
    = pure $ dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ rEndpoint c]
        [wrapBoundary Down (csor TermSort % [gamma, inject insideTy]) inside]
introDownErrorNeutral _ _ = Nothing

-- Don't allow a down boundary to propagate up from a neutral form
introUpErrorNeutral :: StepRule
introUpErrorNeutral _ (label % kids) = do
    (gamma /\ c /\ inside /\ insideTy) /\ i <- flip Util.findWithIndex kids \i -> case _ of
        (Boundary Up ch % [inside])
            | Just ([] /\ [gamma, c]) <- matchChange ch (TermSort %+- [{-gamma-}cSlot, {-c-}cSlot])
            , case label of
                SSInj (DerivLabel parentRule _) -> not ((parentRule == App || parentRule == GreyApp) && i == 0)
                _ -> true
            , isNeutralFormWithoutCursor inside
            , not (isMerelyASubstitution c)
            , Just [_gamma, insideTy] <- matchExprImpl (Smallstep.ssTermSort inside) (sor TermSort %$ [{-gamma-}slot, {-insideTy-}slot])
            -> pure $ gamma /\ c /\ inside /\ insideTy
        _ -> Nothing
    let outsideTy = lEndpoint c
    pure $
        (label % Util.fromJust (Array.updateAt i
            (wrapBoundary Up (csor TermSort % [gamma, inject outsideTy]) $
                dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ insideTy, "outsideType" /\ outsideTy] [inside]) kids))

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

alternateDesign :: Boolean -- A version of the editor where it doesn't automatically insert lambdas and apps
alternateDesign = false

stepRules :: List StepRule
stepRules = do
  let chLang = Smallstep.langToChLang language
  List.fromFoldable (
    [
    mergeErrorsDown
    , mergeErrorsUp
    , localBecomesNonlocal
    , nonlocalBecomesLocal
    , insertSucRule
    , removeSucRule
    , passThroughArrow
    , typeBecomeRhsOfChange
    , introDownErrorNeutral
    , introUpErrorNeutral
--    , introErrorDownVar
    , removeError
--    , removeErrorPermissive
    ]
    <> (if alternateDesign then [] else
    [
    wrapLambda
    , unWrapLambda
--    , rehydrateApp
    , mergeAppGreyApp
    , mergeAppGreyApp2
    , wrapApp
--    , unWrapApp
    , makeAppGreyed
    , removeGreyedApp
    ])
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
onDelete (MInj (TypeOfLabel SortString) % [s]) -- TODO: put in generic
    = Expr.CInj (MInj (TypeOfLabel SortString)) %
        [replaceChange s (MInj (Grammar.DataLabel (DataString "")) % [])]
-- TODO: If you want to be able to delete integer literals, put something here
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
{-
NOTE: this says you can't select integer textboxes.
If we want to be able to, then I need to finish implementing things in Generic.
It needs to have the correct sort on replace.
The code for that is in computeEdits of Buffer.purs. Currently it just assumes that its a String textbox.
-}
isValidCursorSort (MInj (Grammar.TypeOfLabel SortInt) % _) = false
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
keyAction _ (MInj (Grammar.TypeOfLabel _) % [_]) = Nothing -- Don't have newlines in literals!
keyAction "Enter" cursorSort =
        DefaultEdits.makeActionFromPath true forgetSorts splitChange (fst (newPathFromRule Newline 0))"newline" cursorSort
keyAction _ _ = Nothing

extraQueryEdits :: Sort -> String -> Array Edit
extraQueryEdits cursorSort query
    | Just [cursorCtx, cursorTy] <- matchExprImpl cursorSort (sor TermSort %$ [slot, slot]) =
        case Int.fromString query of
            Nothing -> []
            Just n ->
                let dterm = makeLabel IntegerLiteral ["gamma" /\ cursorCtx, "n" /\ MInj (DataLabel (DataInt n)) % []]
                        % [DerivLiteral (DataInt n) % []] in
                Array.mapMaybe identity [ DefaultEdits.makeSubEditFromTerm dterm (show n) cursorSort ]
extraQueryEdits _ _ = []

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
  , extraQueryEdits
  }

