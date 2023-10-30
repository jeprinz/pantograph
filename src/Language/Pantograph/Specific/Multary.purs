module Language.Pantograph.Specific.Multary where

import Prelude

import Language.Pantograph.Generic.Grammar
import Data.Expr
import Language.Pantograph.Generic.ChangeAlgebra
import Language.Pantograph.Generic.Smallstep (wrapBoundary, Direction(..))

import Data.Expr as Expr
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
  | TypeSort {-Type-}
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


  expectedKidsCount VarSort = 4
  expectedKidsCount TermSort = 2
  expectedKidsCount ArgListSort = 2
  expectedKidsCount TypeSort = 1
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
  | Let
  | ArgsListNil
  | ArgsListCons
  | GreyedArgsListCons
  | FunctionCall
  | LocalVar
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
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
sortToType (Expr.Expr (Expr.MInj (Grammar.SInj (DataType dt))) []) =
    Grammar.makeLabel (DataTypeRule dt) [] % []
sortToType (Expr.Expr (Expr.MInj (Grammar.SInj Arrow)) [a, b]) =
    Grammar.makeLabel ArrowRule ["a" /\ a, "b" /\ b] % [sortToType a, sortToType b]
sortToType ty =
    Grammar.makeLabel TypeHole ["type" /\ ty] % []

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = pretty Zero
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = pretty Suc <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, ty, b]) = P.parens $ "λ" <+> x <+> ":" <+> ty <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (Let /\ [x, ty, a, b]) = P.parens $ "let" <+> x <+> ":" <+> ty <+> "=" <+> a <+> b
  prettyExprF'_unsafe_RuleLabel (ArrowRule /\ [a, b]) = P.parens $ a <+> "->" <+> b
  prettyExprF'_unsafe_RuleLabel (DataTypeRule dataType /\ []) = pretty dataType
  prettyExprF'_unsafe_RuleLabel (ArgsListNil /\ []) = "[]"
  prettyExprF'_unsafe_RuleLabel (ArgsListCons /\ [arg, rest]) = arg <+> ":" <+> rest
  prettyExprF'_unsafe_RuleLabel (GreyedArgsListCons /\ [arg, rest]) = arg <+> ":" <+> rest
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

  defaultDerivTerm' (Expr.MInj (Grammar.SInj TermSort) % [gamma, ty])
    = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [sortToType ty])
  defaultDerivTerm' (Expr.MInj (Grammar.SInj VarSort) % [_gamma, _x, _ty, _locality]) = Nothing
  defaultDerivTerm' (Expr.MInj (Grammar.SInj TypeSort) % [ty]) =
    pure $ sortToType ty
  -- TODO: This case should probably be in Generic.
  defaultDerivTerm' (Expr.MInj (Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % [] -- TODO: this case should be in generic rather than here. In other words, the defaultDerivTerm in Grammar should do this case, and only hand the language specific cases to this function.
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

--appRule :: Rule
--appRule = Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
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

  Lam -> Grammar.makeRule ["x", "a", "b", "gamma"] \[x, a, b, gamma] ->
    [ Grammar.NameSortLabel %* [x]
    , TypeSort %|-* [a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], b] ]
    /\ --------
    ( TermSort %|-* [gamma, Arrow %|-* [a, b]])

  ArgsListNil -> Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    ( ArgListSort %|-* [gamma, TypeListNil %|-* []] )

  ArgsListCons -> Grammar.makeRule ["gamma", "t", "ts"] \[gamma, t, ts] ->
    [ TermSort %|-* [gamma, t]
    , ArgListSort %|-* [gamma, ts]]
    /\ -------
    ( ArgListSort %|-* [gamma, TypeListCons %|-* [t, ts]] )

  GreyedArgsListCons -> Grammar.makeRule ["gamma", "t", "ts"] \[gamma, t, ts] ->
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
    [TypeSort %|-* [a], TypeSort %|-* [b]]
    /\ --------
    ( TypeSort %|-* [Arrow %|-* [a, b]] )

  If -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
      [ TermSort %|-* [gamma, DataType Bool %|-* []]
      , TermSort %|-* [gamma, ty]
      , TermSort %|-* [gamma, ty] ]
      /\
      ( TermSort %|-* [gamma, ty] )

-- TODO TODO TODO: figure out where the type errors will go!!!