module Language.Pantograph.Specific.October14 where

import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
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
import Language.Pantograph.Generic.Rendering.Base as Rendering
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
  | NeutralSort {-Ctx-} {-Type-}
  | TypeSort {-Type-}
  -- Contexts
  | CtxConsSort {-String-} {-Type-} {-Ctx-}
  | CtxNilSort
  -- Types
  | DataType DataType
  | Arrow {-Type-} {-Type-}
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
  prettyExprF'_unsafe (NeutralSort /\ [gamma, ty]) = "Neutral" <+> parens gamma <+> ty
  prettyExprF'_unsafe (TypeSort /\ [t]) = "Type" <+> parens t
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, "∅"]) = x <> ":" <> ty
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, gamma]) = x <> ":" <> ty <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"
  prettyExprF'_unsafe (Local /\ []) = "Local"
  prettyExprF'_unsafe (NonLocal /\ []) = "NonLocal"
  prettyExprF'_unsafe (DataType ty /\ []) = show ty
  prettyExprF'_unsafe (Arrow  /\ [a, b]) = a <> " -> " <> b


  expectedKidsCount VarSort = 4
  expectedKidsCount TermSort = 2
  expectedKidsCount NeutralSort = 2
  expectedKidsCount TypeSort = 1
  expectedKidsCount CtxConsSort = 3
  expectedKidsCount CtxNilSort = 0
  expectedKidsCount Local = 0
  expectedKidsCount NonLocal = 0
  expectedKidsCount (DataType _) = 0
  expectedKidsCount Arrow = 2

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
type Query = Rendering.Query
type Output = Rendering.Output PreSortLabel RuleLabel
type HoleyDerivZipper = Rendering.HoleyDerivZipper PreSortLabel RuleLabel

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
  | App
  | FunctionCall
  | Ref
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
  | Newline -- TODO: is this really an acceptable way for newlines to work? Its broken for applications, isn't it?
  | If -- TODO: should this be generalized in any way? Maybe for any type? For now I'll just do if.

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
sortToType (Expr.Expr (Expr.Meta (Right (Grammar.InjectSortLabel (DataType dt)))) []) =
    Grammar.makeLabel (DataTypeRule dt) [] % []
sortToType (Expr.Expr (Expr.Meta (Right (Grammar.InjectSortLabel Arrow))) [a, b]) =
    Grammar.makeLabel ArrowRule ["a" /\ a, "b" /\ b] % [sortToType a, sortToType b]
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
  prettyExprF'_unsafe_RuleLabel (DataTypeRule dataType /\ []) = pretty dataType
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ []) = "?"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel (FunctionCall /\ [neu]) = "call" <+> P.parens neu
  prettyExprF'_unsafe_RuleLabel (If /\ [c, t, e]) = "if" <+> c <+> "then" <+> t <+> "else" <+> e
  prettyExprF'_unsafe_RuleLabel other = bug ("[prettyExprF'...] the input was: " <> show other)

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    TypeHole -> true
    _ -> false

  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % [gamma, ty]) = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [])
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel VarSort)) % [_gamma, _x, _ty, _locality]) = empty
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel TypeSort)) % [ty]) =
    pure $ sortToType ty
--    pure (Grammar.makeLabel TypeHole ["type" /\ ty] % [])
  defaultDerivTerm' (Expr.Meta (Right Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % [] -- TODO: this case should be in generic rather than here. In other words, the defaultDerivTerm in Grammar should do this case, and only hand the language specific cases to this function.
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

  FunctionCall -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ NeutralSort %|-* [gamma, ty] ]
    /\
    ( TermSort %|-* [gamma, ty] )

  Lam -> Grammar.makeRule ["x", "a", "b", "gamma"] \[x, a, b, gamma] ->
    [ Grammar.NameSortLabel %* [x]
    , TypeSort %|-* [a]
    , TermSort %|-* [CtxConsSort %|-* [x, a, gamma], b] ]
    /\ --------
    ( TermSort %|-* [gamma, Arrow %|-* [a, b]])

  App -> Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ NeutralSort %|-* [gamma, Arrow %|-* [a, b]]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( NeutralSort %|-* [gamma, b] )

  FreeVar -> Grammar.makeRule ["name", "type"] \[name, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, ty, NonLocal %|-* []] )

  Ref -> Grammar.makeRule ["gamma", "x", "type", "locality"] \[gamma, x, ty, locality] ->
    [ VarSort %|-* [gamma, x, ty, locality] ]
    /\ --------
    ( NeutralSort %|-* [gamma, ty] )

  TermHole -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ ]
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

arrangeDerivTermSubs :: Unit -> Rendering.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx, rule, sort} = case rule /\ sort of
  _ /\ (Expr.Meta (Right (Grammar.InjectSortLabel VarSort)) %
    [ _gamma
    , Expr.Meta (Right (Grammar.StringSortLabel str)) % []
    , _ty
    , locality ]) ->
    -- TODO: use locality in rendering?
    let postfix = if locality == sor Local % [] then "" else "!" in
    [pure [nameElem (str <> postfix)]]
  -- term
  Ref /\ _ ->
    [pure [refElem], Left (renCtx /\ 0)]
  Lam /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem, lambdaElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [mapstoElem], Left (renCtx' /\ 2), pure [Rendering.rparenElem]]
  Let /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [letElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem]
        , pure (if renCtx.isInlined then [] else [Rendering.newlineElem])
        , Left (renCtx' /\ 3)]
  App /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.spaceElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
  FunctionCall /\ _ ->
    [Left (renCtx /\ 0)]
  -- types
  DataTypeRule dataType /\ _ ->
    [pure [dataTypeElem (pretty dataType)]]
  ArrowRule /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [Left (renCtx' /\ 0), pure [arrowElem], Left (renCtx' /\ 1)]
  -- format
  Newline /\ _ ->
    Array.concat
      [ if renCtx.isInlined then [] else
        [pure $ [Rendering.spaceElem] <> [Rendering.newlineElem] <> Array.replicate renCtx.indentationLevel Rendering.indentElem]
      , [Left (renCtx /\ 0)] ]
  -- hole
  TermHole /\ (Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % [_gamma, ty])
    ->  -- TODO TODO TODO: it shouldn't just display the type as text using pretty. Ideally it should produce HTML.
        [Left (renCtx /\ 0), pure [colonElem, HH.text (pretty ty)]]
  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [colonElem, typeElem]]
  If /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [ifElem], Left (renCtx /\ 0), pure [Rendering.newlineElem, thenElem], Left (renCtx' /\ 1), pure [Rendering.newlineElem, elseElem], Left (renCtx' /\ 2)]
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

typeElem = Rendering.makePuncElem "Type" "Type"

nameElem str = HH.span [classNames ["name"]] [HH.text str]
dataTypeElem str = HH.span [classNames ["datatype"]] [HH.text str]

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

-- returns a list of all indices in the context
getIndices :: Sort -> List DerivTerm
getIndices ctx = Expr.matchExpr2 ctx
    (sor CtxConsSort %$ [slot, slot, slot]) (\[name, ty, ctx'] ->
        let wrapInSuc var =
             Expr.matchExpr (Grammar.derivTermSort var) (sor VarSort %$ [slot, slot, slot, slot]) \[gamma, x, tyX, locality] ->
             Grammar.makeLabel Suc [ "gamma" /\ gamma , "x" /\ x, "typeX" /\ tyX, "y" /\ name, "typeY" /\ ty, "locality" /\ locality]
             % [var]
        in
        -- new var
        (Grammar.makeLabel Zero ["gamma" /\ ctx', "x" /\ name, "type" /\ ty] % [])
        -- wrap vars from ctx' in a Suc
        : map wrapInSuc (getIndices ctx')
    )
    (sor CtxNilSort %$ []) (\[] ->
        Nil
    )

getVarEdits :: {-sort-}Sort -> List Edit
getVarEdits sort =
    Expr.matchExpr2 sort (sor TermSort %$ [slot, slot]) (\[ctx, ty] ->
            let wrapInRef index =
                 Expr.matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot , slot, slot, slot]) \[gamma, x, ty, locality] ->
                 Grammar.makeLabel Ref [ "gamma" /\ gamma , "x" /\ x, "type" /\ ty, "locality" /\ locality]
                 % [index]
            in
            let wrapArgs :: DerivTerm -> DerivTerm
                wrapArgs neu = case Grammar.derivTermSort neu of
                    neuSort | Maybe.Just [gamma, a, b] <- Expr.matchExprImpl neuSort (sor NeutralSort %$ [{-gamma-}slot, sor Arrow %$ [{-a-}slot, {-b-}slot]]) ->
                        wrapArgs $
                        Grammar.makeLabel App ["gamma" /\ gamma, "a" /\ a, "b" /\ b] % [neu, Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ a] % []]
                    neuSort | Maybe.Just [gamma, t] <- Expr.matchExprImpl neuSort (sor NeutralSort %$ [{-gamma-}slot, {-t-}slot]) ->
                        Grammar.makeLabel FunctionCall ["gamma" /\ gamma, "type" /\ t] % [neu]
            in
            let indices = getIndices ctx in
            let makeEdit index =
                    Expr.matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot, slot]) \[_ctx2, name, _varTy, _locality] ->
                    do
                        let var = wrapInRef index
                        let neutral = wrapArgs var
                        [_gamma, neutralTy] <- Expr.matchExprImpl (Grammar.derivTermSort neutral) (sor TermSort %$ [slot, slot])
                        _newTy /\ sub <- unify neutralTy ty -- think about order
                        pure {
                            label: Grammar.matchStringLabel name
                            , action: defer \_ -> Edit.FillAction {
                                sub , dterm: neutral
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
splitChange c | Maybe.Just ([] /\ [ctx, ty]) <- Expr.matchChange c (TermSort %+- [cSlot, cSlot])
    =
        let _ctx1 /\ ctx2 = ChangeAlgebra.endpoints ctx in
        let ty1 /\ _ty2 = ChangeAlgebra.endpoints ty in
        {upChange: csor TermSort % [ChangeAlgebra.inject ctx2, ty]
        , cursorSort: sor TermSort % [ctx2, ty1]
        , downChange: csor TermSort % [ctx, ChangeAlgebra.inject ty1]}
splitChange c | Maybe.Just ([] /\ [_ty]) <- Expr.matchChange c (TypeSort %+- [cSlot])
    = {upChange: c, cursorSort: lEndpoint c, downChange: ChangeAlgebra.inject (lEndpoint c)}
splitChange c | Maybe.Just ([] /\ [_gamma, _ty]) <- Expr.matchChange c (NeutralSort %+- [cSlot, cSlot])
    = {upChange: ChangeAlgebra.inject (rEndpoint c), cursorSort: rEndpoint c, downChange: c}
-- TODO TODO TODO: Also implement the case where the change is just a metavariable identity! Later: I don't remember why I wrote this, maybe I'll find out later.
splitChange c = bug ("splitChange - got c = " <> pretty c)

makeEditFromTerm = DefaultEdits.makeEditFromTerm
makeEditFromPath = DefaultEdits.makeEditFromPath languageChanges splitChange

editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> Array.mapMaybe identity [
        makeEditFromTerm (newTermFromRule (DataTypeRule Int)) "Int" cursorSort
        , makeEditFromTerm (newTermFromRule (DataTypeRule String)) "String" cursorSort
        , makeEditFromTerm (newTermFromRule (DataTypeRule Bool)) "Bool" cursorSort
        , makeEditFromTerm (newTermFromRule If) "If" cursorSort
    ]

editsAtCursor cursorSort = Array.mapMaybe identity
    [
    makeEditFromPath (newPathFromRule Lam 2) "lambda" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
    , makeEditFromPath (newPathFromRule ArrowRule 1) "arrow" cursorSort
    , makeEditFromPath (newPathFromRule App 0) "app" cursorSort

--    , makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort
--    , makeEditFromPath (newPathFromRule ArrowRule 1) "->" cursorSort
--    , makeEditFromPath (newPathFromRule (FormatRule Newline) 0 )"newline" cursorSort
    ]
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
        pure $ Smallstep.wrapBoundary Smallstep.Up (csor VarSort % [ChangeAlgebra.inject (rEndpoint ctx), a', ty', Expr.Replace (sor Local % []) (sor NonLocal % []) % []])
            (Smallstep.wrapBoundary Smallstep.Down (csor VarSort % [ChangeAlgebra.diff (CtxNilSort %|-* []) (rEndpoint ctx), ChangeAlgebra.inject a, ChangeAlgebra.inject ty, csor NonLocal % []])
                (dTERM FreeVar ["name" /\ a, "type" /\ ty] [])))

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

typeBecomeRhsOfChange :: StepRule
typeBecomeRhsOfChange = Smallstep.makeDownRule
    (TypeSort %+- [{-c-}cSlot])
    {-t-}slot
    (\[] [c] [_t] -> pure (Smallstep.termToSSTerm (sortToType (rEndpoint c))))

-- down{t}_(Term G (+ A -> B)) ~~> Lam ~ : A. down{t}_(Term (+ ~:A, G) B)
wrapLambda :: StepRule
wrapLambda = Smallstep.makeDownRule
    (TermSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    {-t-}slot
    (\[a] [gamma, b] [t] ->
        let varName = (Expr.Meta (Right (Grammar.StringSortLabel "")) % []) in
        pure $
            dTERM Lam ["x" /\ varName, "a" /\ a, "b" /\ rEndpoint b, "gamma" /\ rEndpoint gamma] [
                    Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (Grammar.NameSortLabel %* [varName]))
                    , dTERM TermHole ["gamma" /\ rEndpoint gamma, "type" /\ a] []
                    , Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [Expr.plusChange (sor CtxConsSort) [varName, a] gamma [] , b]) $
                        t
                ])

-- down{Lam x : A. t}_(Term G (- A -> B)) ~~> down{t}_(Term (-x : A, G) B)
--unWrapLambda :: StepRule
--unWrapLambda = Smallstep.makeDownRule
--    (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
--    (Lam %# [{-name-}slot, {-ty-} slot,{-t-}slot])
--    (\[a] [gamma, b] [name, _ty, t] ->
--        pure $
--            Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [Expr.minusChange (sor CtxConsSort) [?name, a] gamma [] , b]) $
--                t
--                )

unWrapLambda2 :: StepRule
unWrapLambda2 (Expr.Expr (Smallstep.Boundary Smallstep.Down ch) [
        Expr.Expr (Inject (Grammar.DerivLabel Lam sigma)) [_name, _ty, body]
    ])
    | Just ([a] /\ [gamma, b]) <- trace "got hereee" \_ -> Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    =
    let varName = Util.lookup' (Expr.RuleMetaVar "x") sigma in
    pure $
        Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [Expr.minusChange (sor CtxConsSort) [varName, a] gamma [] , b]) $
            body

unWrapLambda2 _ = Nothing
--    = case term of
--      (Expr.Expr (Boundary Down inputCh) [inputDeriv]) -> do

-- up{t}_(Term G (+ A -> B)) ~~> up{App t ?}_(Term G B)
wrapApp :: StepRule
wrapApp = Smallstep.makeUpRule
    (NeutralSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    {-t-}slot
    (\[t] -> t /\ (\[a] [gamma, b] inside ->
        Smallstep.wrapBoundary Smallstep.Up (csor NeutralSort % [gamma, b]) $
            dTERM App ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b]
                [inside, Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (sor TermSort % [rEndpoint gamma, a]))]))

-- App up{t1}_(Term G (- A -> B)) t2 ~~> up{t1}_(Term G B)
unWrapApp :: StepRule
unWrapApp = Smallstep.makeUpRule
    (NeutralSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    (App %# [{-t1-}slot, {-t2-}slot])
    (\[t1, _t2] -> t1 /\ (\[_a] [gamma, b] inside ->
        Smallstep.wrapBoundary Smallstep.Up (csor NeutralSort % [gamma, b]) $
            inside))

{-
This is necessary because the wrapApp rule conflicts with the defaultUp, and the priority order of the list isn't enough
because defaultUp happens on a term higher up in the tree.
-}
isUpInCall :: SSTerm -> Boolean
isUpInCall (Expr.Expr (Inject (Grammar.DerivLabel FunctionCall _)) [
        Expr.Expr (Smallstep.Boundary Smallstep.Up ch) [_]
    ])
    | Just ([] /\ [gamma, ty]) <- Expr.matchChange ch (NeutralSort %+- [cSlot, cSlot])
    = not (ChangeAlgebra.isMerelyASubstitution ty)
isUpInCall _ = false

stepRules :: List StepRule
stepRules = do
  let chLang = Smallstep.langToChLang language
  List.fromFoldable
    [
    localBecomesNonlocal
    , nonlocalBecomesLocal
    , insertSucRule
    , removeSucRule
    , typeBecomeRhsOfChange
    , wrapLambda
    , unWrapLambda2
    , wrapApp
    , unWrapApp
    , Smallstep.defaultDown chLang
    , Smallstep.unless isUpInCall (Smallstep.defaultUp chLang)
    ]

onDelete :: Sort -> SortChange
onDelete cursorSort
    | Maybe.Just [ty] <- Expr.matchExprImpl cursorSort (sor TypeSort %$ [slot])
    = csor TypeSort % [Expr.replaceChange ty (Expr.fromMetaVar (Expr.freshMetaVar "deleted"))]
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
clipboardSort _other = Expr.fromMetaVar (Expr.freshMetaVar "anySort")

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
  , isValidCursorSort: const true
  , isValidSelectionSorts: const true
  , languageChanges
  , onDelete
  , generalizeDerivation
  , specializeDerivation
  , forgetSorts
  , clipboardSort
  }

