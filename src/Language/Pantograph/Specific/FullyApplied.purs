module Language.Pantograph.Specific.FullyApplied where

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
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust)
import Util as Util
import Language.Pantograph.Lib.DefaultEdits as DefaultEdits
import Language.Pantograph.Lib.GreyedRules as GreyedRules

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
  | App
  | GreyedApp
  | FunctionCall
  | Ref
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
  | Newline -- TODO: is this really an acceptable way for newlines to work? Its broken for applications, isn't it?
  | If -- TODO: should this be generalized in any way? Maybe for any type? For now I'll just do if.
  | ErrorCall
  | ErrorBoundary

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
  prettyExprF'_unsafe_RuleLabel (GreyedApp /\ [f, a]) = P.parens $ f <+> "<" <+> a <+> ">"
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ [ty]) = "(? : " <> ty <> ")"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel (FunctionCall /\ [neu]) = "call" <+> P.parens neu
  prettyExprF'_unsafe_RuleLabel (If /\ [c, t, e]) = "if" <+> c <+> "then" <+> t <+> "else" <+> e
  prettyExprF'_unsafe_RuleLabel (ErrorCall /\ [t]) = "{{" <+> t <+> "}}"
  prettyExprF'_unsafe_RuleLabel (ErrorBoundary /\ [t]) = "{{" <+> t <+> "}}"
  prettyExprF'_unsafe_RuleLabel other = bug ("[prettyExprF'...] the input was: " <> show other)

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    TypeHole -> true
    _ -> false

  defaultDerivTerm' (Expr.MInj (Grammar.SInj TermSort) % [gamma, ty])
    = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [sortToType ty])
  defaultDerivTerm' (Expr.MInj (Grammar.SInj VarSort) % [_gamma, _x, _ty, _locality]) = empty
  defaultDerivTerm' (Expr.MInj (Grammar.SInj TypeSort) % [ty]) =
    pure $ sortToType ty
  -- TODO: This case should probably be in Generic.
  defaultDerivTerm' (Expr.MInj (Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % [] -- TODO: this case should be in generic rather than here. In other words, the defaultDerivTerm in Grammar should do this case, and only hand the language specific cases to this function.
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

appRule :: Rule
appRule = Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ NeutralSort %|-* [gamma, Arrow %|-* [a, b]]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( NeutralSort %|-* [gamma, b] )

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

  App -> appRule

  GreyedApp -> GreyedRules.createGreyedConstruct appRule 0

  FreeVar -> Grammar.makeRule ["name", "type"] \[name, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, ty, NonLocal %|-* []] )

  Ref -> Grammar.makeRule ["gamma", "x", "type", "locality"] \[gamma, x, ty, locality] ->
    [ VarSort %|-* [gamma, x, ty, locality] ]
    /\ --------
    ( NeutralSort %|-* [gamma, ty] )

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

  ErrorCall -> Grammar.makeRule ["gamma", "insideType", "outsideType"] \[gamma, insideType, outsideType] ->
    [NeutralSort %|-* [gamma, insideType]]
    /\
    ( TermSort %|-* [gamma, outsideType])

  ErrorBoundary -> Grammar.makeRule ["gamma", "insideType", "outsideType"] \[gamma, insideType, outsideType] ->
    [TermSort %|-* [gamma, insideType]]
    /\
    ( TermSort %|-* [gamma, outsideType])

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

arrangeDerivTermSubs :: Unit -> Base.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx, rule, sort, sigma} = case rule /\ sort of
  _ /\ (Expr.MInj (Grammar.SInj VarSort) %
    [ _gamma
    , Expr.MInj (Grammar.StringSortLabel str) % []
    , _ty
    , locality ]) ->
    -- TODO: use locality in rendering?
    let postfix = if locality == sor Local % [] then "" else "!" in
    [pure [nameElem (str <> postfix)]]
  -- term
  Ref /\ _ ->
    [Left (renCtx /\ 0)]
  Lam /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem, lambdaElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [mapstoElem], Left (renCtx' /\ 2), pure [Rendering.rparenElem]]
  Let /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [letElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem]
        , pure (if renCtx.isInlined then [] else newlineIndentElem (renCtx.indentationLevel))
        , Left (renCtx /\ 3)]
  App /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [Left (renCtx' /\ 0), pure [Rendering.spaceElem], Left (renCtx' /\ 1)]
  GreyedApp /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.spaceElem, HH.text "<"], Left (renCtx' /\ 1), pure [HH.text ">", Rendering.rparenElem]]
  FunctionCall /\ _ ->
    [pure [Rendering.lparenElem], Left (renCtx /\ 0), pure [Rendering.rparenElem]]
  -- types
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
  TermHole /\ (Expr.MInj (Grammar.SInj TermSort) % [_gamma, ty])
    ->  [pure [Rendering.lbraceElem], Left (renCtx /\ 0), pure [colonElem]
        , Left (renCtx{cssClasses = Set.singleton "typesubscript"} /\ 1)
        , pure [Rendering.rbraceElem]]
--  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [colonElem, typeElem]]
  -- only has inner hole? So messes up keyboard cursor movement. TODO: fix.
  TypeHole /\ _ | Just (Expr.MV mv % []) <- Map.lookup (Expr.RuleMetaVar "type") sigma ->
    [Left (renCtx /\ 0), pure [HH.text (show (Base.getMetavarNumber renCtx mv))]]
  TypeHole /\ _ -> [Left (renCtx /\ 0), pure [HH.text ("error: " <> show (Map.lookup (Expr.RuleMetaVar "type") sigma))]]
  If /\ _ ->
    let renCtx' = Base.incremementIndentationLevel renCtx in
    [pure [ifElem], Left (renCtx /\ 0), pure ((newlineIndentElem renCtx.indentationLevel) <> [thenElem]), Left (renCtx' /\ 1),
        pure ((newlineIndentElem renCtx.indentationLevel) <> [elseElem]), Left (renCtx' /\ 2)]
  ErrorCall /\ _ -> [pure [errorLeftSide], Left (renCtx /\ 0), pure [errorRightSide]]
  ErrorBoundary /\ _ -> [pure [errorLeftSide], Left (renCtx /\ 0), pure [errorRightSide]]
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

newlineIndentElem :: forall t1 t2. Int -> Array (HH.HTML t1 t2)
--newlineIndentElem n = [Rendering.fillRightSpace, Rendering.newlineElem] <> Array.replicate n tabElem
newlineIndentElem n = [Rendering.newlineElem] <> Array.replicate n tabElem

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------
wrapInSuc :: {-name-}Sort -> {-ty-}Sort -> DerivTerm -> DerivTerm
wrapInSuc name ty t =
         Expr.matchExpr (Grammar.derivTermSort t) (sor VarSort %$ [slot, slot, slot, slot]) \[gamma, x, tyX, locality] ->
         Grammar.makeLabel Suc [ "gamma" /\ gamma , "x" /\ x, "typeX" /\ tyX, "y" /\ name, "typeY" /\ ty, "locality" /\ locality]
         % [t]

-- gets a free variable in a context
getFreeVar :: {-The context-}Sort -> {-The name-} Sort -> {-The type-} Sort -> DerivTerm
getFreeVar ctx fvName fvType = Expr.matchExpr2 ctx
    (sor CtxConsSort %$ [slot, slot, slot]) (\[name, ty, ctx'] ->
        wrapInSuc name ty (getFreeVar ctx' fvName fvType)
    )
    (sor CtxNilSort %$ []) (\[] ->
        Grammar.makeLabel FreeVar ["name" /\ fvName, "type" /\ fvType] % []
    )

-- returns a list of all indices in the context
getIndices :: Sort -> List DerivTerm
getIndices ctx = Expr.matchExpr2 ctx
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
                        Grammar.makeLabel App ["gamma" /\ gamma, "a" /\ a, "b" /\ b] % [neu, Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ a] % [sortToType a]]
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
splitChange c =
    case ChangeAlgebra.eliminateReplaces c of
        c | Maybe.Just ([] /\ [ctx, ty]) <- Expr.matchChange c (TermSort %+- [cSlot, cSlot])
        ->
            let _ctx1 /\ ctx2 = ChangeAlgebra.endpoints ctx in
            let ty1 /\ _ty2 = ChangeAlgebra.endpoints ty in
            {upChange: csor TermSort % [ChangeAlgebra.inject ctx2, ty]
            , cursorSort: sor TermSort % [ctx2, ty1]
            , downChange: csor TermSort % [ctx, ChangeAlgebra.inject ty1]}
        c | Maybe.Just ([] /\ [_ty]) <- Expr.matchChange c (TypeSort %+- [cSlot])
        -> {upChange: c, cursorSort: lEndpoint c, downChange: ChangeAlgebra.inject (lEndpoint c)}
        c | Maybe.Just ([] /\ [_gamma, _ty]) <- Expr.matchChange c (NeutralSort %+- [cSlot, cSlot])
        -> {upChange: ChangeAlgebra.inject (rEndpoint c), cursorSort: rEndpoint c, downChange: c}
        -- TODO: maybe could just generalize this to when c is the identity?
        ((Expr.Inject (Expr.MV _)) % []) ->
            {upChange: c, cursorSort: rEndpoint c, downChange: c}
        c -> bug ("splitChange - got c = " <> pretty c)

makeEditFromPath = DefaultEdits.makeEditFromPath languageChanges splitChange

editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> Array.mapMaybe identity [
        DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Int)) "Int" cursorSort
        , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule String)) "String" cursorSort
        , DefaultEdits.makeChangeEditFromTerm (newTermFromRule (DataTypeRule Bool)) "Bool" cursorSort
        , DefaultEdits.makeSubEditFromTerm (newTermFromRule If) "If" cursorSort
    ]

editsAtCursor cursorSort = Array.mapMaybe identity
    [
    makeEditFromPath (newPathFromRule Lam 2) "lambda" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
    , makeEditFromPath (newPathFromRule ArrowRule 1) "arrow" cursorSort
    , makeEditFromPath (newPathFromRule App 0) "app" cursorSort
--    , makeEditFromPath (newPathFromRule ErrorCall 0) "error" cursorSort

--    , makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort
--    , makeEditFromPath (newPathFromRule ArrowRule 1) "->" cursorSort
    , makeEditFromPath (newPathFromRule Newline 0 )"newline" cursorSort
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

passThroughArrow :: StepRule
passThroughArrow dterm =
    case dterm of
    ((Smallstep.Boundary Smallstep.Down ch) % [
        (Inject (Grammar.DerivLabel ArrowRule _sigma)) % [da, db]
    ])
    | Just ([] /\ [ca, cb]) <- Expr.matchChange ch (TypeSort %+- [Arrow %+- [{-ca-}cSlot, {-cb-}cSlot]])
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
        let varName = (Expr.MInj (Grammar.StringSortLabel "") % []) in
        pure $
            dTERM Lam ["x" /\ varName, "a" /\ a, "b" /\ rEndpoint b, "gamma" /\ rEndpoint gamma] [
                    Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (Grammar.NameSortLabel %* [varName]))
                    , dTERM TypeHole ["type" /\ a] []
                    , Smallstep.wrapBoundary Smallstep.Down (csor TermSort % [Expr.plusChange (sor CtxConsSort) [varName, a] gamma [] , b]) $
                        t
                ])

-- down{Lam x : A. t}_(Term G (- A -> B)) ~~> down{t}_(Term (-x : A, G) B)
unWrapLambda :: StepRule
unWrapLambda (Expr.Expr (Smallstep.Boundary Smallstep.Down ch) [
        Expr.Expr (Inject (Grammar.DerivLabel Lam sigma)) [_name, _ty, body]
    ])
    = do
        let varName = Util.lookup' (Expr.RuleMetaVar "x") sigma
        restOfCh <- case unit of
                    _ | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
                        -> pure (csor TermSort % [Expr.minusChange (sor CtxConsSort) [varName, a] gamma [] , b])
                    -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
                    _ | Just ([a, b, x] /\ [gamma]) <- Expr.matchChange ch
                            (TermSort %+- [{-gamma-}cSlot, Expr.replaceChange ((Expr.InjectMatchLabel (sor Arrow)) % [{-a-}slot, {-b-}slot]) {-x-}slot])
                      , (Expr.MV _ % _) <- x
                        -> pure (csor TermSort % [Expr.minusChange (sor CtxConsSort) [varName, a] gamma [], Expr.replaceChange b x])
                    _ -> Nothing
        pure $
            Smallstep.wrapBoundary Smallstep.Down restOfCh $
                body

unWrapLambda _ = Nothing
--    = case term of
--      (Expr.Expr (Boundary Down inputCh) [inputDeriv]) -> do

-- up{t}_(Term G (+ A -> B)) ~~> up{App t ?}_(Term G B)
wrapApp :: StepRule
wrapApp = Smallstep.makeUpRule
    (NeutralSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    {-t-}slot
    (\[t] -> t /\ (\[a] [gamma, b] inside -> pure $
        Smallstep.wrapBoundary Smallstep.Up (csor NeutralSort % [gamma, b]) $
            dTERM App ["gamma" /\ rEndpoint gamma, "a" /\ a, "b" /\ rEndpoint b]
                [inside, Smallstep.termToSSTerm $ Util.fromJust' "wrapApp" $ (Grammar.defaultDerivTerm (sor TermSort % [rEndpoint gamma, a]))]))

-- App up{t1}_(Term G (- A -> B)) t2 ~~> up{t1}_(Term G B)
unWrapApp :: StepRule
unWrapApp = Smallstep.makeUpRule
    (NeutralSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
    (App %# [{-t1-}slot, {-t2-}slot])
    (\[t1, _t2] -> t1 /\ (\[_a] [gamma, b] inside -> pure $
        Smallstep.wrapBoundary Smallstep.Up (csor NeutralSort % [gamma, b]) $
            inside))

makeAppGreyed :: StepRule
makeAppGreyed ((Inject (Grammar.DerivLabel App sigma)) % [
        (Smallstep.Boundary Smallstep.Up ch) % [inside]
        , arg
    ])
    = do
        restOfCh <- case unit of
                    _ | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (NeutralSort %+- [{-gamma-}cSlot, dMINUS Arrow [{-a-}slot] {-b-}cSlot []])
                        -> pure (csor NeutralSort % [gamma, b])
                    -- This is for dealing with the case where the user for some reason deletes some output arrows of a function type.
                    _ | Just ([a, b, x] /\ [gamma]) <- Expr.matchChange ch
                            (NeutralSort %+- [{-gamma-}cSlot, Expr.replaceChange ((Expr.InjectMatchLabel (sor Arrow)) % [{-a-}slot, {-b-}slot]) {-x-}slot])
                      , (Expr.MV _ % _) <- x
                        -> pure (csor NeutralSort % [gamma, Expr.replaceChange b x])
                    _ -> Nothing
        let sigma' = Map.insert (Expr.RuleMetaVar "anything") (Smallstep.ssTermSort inside) sigma -- The "anything" refers to whats in createGreyedConstruct in GreyedRules.purs
        pure $ Smallstep.wrapBoundary Smallstep.Up restOfCh $
            (Smallstep.Inject (Grammar.DerivLabel GreyedApp sigma')) % [
                inside
                , arg
            ]
makeAppGreyed _ = Nothing

removeGreyedHoleArg :: StepRule
removeGreyedHoleArg ((Inject (Grammar.DerivLabel GreyedApp _)) % [
        inside
        , (Inject (Grammar.DerivLabel TermHole _) % _)
    ])
    = pure inside
removeGreyedHoleArg _ = Nothing

rehydrateApp :: StepRule
rehydrateApp ((Inject (Grammar.DerivLabel GreyedApp sigma)) % [
        (Smallstep.Boundary Smallstep.Up ch) % [inside]
        , arg
    ])
    | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (NeutralSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    =
        if not (a == (Util.lookup' (Expr.RuleMetaVar "a") sigma)) then Nothing else pure $
        let sigma' = Map.delete (Expr.RuleMetaVar "anything") sigma in -- The "anything" refers to whats in createGreyedConstruct in GreyedRules.purs
            Smallstep.wrapBoundary Smallstep.Up (csor NeutralSort % [gamma, b]) $
                (Smallstep.Inject (Grammar.DerivLabel App sigma')) % [
                    inside
                    , arg
                ]
rehydrateApp _ = Nothing

-- Two kinds of boundaries that I am thinking of. The first is ErrorCall which replaces FunctionCall:
replaceCallWithError :: StepRule
replaceCallWithError = Smallstep.makeUpRule
    (NeutralSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
    (FunctionCall %# [{-t-}slot])
    (\[t] -> t /\ (\[] [gamma, ty] inside -> pure $
            dTERM ErrorCall ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
                [inside]))

replaceErrorWithCall :: StepRule
replaceErrorWithCall (Expr.Expr (Inject (Grammar.DerivLabel ErrorCall sigma)) [t])
    =
    let insideType = Util.lookup' (Expr.RuleMetaVar "insideType") sigma in
    let outsideType = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma in
    if insideType == outsideType then
        pure $ dTERM FunctionCall ["gamma" /\ gamma, "type" /\ insideType] [t]
    else
        Nothing
replaceErrorWithCall _ = Nothing

-- The second is Error which wraps a term

wrapCallInErrorUp :: StepRule
wrapCallInErrorUp ((Inject (Grammar.DerivLabel FunctionCall sigma)) % [
        (Smallstep.Boundary Smallstep.Up ch) % [inside]
    ])
    | Just ([] /\ [gamma, ty]) <- Expr.matchChange ch (NeutralSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
    =
    if Maybe.isJust (Expr.matchChange ty (dPLUS Arrow [{-a-}slot] {-b-}cSlot [])) || ChangeAlgebra.isMerelyASubstitution ty
        then Nothing
        else pure $
            let outsideType = Util.lookup' (Expr.RuleMetaVar "type") sigma in
            Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, ChangeAlgebra.inject outsideType]) $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
                [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ rEndpoint ty] [inside ]]
wrapCallInErrorUp _ = Nothing

--wrapCallInErrorUp2 :: StepRule
--wrapCallInErrorUp2 = Smallstep.makeUpRule
--    (NeutralSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    (FunctionCall %# [slot])
--    (\[t] ->
--        t /\ (\[] [gamma, ty] inside ->
--        -- Should only create an error boundary if both 1) this is not a change that could create an application and 2) this is not merely a substitution that could be propagated through
--        if Maybe.isJust (Expr.matchChange ty (dPLUS Arrow [{-a-}slot] {-b-}cSlot [])) || ChangeAlgebra.isMerelyASubstitution ty
--            then Nothing
--            else pure $
--                Smallstep.wrapBoundary Smallstep.Up (csor TermSort % [gamma, ChangeAlgebra.inject (rEndpoint ?h)]) $
--                dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
--                    [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ rEndpoint ty] [inside ]]))
--wrapCallInErrorUp :: StepRule
--wrapCallInErrorUp = Smallstep.makeUpRule
--    (TermSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    {-t-}slot
--    (\[t] -> t /\ (\[] [gamma, ty] inside ->
--            case Expr.matchDiffExprs Smallstep.compareMatchLabel t (FunctionCall %# [slot]) of
--                    Nothing -> Nothing
--                    Just _ -> pure $
--                        dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ rEndpoint ty, "outsideType" /\ lEndpoint ty]
--                            [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ rEndpoint ty] [inside]]))
wrapCallInErrorDown :: StepRule
wrapCallInErrorDown ((Smallstep.Boundary Smallstep.Down ch) % [
        (Inject (Grammar.DerivLabel FunctionCall sigma)) % [inside]
    ])
    | Just ([] /\ [gamma, ty]) <- Expr.matchChange ch (TermSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
    =
            if ChangeAlgebra.isMerelyASubstitution ty then Nothing else
            let insideType = Util.lookup' (Expr.RuleMetaVar "type") sigma in
            pure $
            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint ty, "outsideType" /\ rEndpoint ty]
                [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ lEndpoint ty] [
                    Smallstep.wrapBoundary Smallstep.Down (csor NeutralSort % [gamma, ChangeAlgebra.inject insideType]) inside
                ]]
wrapCallInErrorDown _ = Nothing

--wrapCallInErrorDown :: StepRule
--wrapCallInErrorDown = Smallstep.makeDownRule
--    (TermSort %+- [{-gamma-}cSlot, {-ty-}cSlot])
--    (FunctionCall %# [{-inside-}slot])
--    (\[] [gamma, ty] [inside] ->
--            if ChangeAlgebra.isMerelyASubstitution ty then Nothing else
--            pure $
--            dTERM ErrorBoundary ["gamma" /\ rEndpoint gamma, "insideType" /\ lEndpoint ty, "outsideType" /\ rEndpoint ty]
--                [dTERM FunctionCall ["gamma" /\ rEndpoint gamma, "type" /\ lEndpoint ty] [
--                    Smallstep.wrapBoundary Smallstep.Down (csor NeutralSort % [gamma, ChangeAlgebra.inject ?h]) inside
--                ]])

removeError :: StepRule
removeError (Expr.Expr (Inject (Grammar.DerivLabel ErrorBoundary sigma)) [t])
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
mergeErrors (Expr.Expr (Inject (Grammar.DerivLabel ErrorBoundary sigma1)) [
        Expr.Expr (Inject (Grammar.DerivLabel ErrorBoundary sigma2)) [t]
    ])
    =
    let insideInside = Util.lookup' (Expr.RuleMetaVar "insideType") sigma2 in
    let outsideOutside = Util.lookup' (Expr.RuleMetaVar "outsideType") sigma1 in
    let gamma = Util.lookup' (Expr.RuleMetaVar "gamma") sigma1 in
    pure $ dTERM ErrorBoundary ["gamma" /\ gamma, "insideType" /\ insideInside, "outsideType" /\ outsideOutside] [t]
mergeErrors _ = Nothing

{-
This is necessary because the wrapApp rule conflicts with the defaultUp, and the priority order of the list isn't enough
because defaultUp happens on a term higher up in the tree.
-}
isUpInCall :: SSTerm -> Boolean
isUpInCall (Expr.Expr (Inject (Grammar.DerivLabel FunctionCall _)) [
        Expr.Expr (Smallstep.Boundary Smallstep.Up ch) [_]
    ])
    | Just ([a] /\ [gamma, b]) <- Expr.matchChange ch (NeutralSort %+- [{-gamma-}cSlot, dPLUS Arrow [{-a-}slot] {-b-}cSlot []])
    = true
--    = not (ChangeAlgebra.isMerelyASubstitution ty)
--    = ChangeAlgebra.isMerelyASubstitution ty
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
    , passThroughArrow
    , typeBecomeRhsOfChange
    , wrapLambda
    , unWrapLambda
    , rehydrateApp
    , wrapApp
--    , unWrapApp
    , makeAppGreyed
    , wrapCallInErrorUp
    , wrapCallInErrorDown
    , removeError
    , mergeErrors
    , Smallstep.defaultDown chLang
    , Smallstep.unless isUpInCall (Smallstep.defaultUp chLang)
--    , replaceCallWithError
--    , replaceErrorWithCall
    , removeGreyedHoleArg
    ]

onDelete :: Sort -> SortChange
onDelete cursorSort
    | Maybe.Just [ty] <- Expr.matchExprImpl cursorSort (sor TypeSort %$ [slot])
    = csor TypeSort % [Expr.replaceChange ty (Expr.fromMetaVar (Expr.freshMetaVar "deleted"))]
onDelete (Expr.MInj Grammar.NameSortLabel % [s])
    = Expr.Inject (Expr.MInj Grammar.NameSortLabel) %
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
        bottom: (Expr.MInj (Grammar.SInj NeutralSort) % _)
        , top: (Expr.MInj (Grammar.SInj NeutralSort) % _)
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
  , languageChanges
  , onDelete
  , generalizeDerivation
  , specializeDerivation
  , forgetSorts
  , clipboardSort
  }

