module Language.Pantograph.Specific.STLC where

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
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Data.Variant (Variant)
import Debug (traceM, trace)
import Debug as Debug
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML as HH
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.ChangeAlgebra (rEndpoint)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Edit (newPathFromRule)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar ((%|-), (%|-*), sor, csor)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (EditorSpec)
import Language.Pantograph.Generic.Rendering.Base as Rendering
import Language.Pantograph.Generic.Rendering.Console (logConsole)
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Language.Pantograph.Generic.Smallstep ((%+-), dPLUS, dMINUS, (%#))
import Language.Pantograph.Generic.Smallstep (StepExprLabel(..), cSlot, dTERM)
import Language.Pantograph.Generic.Smallstep as SmallStep
import Language.Pantograph.Generic.Unification (unify)
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Text.Pretty as P
import Type.Direction (Up)
import Util (fromJust)

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
  -- Big ones
  = VarSort {-Ctx-} {-String-} {-Type-} {-Local or NonLocal-}
  | TermSort {-Ctx-} {-Type-}
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
  prettyExprF'_unsafe (TypeSort /\ [t]) = "Type" <+> parens t
  prettyExprF'_unsafe (CtxConsSort /\ [x, ty, gamma]) = x <> ":" <> ty <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"
  prettyExprF'_unsafe (Local /\ []) = "Local"
  prettyExprF'_unsafe (NonLocal /\ []) = "NonLocal"
  prettyExprF'_unsafe (DataType ty /\ []) = show ty
  prettyExprF'_unsafe (Arrow  /\ [a, b]) = a <> " -> " <> b


  expectedKidsCount VarSort = 4
  expectedKidsCount TermSort = 2
  expectedKidsCount TypeSort = 1
  expectedKidsCount CtxConsSort = 3
  expectedKidsCount CtxNilSort = 0
  expectedKidsCount Local = 0
  expectedKidsCount NonLocal = 0
  expectedKidsCount (DataType _) = 0
  expectedKidsCount Arrow = 2

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

type Expr = Expr.Expr PreSortLabel
type MetaExpr = Expr.MetaExpr PreSortLabel
type Zipper = Expr.Zipper PreSortLabel
type Tooth = Expr.Tooth PreSortLabel
type Sort = Grammar.Sort PreSortLabel

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
  | Ref
  | FreeVar
  | TermHole
  | TypeHole
  | DataTypeRule DataType
  | ArrowRule
  | FormatRule Format

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

data Format
  = Newline
  | Comment

derive instance Generic Format _
derive instance Eq Format
derive instance Ord Format
instance Show Format where show x = genericShow x
instance Enum Format where
  pred x = genericPred x
  succ x = genericSucc x
instance Bounded Format where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language PreSortLabel RuleLabel
type Rule = Grammar.Rule PreSortLabel

instance Grammar.IsRuleLabel PreSortLabel RuleLabel where
  prettyExprF'_unsafe_RuleLabel (Zero /\ []) = pretty Zero
  prettyExprF'_unsafe_RuleLabel (Suc /\ [x]) = pretty Suc <> x
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, ty, b]) = P.parens $ "λ" <+> x <+> ":" <+> ty <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (Let /\ [x, ty, a, b]) = P.parens $ "let" <+> x <+> ":" <+> ty <+> "=" <+> a <+> b
  prettyExprF'_unsafe_RuleLabel (ArrowRule /\ [a, b]) = P.parens $ a <+> "->" <+> b
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ []) = "?"
  prettyExprF'_unsafe_RuleLabel (TypeHole /\ []) = "?<type>"
  prettyExprF'_unsafe_RuleLabel (FormatRule Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FormatRule Comment /\ [str, a]) = " /* " <> str <> "*/ " <> a
  prettyExprF'_unsafe_RuleLabel (FreeVar /\ []) = "free"
  prettyExprF'_unsafe_RuleLabel other = bug ("[prettyExprF'...] the input was: " <> show other)

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    TypeHole -> true -- Jacob: what does setting this flag do exactly?
    _ -> false

  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % [gamma, ty]) = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma, "type" /\ ty] % [])
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel VarSort)) % [_gamma, _x, _ty, _locality]) = empty
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel TypeSort)) % [ty]) = pure (Grammar.makeLabel TypeHole ["type" /\ ty] % [])
  defaultDerivTerm' (Expr.Meta (Right Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % []
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

--ctxCons x gamma = CtxConsSort %|-* [x, gamma]
--infixl 7 ctxCons as %:

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

  App -> Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %|-* [gamma, Arrow %|-* [a, b]]
    , TermSort %|-* [gamma, a] ]
    /\ --------
    ( TermSort %|-* [gamma, b] )

  FreeVar -> Grammar.makeRule ["name", "type"] \[name, ty] ->
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, ty, NonLocal %|-* []] )

  Ref -> Grammar.makeRule ["gamma", "x", "type", "locality"] \[gamma, x, ty, locality] ->
    [ VarSort %|-* [gamma, x, ty, locality] ]
    /\ --------
    ( TermSort %|-* [gamma, ty] )

  TermHole -> Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ ]
    /\ --------
    ( TermSort %|-* [gamma, ty] )

  FormatRule Newline -> Grammar.makeRule ["s"] \[s] ->
    [ s ]
    /\ --------
    ( s )

  FormatRule Comment -> Grammar.makeRule ["comment", "gamma"] \[comment, s] ->
    [ Grammar.NameSortLabel %* [comment]
    , s ]
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


--------------------------------------------------------------------------------
-- DerivTerm (and friends)
--------------------------------------------------------------------------------

type DerivTerm = Grammar.DerivTerm PreSortLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir PreSortLabel RuleLabel
type DerivZipper = Grammar.DerivZipper PreSortLabel RuleLabel
type DerivZipperp = Grammar.DerivZipperp PreSortLabel RuleLabel

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges PreSortLabel RuleLabel
type SortChange = Grammar.SortChange PreSortLabel
type ChangeRule = Grammar.ChangeRule PreSortLabel

languageChanges :: LanguageChanges
languageChanges = Grammar.defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  _ -> identity

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

type Query = Rendering.Query
type Output = Rendering.Output PreSortLabel RuleLabel

arrangeDerivTermSubs :: Unit -> Rendering.ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs _ {renCtx, rule, sort} = case rule /\ sort of
  _ /\ (Expr.Meta (Right (Grammar.InjectSortLabel VarSort)) % 
    [ _gamma
    , Expr.Meta (Right (Grammar.StringSortLabel str)) % [] 
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
    [pure [letElem], Left (renCtx /\ 0), pure [colonElem], Left (renCtx /\ 1), pure [equalsElem], Left (renCtx' /\ 2), pure [inElem], Left (renCtx' /\ 3)]
  App /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.spaceElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
  -- types
  DataTypeRule dataType /\ _ ->
    [pure [dataTypeElem (pretty dataType)]]
  ArrowRule /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [Left (renCtx' /\ 0), pure [arrowElem], Left (renCtx' /\ 1)]
  -- format
  FormatRule Newline /\ _ ->
    Array.concat
      [ if renCtx.isInlined then [] else
        [pure $ [Rendering.spaceElem] <> [Rendering.newlineElem] <> Array.replicate renCtx.indentationLevel Rendering.indentElem]
      , [Left (renCtx /\ 0)] ]
  FormatRule Comment /\ _ ->
    [ pure [Rendering.commentBeginElem]
    , Left (renCtx /\ 0)
    , pure [Rendering.commentEndElem]
    , Left (renCtx /\ 1)]
  -- hole 
  TermHole /\ _ -> bug $
    "[STLC.Grammar.arrangeDerivTermSubs] `TermHole` should be handled generically instead of here"
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
arrowElem = Rendering.makePuncElem "arrow" "->"

nameElem str = HH.span [classNames ["name"]] [HH.text str]
dataTypeElem str = HH.span [classNames ["datatype"]] [HH.text str]

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

type Edit = Edit.Edit PreSortLabel RuleLabel
type HoleyDerivZipper = Rendering.HoleyDerivZipper PreSortLabel RuleLabel

{-
This function is used both for inserting and deleting paths.

Should have the property that if it maps c -> c1 /\ s /\ c2, then
c = c1 o c2, and   _ --[c1]--> s --[c2]--> _
-}
splitTopBottomChange :: SortChange -> SortChange /\ Sort /\ SortChange
splitTopBottomChange c
    | Maybe.Just ([] /\ [ctx, ty]) <- Expr.matchChange c (TermSort %+- [cSlot, cSlot])
    =
        let ctx1 /\ ctx2 = ChangeAlgebra.endpoints ctx in
        let ty1 /\ ty2 = ChangeAlgebra.endpoints ty in
        csor TermSort % [ChangeAlgebra.inject ctx1, ty]
        /\ sor TermSort % [ctx1, ty2]
        /\ csor TermSort % [ctx, ChangeAlgebra.inject ty2]
--        csor TermSort % [ctx, tyBot]
--splitTopBottomChange c
--    | Maybe.Just ([] /\ [tyTop]) <- Expr.matchChange c (TypeSort %+- [cSlot])
--    , Maybe.Just ([] /\ [tyBot]) <- Expr.matchChange bottomSort (TypeSort %+- [cSlot])
--    = csor TypeSort % [tyBot]
splitTopBottomChange c = bug ("splitTopBottomChange - got c = " <> pretty c)

-- Makes an edit that inserts a path, and propagates the context change downwards and type change upwards
makeEditFromPath :: DerivPath Up /\ Sort -> String -> Sort -> Maybe Edit
makeEditFromPath (path /\ bottomOfPathSort) name cursorSort = do
    let change = SmallStep.getPathChange languageChanges path bottomOfPathSort
    let preTopChange /\ preCursorSort /\ preBotChange = splitTopBottomChange (ChangeAlgebra.invert change)
--    _ /\ sub <- unify cursorSort preCursorSort -- TODO: should these arguments to unify be flipped? Does it matter?
    _ /\ sub <- unify preCursorSort cursorSort
    let topChange = ChangeAlgebra.subSomeMetaChange sub preTopChange
    let botChange = ChangeAlgebra.subSomeMetaChange sub preBotChange
    let pathSubbed = map (Grammar.subDerivLabel sub) path
    pure $ { label : name
    , action : defer \_ -> Edit.WrapAction
    {
        topChange: ChangeAlgebra.invert topChange
        , dpath : pathSubbed -- DerivPath Up l r
        , botChange
    }
    }

--assertHasVarSort :: Sort -> Sort /\ Sort
--assertHasVarSort (VarSort %|-* []) = ?h

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
            let indices = getIndices ctx in
            let makeEdit index =
                    Expr.matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot, slot]) \[_ctx2, name, varTy, _locality] ->
                    do
                        newTy /\ sub <- unify varTy ty -- think about order
                        let var = wrapInRef index
                        pure {
                            label: Grammar.matchStringLabel name
                            , action: defer \_ -> Edit.ReplaceAction {
                                -- TODO TODO TODO TODO TODO: needs to actually output the correct change, based on sub!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                -- should map sub by (x |-> e) -> x |-> (Replace x e), and then use that on ty?????
                                topChange: ChangeAlgebra.inject $ Grammar.derivTermSort var
                                , dterm: var
                            }
                        }
            in
            List.mapMaybe makeEdit indices
        )
        -- If its not a TermSort, then there are no var edits
        slot \[_] -> Nil

editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> []
editsAtCursor cursorSort = Array.mapMaybe identity
    [
    makeEditFromPath (newPathFromRule Lam 2) "lambda" cursorSort
    , makeEditFromPath (newPathFromRule Let 3) "let" cursorSort
--    , makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort
--    , makeEditFromPath (newPathFromRule ArrowRule 1) "->" cursorSort
--    , makeEditFromPath (newPathFromRule (FormatRule Newline) 0 )"newline" cursorSort
    ]
--    [fromJust $ makeEditFromPath (newPathFromRule Lam 1)] -- [makeEditFromPath (newPathFromRule Lam 1)] -- Edit.defaultEditsAtCursor

--------------------------------------------------------------------------------
-- StepRules
--------------------------------------------------------------------------------

type StepRule = SmallStep.StepRule PreSortLabel RuleLabel

-- down{i}_(VarSort (+ y : Y, ctx) x X locality) -> Suc down{i}_(VarSort ctx x X locality)
insertSucRule :: StepRule
insertSucRule = SmallStep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-y-}slot, {-typeY-}slot] {-ctx-}cSlot [], {-x-}cSlot, {-ty-}cSlot,{-locality-}cSlot])
    {-i-}slot
    (\[y, typeY] [ctx, x, typeX, locality] [i] ->
        pure $
            SmallStep.wrapBoundary SmallStep.Down (csor VarSort % [ctx, x, typeX, locality]) $
            dTERM Suc ["gamma" /\ rEndpoint ctx, "x" /\ rEndpoint x, "typeX" /\ rEndpoint typeX,
                "y" /\ y, "typeY" /\ typeY, "locality" /\ rEndpoint locality] [i])
        -- x is type of var, y is type of thing added to ctx

-- diff 0 (A, B, 0) = (+A, +B, 0) 
-- i.e. adds to the LHS context in order to produce the RHS context i.e. what
-- change required of the first context to get second context

-- down{Z}_(VarSort (- A : ty, Gamma) A ty Local) ~> up{down{FreeVar}_(Var (diff 0 Gamma) A ty NonLocal))}_(VarSort Gamma A ty (Replace Local NonLocal))
localBecomesNonlocal :: StepRule
localBecomesNonlocal = SmallStep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-a-}slot, {-ty-}slot] {-ctx-} cSlot [], {-a'-}cSlot, {-ty'-}cSlot, Local %+- []])
    (Zero %# [])
    (\[a, ty] [ctx, a', ty'] [] ->
        if not (ChangeAlgebra.inject a == a' && ChangeAlgebra.inject ty == ty') then Maybe.Nothing else
        pure $ SmallStep.wrapBoundary SmallStep.Up (csor VarSort % [ctx, a', ty', Expr.Replace (sor Local % []) (sor NonLocal % []) % []])
            (SmallStep.wrapBoundary SmallStep.Down (csor VarSort % [ChangeAlgebra.diff (CtxNilSort %|-* []) (rEndpoint ctx), ChangeAlgebra.inject a, ChangeAlgebra.inject ty, csor NonLocal % []])
                (dTERM FreeVar ["name" /\ a, "type" /\ ty] [])))

-- down{Suc i}_(VarSort (- y : TypeY, ctx) x typeX locality) -> down{i}_(VarSort ctx x typeX locality)
removeSucRule :: StepRule
removeSucRule = SmallStep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-y-}slot, {-typeY-}slot] {-ctx-} cSlot [], {-x-}cSlot, {-typeX-}cSlot, {-locality-}cSlot])
    (Suc %# [{-i-}slot])
    (\[_y, _typeY] [ctx, x, typeX, locality] [i] -> pure $
        SmallStep.wrapBoundary SmallStep.Down (csor VarSort % [ctx, x, typeX, locality]) $
        i)

--- down{i}_(Var (+ A : ty , ctx) A ty NonLocal) ~~> Z
nonlocalBecomesLocal :: StepRule
nonlocalBecomesLocal = SmallStep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-a-}slot, {-ty-}slot] {-ctx-} cSlot [], {-a'-}cSlot, {-ty'-}cSlot, NonLocal %+- []])
    {-i-}slot
    (\[a, ty] [ctx, a', ty'] [] ->
        if not (ChangeAlgebra.inject a == a' && ChangeAlgebra.inject ty == ty') then Maybe.Nothing else
        pure $ dTERM Zero ["gamma" /\ rEndpoint ctx, "x" /\ a, "type" /\ ty] [])

-- down{t}_(Type (+ A -> B)) ~~> [A] -> down{t}_B
--arrowWrap :: StepRule
--arrowWrap = ?h

stepRules :: List StepRule
stepRules = do
  let chLang = SmallStep.langToChLang language
  List.fromFoldable
    [
--    insertSucRule
--    , removeSucRule
--    , localBecomesNonlocal
--    , nonlocalBecomesLocal
      SmallStep.defaultDown chLang
    , SmallStep.defaultUp chLang
    ]

removePathChanges ::
  {bottomSort :: Sort, topSort :: Sort} ->
  {downChange :: SortChange, upChange :: SortChange, cursorSort :: Sort}
removePathChanges {bottomSort, topSort} = {
    downChange: ChangeAlgebra.diff bottomSort topSort
    , upChange: ChangeAlgebra.inject topSort
    , cursorSort: topSort
}

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "SULC dterm" $ 
      Grammar.defaultDerivTerm (TermSort %|-* [CtxNilSort %|-* [], Expr.fromMetaVar (Expr.freshMetaVar "tyhole")])
  , removePathChanges
  , editsAtCursor
  , editsAtHoleInterior
  , arrangeDerivTermSubs
  , stepRules
  , isValidCursorSort: const true
  , isValidSelectionSorts: const true
  }

