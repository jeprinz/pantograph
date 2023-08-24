module Language.Pantograph.SULC where

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
import Language.Pantograph.Generic.Grammar ((%|-), (%|-*), sor)
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

data PreSortLabel
  = VarSort {-Ctx-} {-String-} {-Local or NonLocal-}
  | TermSort {-Ctx-}
  | CtxConsSort {-String-} {-Ctx-}
  | CtxNilSort
  | Local
  | NonLocal

derive instance Generic PreSortLabel _
instance Show PreSortLabel where show x = genericShow x
instance Eq PreSortLabel where eq x = genericEq x
instance Ord PreSortLabel where compare x y = genericCompare x y

instance Pretty PreSortLabel where
  pretty = show

instance IsExprLabel PreSortLabel where
  prettyExprF'_unsafe (VarSort /\ [gamma, x, locality]) = "Var" <+> parens gamma <+> x <+> "(" <> show locality <> ")"
  prettyExprF'_unsafe (TermSort /\ [gamma]) = "Term" <+> parens gamma
  prettyExprF'_unsafe (CtxConsSort /\ [x, gamma]) = x <> ", " <> gamma
  prettyExprF'_unsafe (CtxNilSort /\ []) = "∅"
  prettyExprF'_unsafe (Local /\ []) = "Local"
  prettyExprF'_unsafe (NonLocal /\ []) = "NonLocal"

  expectedKidsCount VarSort = 3
  expectedKidsCount TermSort = 1
  expectedKidsCount CtxConsSort = 2
  expectedKidsCount CtxNilSort = 0
  expectedKidsCount Local = 0
  expectedKidsCount NonLocal = 0

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
  | App
  | Ref
  | FreeVar
  | TermHole
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
  prettyExprF'_unsafe_RuleLabel (Lam /\ [x, b]) = P.parens $ "λ" <+> x <+> "↦" <+> b
  prettyExprF'_unsafe_RuleLabel (App /\ [f, a]) = P.parens $ f <+> a
  prettyExprF'_unsafe_RuleLabel (Ref /\ [x]) = "@" <> x
  prettyExprF'_unsafe_RuleLabel (TermHole /\ []) = "?"
  prettyExprF'_unsafe_RuleLabel (FormatRule Newline /\ [a]) = "<newline> " <> a
  prettyExprF'_unsafe_RuleLabel (FormatRule Comment /\ [str, a]) = " /* " <> str <> "*/ " <> a

  language = language

  isHoleRuleTotalMap = TotalMap.makeTotalMap case _ of
    TermHole -> true
    _ -> false

  -- Note from Jacob: TODO: can we put this particular function in EditorSpec instead of the typeclass? It really doesn't make sense in the typeclass.
  -- NOTE: a criteria is that defaultDerivTerm' should return something with a sort either equal to the input sort, or a specialization (substitution) of it. See the NameSortLabel case; it could input (Name ?x) but output something of the sort (Name "")
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel TermSort)) % [gamma]) = pure (Grammar.makeLabel TermHole ["gamma" /\ gamma] % [])
  defaultDerivTerm' (Expr.Meta (Right (Grammar.InjectSortLabel VarSort)) % [_gamma, _x, _locality]) = empty
  -- NOTE from jacob: I made it only work if given (Name (String _)) and not (Name ?x) because the latter shouldn't appear in programs.
--  defaultDerivTerm' (Expr.Meta (Right Grammar.NameSortLabel) % [Expr.Meta (Right (Grammar.StringSortLabel _str)) % []]) = pure $ Grammar.DerivString "" % []
  defaultDerivTerm' (Expr.Meta (Right Grammar.NameSortLabel) % [_]) = pure $ Grammar.DerivString "" % []
  defaultDerivTerm' sort = bug $ "[defaultDerivTerm] no match: " <> pretty sort

ctxCons x gamma = CtxConsSort %|-* [x, gamma]
infixl 7 ctxCons as %:

language :: Language
language = TotalMap.makeTotalMap case _ of

  Zero -> Grammar.makeRule ["gamma", "x"] \[gamma, x] ->
    []
    /\ --------
    ( VarSort %|-* [x %: gamma, x, Local %|-* []] )

  Suc -> Grammar.makeRule ["gamma", "x", "y", "locality"] \[gamma, x, y, locality] ->
    [ VarSort %|-* [gamma, x, locality] ]
    /\ --------
    ( VarSort %|-* [(y %: gamma), x, locality] )

  Lam -> Grammar.makeRule ["x", "gamma"] \[x, gamma] ->
    [ Grammar.NameSortLabel %* [x]
    , TermSort %|-* [x %: gamma] ]
    /\ --------
    ( TermSort %|-* [gamma])

  App -> Grammar.makeRule ["gamma"] \[gamma] ->
    [ TermSort %|-* [gamma]
    , TermSort %|-* [gamma] ]
    /\ --------
    ( TermSort %|-* [gamma] )

  FreeVar -> Grammar.makeRule ["name"] \[name] -> -- Note that name is stored in the FreeVar, but doesn't affect typing rules. This is fine
    []
    /\ --------
    ( VarSort %|-* [CtxNilSort %|-* [], name, NonLocal %|-* []] )

  Ref -> Grammar.makeRule ["gamma", "x", "locality"] \[gamma, x, locality] ->
    [ VarSort %|-* [gamma, x, locality] ]
    /\ --------
    ( TermSort %|-* [gamma] )

  TermHole -> Grammar.makeRule ["gamma"] \[gamma] ->
    [ ]
    /\ --------
    ( TermSort %|-* [gamma] )

  FormatRule Newline -> Grammar.makeRule ["gamma"] \[gamma] ->
    [ TermSort %|-* [gamma] ]
    /\ --------
    ( TermSort %|-* [gamma] )

  FormatRule Comment -> Grammar.makeRule ["comment", "gamma"] \[comment, gamma] ->
    [ Grammar.NameSortLabel %* [comment]
    , TermSort %|-* [gamma] ]
    /\ --------
    ( TermSort %|-* [gamma] )

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
    , _ ]) -> 
    -- TODO: use locality in rendering?
    [pure [nameElem str]]
  -- term
  Ref /\ _ -> 
    [pure [refElem], Left (renCtx /\ 0)]

  Lam /\ _ -> 
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem, lambdaElem], Left (renCtx /\ 0), pure [mapstoElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
  App /\ _ ->
    let renCtx' = Rendering.incremementIndentationLevel renCtx in
    [pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.spaceElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
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
    "[ULC.Grammar.arrangeDerivTermSubs] `TermHole` should be handled generically instead of here"
  _ -> bug $ 
    "[ULC.Grammar.arrangeDerivTermSubs] no match" <> "\n" <>
    "  - rule = " <> pretty rule <> "\n" <>
    "  - sort = " <> show sort

lambdaElem = Rendering.makePuncElem "lambda" "λ"
mapstoElem = Rendering.makePuncElem "mapsto" "↦"
refElem = Rendering.makePuncElem "ref" "#"
zeroVarElem = Rendering.makePuncElem "zeroVar" "Z"
sucVarElem = Rendering.makePuncElem "sucVar" "S"

nameElem str = HH.span [classNames ["name"]] [HH.text str]

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

type Edit = Edit.Edit PreSortLabel RuleLabel
type HoleyDerivZipper = Rendering.HoleyDerivZipper PreSortLabel RuleLabel

-- Makes an edit that inserts a path, and propagates the context change downwards
makeEditFromPath :: DerivPath Up /\ Sort -> String -> Sort -> Maybe Edit
makeEditFromPath (path /\ bottomOfPathSort) name cursorSort = do
--    let bottomOfPathSort = Grammar.concretizeSort bottomOfPathSort'
    let pathTopSort = Grammar.derivPathSort path bottomOfPathSort
    -- unify the top sort of the path with cursorSort
    -- make a downchange but no upchange
    pathTopSortSubbed /\ sub <- unify cursorSort pathTopSort
    let pathSubbed = map (Grammar.subDerivLabel sub) path
    let bottomOfPathSortSubbed = Expr.subMetaExprPartially sub bottomOfPathSort
    let change = SmallStep.getPathChange languageChanges pathSubbed bottomOfPathSortSubbed
    pure $ { label : name
    , action : defer \_ -> Edit.WrapAction
    {
        topChange : ChangeAlgebra.inject pathTopSortSubbed
        , dpath : pathSubbed -- DerivPath Up l r
        , botChange : ChangeAlgebra.invert change -- SortChange l
    }
    }

--assertHasVarSort :: Sort -> Sort /\ Sort
--assertHasVarSort (VarSort %|-* []) = ?h

-- returns a list of all indices in the context
getIndices :: Sort -> List DerivTerm
getIndices ctx = Expr.matchExpr2 ctx
    (sor CtxConsSort %$ [slot, slot]) (\[name, ctx'] ->
        let wrapInSuc var =
             Expr.matchExpr (Grammar.derivTermSort var) (sor VarSort %$ [slot , slot, slot]) \[gamma, x, locality] ->
             Grammar.makeLabel Suc [ "gamma" /\ gamma , "x" /\ x , "y" /\ name]
             % [var]
        in
        -- new var
        (Grammar.makeLabel Zero ["gamma" /\ ctx', "x" /\ name] % [])
        -- wrap vars from ctx' in a Suc
        : map wrapInSuc (getIndices ctx')
    )
    (sor CtxNilSort %$ []) (\[] ->
        Nil
    )

getVarEdits :: {-sort-}Sort -> List Edit
getVarEdits sort =
    Expr.matchExpr2 sort (sor TermSort %$ [slot]) (\[ctx] ->
            let wrapInRef index =
                 Expr.matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot , slot, slot]) \[gamma, x, locality] ->
                 Grammar.makeLabel Ref [ "gamma" /\ gamma , "x" /\ x, "locality" /\ locality]
                 % [index]
            in
            let indices = getIndices ctx in
            let makeEdit index =
                    Expr.matchExpr (Grammar.derivTermSort index) (sor VarSort %$ [slot, slot, slot]) \[_ctx2, name, locality] ->
                    let var = wrapInRef index in
                    {
                        label: Grammar.matchStringLabel name
                        , action: defer \_ -> Edit.ReplaceAction {
                            topChange: ChangeAlgebra.inject $ Grammar.derivTermSort var
                            , dterm: var
--                            topChange: ChangeAlgebra.inject $ Grammar.derivTermSort var
--                            , dterm: Grammar.makeLabel TermHole [] ["gamma" /\ ctx] % []
                        }
                    }
            in
            map makeEdit indices
        )
        -- If its not a TermSort, then there are no var edits
        slot \[_] -> Nil

editsAtHoleInterior cursorSort = (Array.fromFoldable (getVarEdits cursorSort))
    <> []
editsAtCursor cursorSort = Array.mapMaybe identity
    [makeEditFromPath (newPathFromRule Lam 1) "lambda" cursorSort,
    makeEditFromPath (newPathFromRule App 0) "appLeft" cursorSort]
--    [fromJust $ makeEditFromPath (newPathFromRule Lam 1)] -- [makeEditFromPath (newPathFromRule Lam 1)] -- Edit.defaultEditsAtCursor

--------------------------------------------------------------------------------
-- StepRules
--------------------------------------------------------------------------------

type StepRule = SmallStep.StepRule PreSortLabel RuleLabel

-- TODO:JACOB: propogate ctx changes down (and following rule)
-- down{i}_(VarSort (+ y, ctx) x _) -> Suc i
insertSucRule :: StepRule
insertSucRule = SmallStep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-y-}slot] {-ctx-}cSlot [], {-x-}cSlot, {-locality-}cSlot])
    {-i-}slot
    (\[y] [ctx, x, locality] [i] ->
        pure $ dTERM Suc ["gamma" /\ rEndpoint ctx, "x" /\ rEndpoint x, "y" /\ y, "locality" /\ rEndpoint locality] [i])
        -- x is type of var, y is type of thing added to ctx

-- diff 0 (A, B, 0) = (+A, +B, 0) 
-- i.e. adds to the LHS context in order to produce the RHS context i.e. what
-- change required of the first context to get second context

-- ALTERNATIVE I'M NOT USING: down {Z}_(VarSort (- y, ctx) x Local) ~~> up{S....S FreeVar}_(Var ctx A (Replace Local NonLocal))
-- INSTEAD I'M USING: down{Z}_(VarSort (- A, Gamma) A Local) ~> up{down{FreeVar}_(Var (diff 0 Gamma) A NonLocal))}_(VarSort Gamma A (Replace Local NonLocal))
localBecomesNonlocal :: StepRule
localBecomesNonlocal = SmallStep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-a-}slot] {-ctx-} cSlot [], {-a'-}cSlot, Local %+- []])
    (Zero %# [])
    (\[a] [ctx, a'] [] ->
        if not (ChangeAlgebra.inject a == a') then Maybe.Nothing else
        pure $ SmallStep.wrapBoundary SmallStep.Up (Expr.Replace (sor Local % []) (sor NonLocal % []) % [])
            (SmallStep.wrapBoundary SmallStep.Down (ChangeAlgebra.diff (CtxNilSort %|-* []) (rEndpoint ctx))
                (dTERM FreeVar ["name" /\ a] [])))

-- down{Suc i}_(VarSort (- y, ctx) x _) -> i
removeSucRule :: StepRule
removeSucRule = SmallStep.makeDownRule
    (VarSort %+- [dMINUS CtxConsSort [{-y-}slot] {-ctx-} cSlot [], {-x-}cSlot, {-locality-}cSlot])
    (Suc %# [{-i-}slot])
    (\[_y] [_ctx, _x, _locality] [i] -> pure $ i)

--- down{i}_(Var (+ A , ctx) A NonLocal) ~~> Z
nonlocalBecomesLocal :: StepRule
nonlocalBecomesLocal = SmallStep.makeDownRule
    (VarSort %+- [dPLUS CtxConsSort [{-a-}slot] {-ctx-} cSlot [], {-a'-}cSlot, NonLocal %+- []])
    {-i-}slot
    (\[a] [ctx, a'] [] ->
        if not (ChangeAlgebra.inject a == a') then Maybe.Nothing else
        pure $ dTERM Zero ["gamma" /\ rEndpoint ctx, "x" /\ a] [])

stepRules :: List StepRule
stepRules = do
  let chLang = SmallStep.langToChLang language
  List.fromFoldable
    [
    insertSucRule
    , removeSucRule
    , localBecomesNonlocal
    , nonlocalBecomesLocal
    , SmallStep.defaultDown chLang
    , SmallStep.defaultUp chLang
    ]

--------------------------------------------------------------------------------
-- EditorSpec
--------------------------------------------------------------------------------

removePathChanges _  = hole "removePathChanges"

editorSpec :: EditorSpec PreSortLabel RuleLabel
editorSpec =
  { dterm: assertI $ just "SULC dterm" $ 
      Grammar.defaultDerivTerm (TermSort %|-* [CtxNilSort %|-* []])
  , removePathChanges
  , editsAtCursor
  , editsAtHoleInterior
  , arrangeDerivTermSubs
  , stepRules
  , isValidCursorSort: const true
  , isValidSelectionSorts: const true
  }

