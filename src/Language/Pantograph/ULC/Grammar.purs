module Language.Pantograph.ULC.Grammar where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Bug.Assertion (assert, assertInput, positif)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Expr (class IsExprLabel, Meta(..), Tooth(..), expectedKidsCount, freshMetaVar, freshMetaVar', fromMetaVar, (%), (%<))
import Data.Expr as Expr
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, defer)
import Data.List ((:))
import Data.List as List
import Data.List.Rev (RevList)
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap, wrap)
import Data.Ord.Generic (genericCompare)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.TotalMap as TotalMap
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, defaultLanguageChanges, makeRule)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering (Action(..), Edit)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, parens, pretty, (<+>))

--------------------------------------------------------------------------------
-- ExprLabel
--------------------------------------------------------------------------------

data ExprLabel
  -- Var
  = Zero
  | Suc {-Var-}
  -- Term
  | Lam {-Var-} {-Term-}
  | App {-Term-} {-Term-}
  | Ref {-Var-}
  -- Hole
  | Hole
  -- HoleInterior
  | HoleInterior
  -- Sort
  | VarSort
  | TermSort
  | HoleInteriorSort
  | Sort
  -- Judgement
  | Judgement {-expr-} {-sort-}

derive instance Generic ExprLabel _
instance Show ExprLabel where show x = genericShow x
instance Eq ExprLabel where eq x = genericEq x
instance Ord ExprLabel where compare x y = genericCompare x y

instance IsExprLabel ExprLabel where
  -- var
  prettyExprF'_unsafe (Zero /\ _) = "Z"
  prettyExprF'_unsafe (Suc /\ [v]) = parens $ "S" <+> v
  -- term
  prettyExprF'_unsafe (Lam /\ [v, b]) = parens $ "lam" <+> v <+> "=>" <+> b
  prettyExprF'_unsafe (App /\ [f, a]) = parens $ f <+> a
  prettyExprF'_unsafe (Ref /\ [v]) = "#" <> v
  -- hole
  prettyExprF'_unsafe (Hole /\ [hi]) = "Hole(" <> hi <> ")"
  -- hole interior
  prettyExprF'_unsafe (HoleInterior /\ []) = "?"
  -- sort
  prettyExprF'_unsafe (VarSort /\ []) = "Var"
  prettyExprF'_unsafe (TermSort /\ []) = "Term"
  prettyExprF'_unsafe (HoleInteriorSort /\ []) = "HoleInterior"
  prettyExprF'_unsafe (Sort /\ []) = "Sort"
  -- judgement
  prettyExprF'_unsafe (Judgement /\ []) = "Judgement"

  -- var
  expectedKidsCount Zero = 0
  expectedKidsCount Suc = 1
  -- term
  expectedKidsCount Lam = 2
  expectedKidsCount App = 2
  expectedKidsCount Ref = 1
  -- hole
  expectedKidsCount Hole = 1
  -- hole interior
  expectedKidsCount HoleInterior = 0
  -- sort
  expectedKidsCount VarSort = 0
  expectedKidsCount TermSort = 0
  expectedKidsCount HoleInteriorSort = 0
  expectedKidsCount Sort = 0
  -- judgement
  expectedKidsCount Judgement = 0

--------------------------------------------------------------------------------
-- Expr (and friends)
--------------------------------------------------------------------------------

type Expr = Expr.Expr ExprLabel
type Zipper = Expr.Zipper ExprLabel
type Tooth = Expr.Tooth ExprLabel

-- var
zeroE = Zero % []
sucE v = Suc % [v]
varE n = if n == 0 then zeroE else sucE (varE (n - 1))
-- term
lamE x b = Lam % [x, b]
appE f a = App % [f, a]
refE v = Ref % [v]
-- hole
holeE interior = Hole % [interior]
-- hole interior
holeInteriorE = HoleInterior % []
-- sort
varSortE = VarSort % []
termSortE = TermSort % []
holeInteriorSortE = HoleInteriorSort % []
sortE = Sort % []
-- judgement
judgementE e s = Judgement % [e, s]

--------------------------------------------------------------------------------
-- MetaExpr
--------------------------------------------------------------------------------

type MetaExpr = Expr.MetaExpr ExprLabel

-- var
zeroME = pure Zero % []
sucME v = pure Suc % [v]
varME :: forall l. Int -> MetaExpr
varME = assertInput (positif "varME") \n ->
  if n == 0 then zeroME else sucME (varME (n - 1))
-- term
lamME x b = pure Lam % [x, b]
appME f a = pure App % [f, a]
refME v = pure Ref % [v]
-- hole
holeME interior = pure Hole % [interior]
-- hole interior
holeInteriorME = pure HoleInterior % []
-- sort
varSortME = pure VarSort % []
termSortME = pure TermSort % []
holeInteriorSortME = pure HoleInteriorSort % []
sortME = pure Sort % []
-- judgement
judgementME e s = pure Judgement % [e, s]
infix 3 judgementME as |-

--------------------------------------------------------------------------------
-- Tooth
--------------------------------------------------------------------------------

-- suc_pred = Suc %< ZipList.Path {left: mempty, right: mempty}

-- lam_bod = Lam %< ZipList.Path {left: pure (holeExpr ), right: mempty}
-- app_apl = App %< ZipList.Path {left: pure holeExpr, right: mempty}
-- app_arg = App %< ZipList.Path {left: mempty, right: pure holeExpr}

-- hole_interior = Hole %< ZipList.Path {left: mempty, right: mempty}

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

-- | Naming convention: <title>_<output sort>
data RuleLabel
  = Zero_Var
  | Suc_Var
  | Lam_Term
  | App_Term 
  | Ref_Term
  | Hole_Any
  | HoleInterior_HoleInterior

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
instance IsRuleLabel RuleLabel

--------------------------------------------------------------------------------
-- Language
--------------------------------------------------------------------------------

type Language = Grammar.Language ExprLabel RuleLabel
type Rule = Grammar.Rule ExprLabel

language :: Language
language = TotalMap.makeTotalMap case _ of

  Zero_Var -> makeRule [] \[] ->
    []
    /\ --------
    pure Zero % []

  Suc_Var -> makeRule ["var"] \[var] ->
    [ var |- varSortME ]
    /\ --------
    ( sucME var |- varSortME )

  Lam_Term -> makeRule ["var", "bod"] \[var, bod] ->
    [ var |- varSortME
    , bod |- termSortME ]
    /\ --------
    ( lamME var bod )

  App_Term -> makeRule ["apl", "arg"] \[apl, arg] ->
    [ apl |- termSortME
    , arg |- termSortME ]
    /\ --------
    ( lamME apl arg |- termSortME )

  Ref_Term -> makeRule ["var"] \[var] ->
    [ var |- varSortME ]
    /\ --------
    ( refME var |- termSortME )

  Hole_Any -> makeRule ["sort", "interior"] \[sort, interior] ->
    [ interior |- holeInteriorSortME
    , sort |- sortME ] 
    /\ --------
    ( holeME interior |- sort )

  HoleInterior_HoleInterior -> makeRule [] \[] ->
    [] 
    /\ --------
    ( holeInteriorME |- holeInteriorSortME )

--------------------------------------------------------------------------------
-- DerivExpr (and friends)
--------------------------------------------------------------------------------

type DerivExpr = Grammar.DerivExpr ExprLabel RuleLabel
type DerivPath dir = Grammar.DerivPath dir ExprLabel RuleLabel
type DerivZipper = Grammar.DerivZipper ExprLabel RuleLabel
type DerivZipper' = Grammar.DerivZipper' ExprLabel RuleLabel

-- var
zeroDE = Grammar.DerivLabel Zero_Var zeroME % []
sucDE var = Grammar.DerivLabel Suc_Var (sucME (Grammar.fromDerivExpr var)) % [var]
-- term
refDE var = Grammar.DerivLabel Ref_Term (refME (Grammar.fromDerivExpr var)) % [var]
lamDE var bod = Grammar.DerivLabel Lam_Term (lamME (Grammar.fromDerivExpr var) (Grammar.fromDerivExpr bod)) % [var, bod]
appDE apl arg = Grammar.DerivLabel App_Term (appME (Grammar.fromDerivExpr apl) (Grammar.fromDerivExpr arg)) % [apl, arg]
-- hole
holeDE interior sort = Grammar.DerivLabel Hole_Any (holeME (Grammar.fromDerivExpr interior)) % [interior, sort]
-- hole interior
holeInteriorDE = Grammar.DerivLabel HoleInterior_HoleInterior holeInteriorME % []

--------------------------------------------------------------------------------
-- LanguageChanges
--------------------------------------------------------------------------------

type LanguageChanges = Grammar.LanguageChanges ExprLabel RuleLabel
type ChangeRule = Grammar.ChangeRule ExprLabel

-- !TODO special cases
languageChanges :: LanguageChanges
languageChanges = defaultLanguageChanges language # TotalMap.mapWithKey case _ of
  Zero_Var -> identity
  Suc_Var -> identity
  Lam_Term -> identity
  App_Term -> identity
  Ref_Term -> identity
  Hole_Any -> identity
  HoleInterior_HoleInterior -> identity

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- !TODO OLD

-- --  
-- -- Edit
-- --

-- getEdits :: Zipper -> Array (Edit ExprLabel)
-- getEdits zipper = do
--   let _ = Expr.assertWellformedExpr "getEdits" (unwrap zipper).expr
--   case (unwrap zipper).expr of
--     Expr.Expr Zero [] ->
--       [ deleteEdit VarSort
--       , enS
--       ]
--     Expr.Expr Suc [_p] ->
--       [ deleteEdit VarSort
--       , enS
--       ]
--     Expr.Expr Lam [_v, _b] ->
--       [ deleteEdit TermSort
--       , enLam
--       , enArg
--       ]
--     Expr.Expr App [_f, _a] ->
--       [ deleteEdit TermSort
--       , enLam
--       , enApl
--       , enArg
--       ]
--     Expr.Expr Ref [_v] ->
--       [ deleteEdit TermSort
--       , enLam
--       , enApl
--       , enArg
--       ]
--     Expr.Expr (Hole sort) [_] ->
--       case sort of
--         VarSort -> 
--           [ enS
--           ]
--         TermSort ->
--           [ enLam
--           , enApl
--           , enArg
--           ]
--     Expr.Expr (HoleInterior sort) [] ->
--       case sort of
--         VarSort -> 
--           [ inZ
--           ]
--         TermSort ->
--           [ inRef 
--           ]
--     _ -> []
--   where
--   deleteEdit sort = {ExprLabel: "delete", preview: "?", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {expr = holeExpr ?sort} zipper}
--   enS = {ExprLabel: "S", preview: "S {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath var_p (unwrap zipper).path} zipper}
--   enLam = {ExprLabel: "lam", preview: "lam _ ↦ {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath lam_bod (unwrap zipper).path} zipper}
--   enApl = {ExprLabel: "apl", preview: "_ {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath app_apl (unwrap zipper).path} zipper}
--   enArg = {ExprLabel: "arg", preview: "{{ }} _", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath app_arg (unwrap zipper).path} zipper}

--   inZ = {ExprLabel: "Z", preview: "Z", action: SetZipperAction $ defer \_ -> pure $ Expr.Zipper
--     { path: case (unwrap zipper).path of
--         Expr.Path (Tooth (Hole _VarSort) _ : path') -> Expr.Path path'
--         _ -> unsafeCrashWith "bad inZ"
--     , expr: varExpr 0
--     }}
--   inRef = {ExprLabel: "ref", preview: "{{#_}}", action: SetZipperAction $ defer \_ -> pure $ Expr.Zipper
--     { path: case (unwrap zipper).path of
--         Expr.Path (Tooth (Hole _TermSort) _ : path') -> Expr.Path path'
--         _ -> unsafeCrashWith $ "bad inRef: " <> pretty zipper
--     , expr: refExpr (holeExpr ?VarSort)
--     }}

-- !TODO these are expressions, but i really want DerivExprs 
-- --
-- -- examples
-- --

-- ex_expr1 :: Expr
-- ex_expr1 = lamExpr x (refExpr x)
--   where x = varExpr 0

-- ex_expr2 :: Expr
-- ex_expr2 = lamExpr x holeExpr
--   where x = varExpr 0
