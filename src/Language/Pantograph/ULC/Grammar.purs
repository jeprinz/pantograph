module Language.Pantograph.ULC.Grammar where

import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Expr (class ExprLabel, Meta(..), Tooth(..), expectedKidsCount)
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
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering (Action(..), Edit)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, parens, pretty, (<+>))
import Utility (assert)

data Label
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

derive instance Generic Label _
instance Show Label where show x = genericShow x
instance Eq Label where eq x = genericEq x
instance Ord Label where compare x y = genericCompare x y

instance ExprLabel Label where
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

type Expr = Expr.Expr Label
type MetaExpr = Expr.MetaExpr Label
type Zipper = Expr.Zipper Label
type Tooth = Expr.Tooth Label

-- var
zeroExpr = Expr.Expr Zero []
sucExpr v = Expr.Expr Suc [v]
varExpr n = if n == 0 then zeroExpr else sucExpr (varExpr (n - 1))
-- term
lamExpr x b = Expr.Expr Lam [x, b]
appExpr f a = Expr.Expr App [f, a]
refExpr v = Expr.Expr Ref [v]
-- hole
holeExpr = Expr.Expr Hole []
-- hole interior
holeInteriorExpr = Expr.Expr HoleInterior []
-- sort
varSortExpr = Expr.Expr VarSort []
termSortExpr = Expr.Expr TermSort []
holeInteriorSort = Expr.Expr HoleInteriorSort []

--
-- Rule
--

data RuleName
  = ZeroVar 
  | SucVar
  | LamTerm
  | AppTerm 
  | RefTerm
  | HoleAny
  | HoleInteriorAny

type Rule = Grammar.Rule Label

-- !TODO
-- rules :: Array Rule
-- rules = 
--   [ Grammar.Rule 
--       (Set.fromFoldable [])
--       [Expr.Expr (Meta (Right ?a)) ?a]
--       ?a
--   ]

--
-- Tooth
--

var_p = Tooth Suc (ZipList.Path {left: mempty, right: mempty})

lam_bod = Tooth Lam (ZipList.Path {left: pure (holeExpr ), right: mempty})
app_apl = Tooth App (ZipList.Path {left: pure holeExpr, right: mempty})
app_arg = Tooth App (ZipList.Path {left: mempty, right: pure holeExpr})

hole_interior sort = Hole /\ ZipList.Path {left: mempty, right: mempty}

-- --  
-- -- Edit
-- --

-- getEdits :: Zipper -> Array (Edit Label)
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
--   deleteEdit sort = {label: "delete", preview: "?", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {expr = holeExpr ?sort} zipper}
--   enS = {label: "S", preview: "S {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath var_p (unwrap zipper).path} zipper}
--   enLam = {label: "lam", preview: "lam _ ↦ {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath lam_bod (unwrap zipper).path} zipper}
--   enApl = {label: "apl", preview: "_ {{ }}", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath app_apl (unwrap zipper).path} zipper}
--   enArg = {label: "arg", preview: "{{ }} _", action: SetZipperAction $ defer \_ -> pure $ over Expr.Zipper _ {path = Expr.stepPath app_arg (unwrap zipper).path} zipper}

--   inZ = {label: "Z", preview: "Z", action: SetZipperAction $ defer \_ -> pure $ Expr.Zipper
--     { path: case (unwrap zipper).path of
--         Expr.Path (Tooth (Hole _VarSort) _ : path') -> Expr.Path path'
--         _ -> unsafeCrashWith "bad inZ"
--     , expr: varExpr 0
--     }}
--   inRef = {label: "ref", preview: "{{#_}}", action: SetZipperAction $ defer \_ -> pure $ Expr.Zipper
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
