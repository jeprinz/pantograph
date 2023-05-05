module Language.Pantograph.ULC.Grammar where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Gram (class GramLabel, Gram(..), Path(..), assertWellformedExpr, expectedKidsCount, prettyExpr, prettyNodeG, prettyZipper, stepPath)
import Data.Gram as Gram
import Data.Lazy (Lazy, defer)
import Data.List as List
import Data.List.Rev (RevList)
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Language.Pantograph.Generic.Rendering (Action(..), Edit)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, parens, (<+>))
import Utility (assert)

data Sort = VarSort | TermSort

derive instance Generic Sort _
instance Show Sort where show x = genericShow x
instance Eq Sort where eq x = genericEq x
instance Ord Sort where compare x y = genericCompare x y

instance Pretty Sort where
  pretty VarSort = "Var"
  pretty TermSort = "Term"

data Label
  -- Var
  = Z
  | S {-Var-}
  -- Term
  | Lam {-Var-} {-Term-}
  | App {-Term-} {-Term-}
  | Ref {-Var-}
  -- Hole
  | Hole Sort
  -- HoleInterior
  | HoleInterior Sort

derive instance Generic Label _
instance Show Label where show x = genericShow x
instance Eq Label where eq x = genericEq x
instance Ord Label where compare x y = genericCompare x y

instance GramLabel Label where
  -- var
  prettyNodeG' (Z /\ _) = pure $ "Z"
  prettyNodeG' (S /\ [v]) = pure $ parens $ "S" <+> v
  -- term
  prettyNodeG' (Lam /\ [v, b]) = pure $ parens $ "lam" <+> v <+> "=>" <+> b
  prettyNodeG' (App /\ [f, a]) = pure $ parens $ f <+> a
  prettyNodeG' (Ref /\ [v]) = pure $ "#" <> v
  -- hole
  prettyNodeG' (Hole _ /\ [hi]) = pure $ "Hole(" <> hi <> ")"
  -- hole interior
  prettyNodeG' (HoleInterior _ /\ []) = pure $ "?"
  
  prettyNodeG' _ = mempty

  -- var
  expectedKidsCount Z = 0
  expectedKidsCount S = 1
  -- term
  expectedKidsCount Lam = 2
  expectedKidsCount App = 2
  expectedKidsCount Ref = 1
  -- hole
  expectedKidsCount (Hole _) = 1
  -- hole interior
  expectedKidsCount (HoleInterior _) = 0

type Expr = Gram.Expr Label
type MetaExpr = Gram.MetaExpr Label
type Zipper = Gram.Zipper Label
type Tooth = Gram.Tooth Label

--
-- Expr
--

-- var
varE 0 = Gram (Z /\ [])
varE n = Gram (S /\ [varE (n - 1)])
-- term
lamE x b = Gram (Lam /\ [x, b])
appE f a = Gram (App /\ [f, a])
refE v = Gram (Ref /\ [v])
-- hole
hole sort = Gram (Hole sort /\ [holeInterior sort])
-- hole interior
holeInterior sort = Gram (HoleInterior sort /\ [])

--
-- Tooth
--

var_p = S /\ ZipList.Path {left: mempty, right: mempty}

lam_bod = Lam /\ ZipList.Path {left: pure (hole VarSort), right: mempty}
app_apl = App /\ ZipList.Path {left: pure (hole TermSort), right: mempty}
app_arg = App /\ ZipList.Path {left: mempty, right: pure (hole TermSort)}

hole_interior sort = Hole sort /\ ZipList.Path {left: mempty, right: mempty}

--  
-- Edit
--

getEdits :: Zipper -> Array (Edit Label)
getEdits zipper = do
  let _ = assertWellformedExpr zipper.expr "getEdits"
  case zipper.expr of
    (Gram (Z /\ [])) ->
      [ deleteEdit VarSort
      , enS
      ]
    (Gram (S /\ [_p])) ->
      [ deleteEdit VarSort
      , enS
      ]
    (Gram (Lam /\ [_v, _b])) ->
      [ deleteEdit TermSort
      , enLam
      , enArg
      ]
    (Gram (App /\ [_f, _a])) ->
      [ deleteEdit TermSort
      , enLam
      , enApl
      , enArg
      ]
    (Gram (Ref /\ [_v])) ->
      [ deleteEdit TermSort
      , enLam
      , enApl
      , enArg
      ]
    (Gram (Hole sort /\ [_])) ->
      case sort of
        VarSort -> 
          [ enS
          ]
        TermSort ->
          [ enLam
          , enApl
          , enArg
          ]
    Gram (HoleInterior sort /\ []) ->
      case sort of
        VarSort -> 
          [ inZ
          ]
        TermSort ->
          [ inRef 
          ]
    _ -> []
  where
  deleteEdit sort = {label: "delete", preview: "?", action: SetZipperAction $ defer \_ -> pure $ zipper {expr = hole sort}}
  enS = {label: "S", preview: "S {{ }}", action: SetZipperAction $ defer \_ -> pure $ zipper {path = stepPath var_p zipper.path}}
  enLam = {label: "lam", preview: "lam _ ↦ {{ }}", action: SetZipperAction $ defer \_ -> pure $ zipper {path = stepPath lam_bod zipper.path}}
  enApl = {label: "apl", preview: "_ {{ }}", action: SetZipperAction $ defer \_ -> pure $ zipper {path = stepPath app_apl zipper.path}}
  enArg = {label: "arg", preview: "{{ }} _", action: SetZipperAction $ defer \_ -> pure $ zipper {path = stepPath app_arg zipper.path}}

  inZ = {label: "Z", preview: "Z", action: SetZipperAction $ defer \_ -> pure
    { path: case zipper.path of
        Path (Just (Gram ((Hole VarSort /\ _) /\ path'))) -> Path path'
        _ -> unsafeCrashWith "bad inZ"
    , expr: varE 0
    }}
  inRef = {label: "ref", preview: "{{#_}}", action: SetZipperAction $ defer \_ -> pure
    { path: case zipper.path of
        Path (Just (Gram ((Hole TermSort /\ _) /\ path'))) -> Path path'
        _ -> unsafeCrashWith $ "bad inRef: " <> prettyZipper zipper
    , expr: refE (hole VarSort)
    }}

--
-- examples
--

ex_expr1 :: Expr
ex_expr1 = lamE x (refE x)
  where x = varE 0

ex_expr2 :: Expr
ex_expr2 = lamE x (hole TermSort)
  where x = varE 0
