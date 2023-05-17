module Language.Pantograph.ULC.Rendering where

import Prelude

import Bug.Assertion (assert)
import Data.Array as Array
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.Utilities (classNames)
import Hole as Hole
import Language.Pantograph.Generic.Grammar ((|-))
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering as Rendering
import Language.Pantograph.ULC.Grammar (DerivExpr, DerivZipper, ExprLabel, RuleLabel(..), MetaExpr)
import Text.Pretty (pretty)

type Query = Rendering.Query ExprLabel RuleLabel
type Output = Rendering.Output ExprLabel RuleLabel

renderDerivExprKids' ::
  -- DerivExpr -> 
  (RuleLabel /\ MetaExpr /\ Array DerivExpr) ->
  Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot Query Output String) Aff) -> 
  Array String /\ Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot Query Output String) Aff)
renderDerivExprKids' (r /\ sort /\ kids) kidElems = do
  let kids_kidElems = kids `Array.zip` kidElems
  assert (Expr.wellformedExprF "ULC renderDerivExprKids'" (show <<< fst) (Grammar.DerivLabel r sort /\ kids_kidElems)) \_ -> case r /\ sort /\ kids_kidElems of
    -- var
    Zero /\ _ /\ [] -> ["var", "zero"] /\ 
      [zeroVarElem]
    Suc /\ _ /\ [_ /\ predElem] -> ["var", "suc"] /\ 
      [sucVarElem, predElem]
    -- term
    Ref /\ _ /\ [_ /\ varElem] -> ["term", "ref"] /\ [refElem, varElem]
    Lam /\ _ /\ [_ /\ varElem, _ /\ bodElem] -> ["term", "lam"] /\ 
      [Rendering.lparenElem, lambdaElem, varElem, mapstoElem, bodElem, Rendering.rparenElem]
    App /\ _ /\ [_ /\ aplElem, _ /\ argElem] -> ["term", "app"] /\ 
      [Rendering.lparenElem, aplElem, Rendering.spaceElem, argElem, Rendering.rparenElem]
    -- -- hole
    -- Hole |- sort /\ [_ /\ hiElem] -> ["hole"] /\ 
    --   [ HH.div [classNames ["subnode", "inner"]]
    --       [ HH.div [classNames ["subnode", "hole-interior"]] [hiElem], colonElem
    --       , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
    --       ]
    --   ]
    -- -- hole interior
    -- HoleInterior |- _ /\ [] -> ["holeInterior"] /\ 
    --   [ HH.div [classNames ["subnode", "inner"]]
    --       [holeInteriorElem]
    --   ]
    
    -- !TODO design decision: sometimes I want to handle only the non-hole
    -- Derivs, like here, so how do I encode that interface? would be nice to
    -- have subtyping, which i can do with row-polymorphic variants, but will
    -- that give me what i want?


lambdaElem = Rendering.makePuncElem "lambda" "λ"
mapstoElem = Rendering.makePuncElem "mapsto" "↦"
refElem = Rendering.makePuncElem "ref" "#"
zeroVarElem = Rendering.makePuncElem "zeroVar" "Z"
sucVarElem = Rendering.makePuncElem "sucVar" "S"

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

type Edit = Grammar.Edit ExprLabel RuleLabel
type HoleyDerivZipper = Rendering.HoleyDerivZipper ExprLabel RuleLabel

-- !TODO eventualy this should not even require `DerivZipper` as an arg (??)
getEdits :: HoleyDerivZipper -> Array Edit
-- getEdits _ = Hole.hole "!TODO derive default edits from language"
getEdits _ = Grammar.defaultEdits

-- getEdits = assertInput_ (\(Expr.Zipper dz) -> wellformedExpr "getEdits" dz.expr) \(Expr.Zipper dz) -> do
--   let
--     -- digEdit sort = 
--     --   { label: "dig", preview: "?"
--     --   , action: SetDerivZipperAction $ defer \_ -> Just $ Expr.Zipper dz {expr = holeDE holeInteriorDE sort} }
    
--     enTooth label preview tooth =
--       { label, preview
--       , action: SetDerivZipperAction $ defer \_ -> Just $ Expr.Zipper dz {path = Expr.stepPath tooth dz.path} }

--     inTooth label preview path' tooth innerSort =
--       { label, preview
--       -- , action: SetDerivZipperAction $ defer \_ -> Just $ Expr.Zipper dz {path = Expr.stepPath tooth path', expr = holeDE holeInteriorDE innerSort}
--       , action: SetDerivZipperAction $ defer \_ -> Just $ Expr.Zipper dz {path = Expr.stepPath tooth path', expr = ?a}
--       }
    
--     inFill label preview path' expr =
--       { label, preview
--       , action: SetDerivZipperAction $ defer \_ -> Just $ Expr.Zipper dz {path = path', expr = expr}
--       }
    
--     enSuc = enTooth "suc" "S⌶" (Suc |- ?varSortME %< mempty)
--     enLam = enTooth "lam" "lam ? => ⌶" $ Lam |- ?termSortME %< ZipList.singletonLeft (?holeDE holeInteriorDE varSortME)
--     enApl = enTooth "apl" "⌶ ?" $ App |- termSortME %< ZipList.singletonRight (holeDE holeInteriorDE varSortME)
--     enArg = enTooth "arg" "? ⌶" $ App |- termSortME %< ZipList.singletonLeft (holeDE holeInteriorDE varSortME)
--     inZero path' = inFill "zero" "Z" path' zeroDE
--     inRef path' = inTooth "ref" "ref ⌶" path' (Ref |- termSortME %< mempty) varSortME
--   case dz of
--     -- var
--     {expr: _ |- (Expr.Meta (Right VarSort) % _) % _} -> [digEdit varSortME, enSuc]
--     -- term
--     {expr: _ |- (Expr.Meta (Right TermSort) % []) % _} -> [digEdit termSortME, enLam, enApl, enArg]
--     -- hole interior
--     {path: Expr.Path ((Hole |- (Expr.Meta (Right VarSort) % _) %< _) : ths), expr: HoleInterior |- (Expr.Meta (Right HoleInteriorSort) % _) % _} -> [inZero (Expr.Path ths)]
--     {path: Expr.Path ((Hole |- (Expr.Meta (Right TermSort) % _) %< _) : ths), expr: HoleInterior |- (Expr.Meta (Right HoleInteriorSort) % _) % _} -> [inRef (Expr.Path ths)]

