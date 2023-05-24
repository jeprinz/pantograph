module Language.Pantograph.ULC.Rendering where

import Prelude

import Bug.Assertion (assert)
import Data.Array as Array
import Data.Expr as Expr
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering (defaultEditsAtHoleyDerivZipper)
import Language.Pantograph.Generic.Rendering as Rendering
import Language.Pantograph.ULC.Grammar (DerivExpr, ExprLabel, MetaExpr, RuleLabel(..))

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

editsAtHoleyDerivZipper = defaultEditsAtHoleyDerivZipper
