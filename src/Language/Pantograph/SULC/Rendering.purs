module Language.Pantograph.SULC.Rendering where

import Language.Pantograph.SULC.Grammar
import Prelude

import Bug (bug)
import Bug.Assertion (assert)
import Data.Array as Array
import Data.Expr as Expr
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Hole (hole)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering (defaultEditsAtHoleyDerivZipper)
import Language.Pantograph.Generic.Rendering as Rendering

type Query = Rendering.Query
type Output = Rendering.Output PreSortLabel RuleLabel

renderDerivTermKids' ::
  (RuleLabel /\ Sort /\ Array DerivTerm) ->
  Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot Query Output String) Aff) -> 
  Array String /\ Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot Query Output String) Aff)
renderDerivTermKids' (r /\ sort /\ kids) kidElems = do
  let kids_kidElems = kids `Array.zip` kidElems
  assert (Expr.wellformedExprF "ULC renderDerivTermKids'" (show <<< fst) (Grammar.DerivLabel r sort /\ kids_kidElems)) \_ -> case r /\ sort /\ kids_kidElems of
    -- -- var
    -- Zero /\ _ /\ [] -> ["var", "zero"] /\ 
    --   [zeroVarElem]
    -- Suc /\ _ /\ [_ /\ predElem] -> ["var", "suc"] /\ 
    --   [sucVarElem, predElem]
    -- -- term
    -- Ref /\ _ /\ [_ /\ varElem] -> ["term", "ref"] /\ [refElem, varElem]
    -- Lam /\ _ /\ [_ /\ bodElem] -> ["term", "lam"] /\ 
    --   [Rendering.lparenElem, lambdaElem, bodElem, Rendering.rparenElem]
    -- App /\ _ /\ [_ /\ aplElem, _ /\ argElem] -> ["term", "app"] /\ 
    --   [Rendering.lparenElem, aplElem, Rendering.spaceElem, argElem, Rendering.rparenElem]
    -- -- hole
    -- Hole /\ _ /\ _ -> bug "[ULC.Grammar.renderDerivTermKids'] hole should be handled generically"
    _ -> hole "TODO"

lambdaElem = Rendering.makePuncElem "lambda" "λ"
mapstoElem = Rendering.makePuncElem "mapsto" "↦"
refElem = Rendering.makePuncElem "ref" "#"
zeroVarElem = Rendering.makePuncElem "zeroVar" "Z"
sucVarElem = Rendering.makePuncElem "sucVar" "S"

--------------------------------------------------------------------------------
-- Edit
--------------------------------------------------------------------------------

type Edit = Grammar.Edit PreSortLabel RuleLabel
type HoleyDerivZipper = Rendering.HoleyDerivZipper PreSortLabel RuleLabel

editsAtHoleyDerivZipper = defaultEditsAtHoleyDerivZipper
