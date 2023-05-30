module Language.Pantograph.SULC.Rendering where

import Language.Pantograph.SULC.Grammar
import Prelude

import Bug (bug)
import Bug.Assertion (assert)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as HK
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.Edit as Edit
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base (DerivTermPrerenderer)
import Language.Pantograph.Generic.Rendering.Base as Rendering
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Text.Pretty (pretty)

type Query = Rendering.Query
type Output = Rendering.Output PreSortLabel RuleLabel

prerenderDerivTerm :: DerivTermPrerenderer PreSortLabel RuleLabel
prerenderDerivTerm {rule, sort, kids, kidElems} = do
  let kids_kidElems = kids `Array.zip` kidElems
  assert (Expr.wellformedExprF "ULC prerenderDerivTerm" (show <<< fst) (Grammar.DerivLabel rule sort /\ kids_kidElems)) \_ -> case rule /\ sort /\ kids_kidElems of
    -- var
    Zero /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ [] -> {classNames: ["var", "zero"], subElems: [nameElem str]}
    Suc /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ [_] -> {classNames: ["var", "suc"], subElems: [nameElem str]}
    -- term
    Ref /\ _ /\ [_ /\ varElem] -> {classNames: ["term", "ref"], subElems: [refElem, varElem]}
    Lam /\ _ /\ [_ /\ varElem, _ /\ bodElem] -> {classNames: ["term", "lam"], subElems: [Rendering.lparenElem, lambdaElem, varElem, mapstoElem, bodElem, Rendering.rparenElem]}
    App /\ _ /\ [_ /\ aplElem, _ /\ argElem] -> {classNames: ["term", "app"], subElems: [Rendering.lparenElem, aplElem, Rendering.spaceElem, argElem, Rendering.rparenElem]}
    -- hole
    TermHole /\ _ /\ _ -> bug "[ULC.Grammar.prerenderDerivTerm] hole should be handled generically"

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

editsAtHoleyDerivZipper = Rendering.defaultEditsAtHoleyDerivZipper
