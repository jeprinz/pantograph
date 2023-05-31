module Language.Pantograph.SULC.Rendering where

import Language.Pantograph.SULC.Grammar
import Prelude

import Bug (bug)
import Bug.Assertion (assert)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr ((%))
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
prerenderDerivTerm {rule, sort, kids} = do
  assert (Expr.wellformedExprF "ULC prerenderDerivTerm" pretty (Grammar.DerivLabel rule sort /\ kids)) \_ -> case rule /\ sort /\ kids of
    -- var
    Zero /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ _ -> {classNames: ["var", "zero"], subSymElems: [pure [nameElem str]]}
    Suc /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ _ -> {classNames: ["var", "suc"], subSymElems: [pure [nameElem str]]}
    -- term
    Ref /\ _ /\ _ -> {classNames: ["term", "ref"], subSymElems: [pure [refElem], Left 0]}
    Lam /\ _ /\ _ -> {classNames: ["term", "lam"], subSymElems: [pure [Rendering.lparenElem, lambdaElem], Left 0, pure [mapstoElem], Left 1, pure [Rendering.rparenElem]]}
    App /\ _ /\ _ -> {classNames: ["term", "app"], subSymElems: [pure [Rendering.lparenElem], Left 0, pure [Rendering.spaceElem], Left 1, pure [Rendering.rparenElem]]}
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

editsAtHoleyDerivZipper topSort = case _ of
  Rendering.InjectHoleyDerivZipper dz -> Edit.defaultEditsAtDerivZipper topSort dz
  Rendering.HoleInteriorHoleyDerivZipper p sort -> Edit.defaultEditsAtHoleInterior p sort
