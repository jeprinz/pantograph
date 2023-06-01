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
import Language.Pantograph.Generic.Rendering.Base (ArrangeDerivTermSubs, incremementIndentationLevel)
import Language.Pantograph.Generic.Rendering.Base as Rendering
import Language.Pantograph.Generic.Rendering.Elements as Rendering
import Text.Pretty (pretty)

type Query = Rendering.Query
type Output = Rendering.Output PreSortLabel RuleLabel

arrangeDerivTermSubs :: ArrangeDerivTermSubs PreSortLabel RuleLabel
arrangeDerivTermSubs {renCtx, rule, sort, kids} = do
  assert (Expr.wellformedExprF "ULC arrangeDerivTermSubs" pretty (Grammar.DerivLabel rule sort /\ kids)) \_ -> case rule /\ sort /\ kids of
    -- var
    Zero /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ _ -> 
      [pure [nameElem str]]
    Suc /\ (Expr.Meta (Right Grammar.NameSortLabel) % [_gamma, Expr.Meta (Right (Grammar.StringSortLabel str)) % []]) /\ _ -> 
      [pure [nameElem str]]
    -- term
    Ref /\ _ /\ _ -> 
      [pure [refElem], Left (renCtx /\ 0)]
    Lam /\ _ /\ _ -> 
      let renCtx' = incremementIndentationLevel renCtx in
      [pure [Rendering.lparenElem, lambdaElem], Left (renCtx /\ 0), pure [mapstoElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
    App /\ _ /\ _ ->
      let renCtx' = incremementIndentationLevel renCtx in
      [pure [Rendering.lparenElem], Left (renCtx' /\ 0), pure [Rendering.spaceElem], Left (renCtx' /\ 1), pure [Rendering.rparenElem]]
    -- format
    FormatRule Newline /\ _ /\ _ ->
      let renCtx' = incremementIndentationLevel renCtx in
      Array.concat
        [ if renCtx.isInlined then [] else
          [pure $ [Rendering.spaceElem] <> [Rendering.newlineElem] <> Array.replicate renCtx.indentationLevel Rendering.indentElem]
        , [Left (renCtx' /\ 0)] ]
    FormatRule Comment /\ _ /\ _ ->
      [ pure [Rendering.commentBeginElem]
      , Left (renCtx /\ 0)
      , pure [Rendering.commentEndElem]
      , Left (renCtx /\ 1)]
    -- hole 
    TermHole /\ _ /\ _ -> bug "[ULC.Grammar.arrangeDerivTermSubs] hole should be handled generically"

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

editsAtHoleInterior = Edit.defaultEditsAtHoleInterior
editsAtCursor = Edit.defaultEditsAtCursor

