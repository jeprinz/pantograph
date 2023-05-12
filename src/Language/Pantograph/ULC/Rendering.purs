module Language.Pantograph.ULC.RenderingNew where

import Data.Tuple
import Data.Tuple.Nested
import Language.Pantograph.ULC.Grammar
import Prelude
import Data.Array as Array
import Data.Expr as Expr
import Data.Maybe (Maybe, fromMaybe')
import Halogen.HTML as HH
import Halogen.Utilities (classNames)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (pretty)

-- !TODO render derivs
-- renderExprKids :: forall w i. Expr -> Array (HH.HTML w i) -> Array String /\ Array (HH.HTML w i)
-- renderExprKids (Expr.Expr l kids) kidElems = do
--   let kidExprElems = kids `Array.zip` kidElems
--   Expr.assertWellformedExprF "renderExprKids" (l /\ kidExprElems) \_ -> case l /\ kidExprElems of
--     -- var
--     Z /\ [] -> ["var", "zero"] /\ [zeroVarElem]
--     S /\ [_ /\ p] -> ["var", "suc"] /\ [sucVarElem, HH.div [classNames ["subnode", "s-pred"]] [p]]
--     -- term
--     Lam /\ [_ /\ v, _ /\ b] -> ["term", "lam"] /\ [lparenElem, lambdaElem, HH.div [classNames ["subnode", "lam-bind"]] [v], mapstoElem, HH.div [classNames ["subnode", "lam-bod"]] [b], rparenElem]
--     App /\ [_ /\ f, _ /\ a] -> ["term", "app"] /\ [lparenElem, HH.div [classNames ["subnode", "app-apl"]] [f], spaceElem, HH.div [classNames ["subnode", "app-arg"]] [a], rparenElem]
--     Ref /\ [_ /\ v] -> ["term", "ref"] /\ [refElem, HH.div [classNames ["subnode", "ref-var"]] [v]]
--     Hole sort /\ [_ /\ hi] -> ["hole"] /\ [HH.div [classNames ["subnode", "hole-interior"]] [hi, colonElem, HH.div [classNames ["subnode", "hole-sort"]] [HH.text $ pretty sort]]]
--     HoleInterior _sort /\ [] -> ["hole-interior"] /\ [holeInteriorElem]

--   -- fromMaybe' (\_ -> unsafeCrashWithUnexpectedMalformedExpr expr "renderExprKids") 
--   --   $ (renderExprKids' expr kidElems)

makePuncElem :: forall w i. String -> String -> HH.HTML w i
makePuncElem className symbol = HH.div [classNames ["subnode", "punctuation", className]] [HH.text symbol]

lambdaElem = makePuncElem "lambda" "λ"
mapstoElem = makePuncElem "mapsto" "↦"
spaceElem = makePuncElem "space" " "
refElem = makePuncElem "ref" "#"
zeroVarElem = makePuncElem "zeroVar" "Z"
sucVarElem = makePuncElem "sucVar" "S"
holeInteriorElem = makePuncElem "holeInterior" "?"
lparenElem = makePuncElem "lparen" "("
rparenElem = makePuncElem "rparen" ")"
colonElem = makePuncElem "colon" ":"
