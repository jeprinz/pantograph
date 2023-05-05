module Language.Pantograph.ULC.Rendering where

import Data.Tuple
import Data.Tuple.Nested
import Language.Pantograph.ULC.Grammar
import Prelude

import Data.Gram (Gram(..), assertWellformedExpr, assertWellformedNodeG, unsafeCrashWithUnexpectedMalformedExpr)
import Data.Maybe (Maybe, fromMaybe')
import Halogen.HTML as HH
import Halogen.Utilities (classNames)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (pretty)

renderExprKids :: forall w i. Expr -> Array (HH.HTML w i) -> Array String /\ Array (HH.HTML w i)
renderExprKids expr kidElems = do
  let _ = assertWellformedExpr expr
  fromMaybe' (\_ -> unsafeCrashWithUnexpectedMalformedExpr expr "renderExprKids") 
  $ (renderExprKids' expr kidElems)

renderExprKids' :: forall w i. Expr -> Array (HH.HTML w i) -> Maybe (Array String /\ Array (HH.HTML w i))
-- var
renderExprKids' (Gram (Z /\ _)) [] = pure $ ["var", "zero"] /\ [zeroVarElem]
renderExprKids' (Gram (S /\ _)) [p] = pure $ ["var", "suc"] /\ [sucVarElem, HH.div [classNames ["subnode", "s-pred"]] [p]]
-- term
renderExprKids' (Gram (Lam /\ _)) [v, b] = pure $ ["term", "lam"] /\ [lparenElem, lambdaElem, HH.div [classNames ["subnode", "lam-bind"]] [v], mapstoElem, HH.div [classNames ["subnode", "lam-bod"]] [b], rparenElem]
renderExprKids' (Gram (App /\ _)) [f, a] = pure $ ["term", "app"] /\ [lparenElem, HH.div [classNames ["subnode", "app-apl"]] [f], spaceElem, HH.div [classNames ["subnode", "app-arg"]] [a], rparenElem]
renderExprKids' (Gram (Ref /\ _)) [v] = pure $ ["term", "ref"] /\ [refElem, HH.div [classNames ["subnode", "ref-var"]] [v]]
renderExprKids' (Gram (Hole sort /\ _)) [hi] = pure $ ["hole"] /\ [HH.div [classNames ["subnode", "hole-interior"]] [hi, colonElem, HH.div [classNames ["subnode", "hole-sort"]] [HH.text $ pretty sort]]]
renderExprKids' (Gram (HoleInterior _ /\ _)) [] = pure $ ["hole-interior"] /\ [holeInteriorElem]
renderExprKids' _ _ = mempty

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