module Language.Pantograph.Generic.Rendering.Elements where


import Halogen.HTML as HH
import Halogen.Utilities (classNames)

makePuncElem :: forall w i. String -> String -> HH.HTML w i
makePuncElem className symbol = HH.div [classNames ["subnode", "punctuation", className]] [HH.text symbol]

spaceElem :: forall w i. HH.HTML w i
spaceElem = makePuncElem "space" " "
lparenElem :: forall w i. HH.HTML w i
lparenElem = makePuncElem "lparen" "("
rparenElem :: forall w i. HH.HTML w i
rparenElem = makePuncElem "rparen" ")"
colonElem :: forall w i. HH.HTML w i
colonElem = makePuncElem "colon" ":"
turnstileElem :: forall w i. HH.HTML w i
turnstileElem = makePuncElem "turnstile" "⊢"
interrogativeElem :: forall w i. HH.HTML w i
interrogativeElem = makePuncElem "interrogative" "?"

ibeamElem :: forall w i. HH.HTML w i
ibeamElem = makePuncElem "ibeam" "⌶"

placeholderCursorNodeElem :: forall w i. HH.HTML w i
placeholderCursorNodeElem =
  HH.div [classNames ["node", "placeholder-cursor"]]
    [ HH.div [classNames ["subnode", "inner"]]
        -- [ibeamElem]
        [spaceElem]
    ]