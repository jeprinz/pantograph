module Language.Pantograph.Generic.Rendering.Elements where


import Halogen.HTML as HH
import Halogen.Utilities (classNames)

makePuncElem :: forall w i. String -> String -> HH.HTML w i
makePuncElem className symbol = HH.div [classNames ["subnode", "punctuation", className]] [HH.text symbol]

spaceElem = makePuncElem "space" " "
lparenElem = makePuncElem "lparen" "("
-- lparenElem = makePuncElem "lparen" "❰"
rparenElem = makePuncElem "rparen" ")"
-- rparenElem = makePuncElem "rparen" "❱"
lbraceElem = makePuncElem "lbrace" "{"
rbraceElem = makePuncElem "rbrace" "}"
lbracketElem = makePuncElem "lbracket" "["
rbracketElem = makePuncElem "rbracket" "]"
colonElem = makePuncElem "colon" ":"
commaElem = makePuncElem "comma" ","
turnstileElem = makePuncElem "turnstile" "⊢"
interrogativeElem = makePuncElem "interrogative" "?"
-- squareElem = makePuncElem "square" "☐"
squareElem = makePuncElem "square" "▪"
upArrowElem = makePuncElem "upArrow" "↑"
downArrowElem = makePuncElem "downArrow" "↓"
newlineElem = HH.br_
fillRightSpace = HH.div [classNames ["fill-right-space"]] []
indentElem = makePuncElem "indent" "  "

commentBeginElem = makePuncElem "commentBegin" " /* "
commentEndElem = makePuncElem "commentEnd" " */ "

ibeamElem :: forall w i. HH.HTML w i
ibeamElem = makePuncElem "ibeam" "⌶"

placeholderCursorNodeElem :: forall w i. HH.HTML w i
placeholderCursorNodeElem =
  HH.div [classNames ["node", "placeholderCursor"]]
    [ HH.div [classNames ["subnode", "placeholderCursor-inner"]]
        -- [ibeamElem]
        [spaceElem]
    ]
