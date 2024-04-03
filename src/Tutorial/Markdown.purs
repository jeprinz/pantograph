module Tutorial.Markdown where

import Prelude

import Data.Array as Array
import Data.Lazy (defer, force)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Util (fromJust')

type HTML = HH.HTML Unit Unit

dummyButton :: forall w2 i3. String -> HH.HTML w2 i3
dummyButton s = HH.button [classNames ["TutorialControlButton TutorialControlButtonDummy"]] [HH.text s]

dummyDataTyHole :: forall w2 i3. String -> HH.HTML w2 i3
dummyDataTyHole str_dataty =
  HH.code_
    [HH.div [classNames ["node"]]
      [ HH.div [classNames ["subnode", "punctuation", "lbrace"]] [HH.text "{"]
      , HH.div [classNames ["node", "holeInterior"]] [HH.div [classNames ["node", "holeInterior-inner"]] [HH.div [classNames ["subnode", "punctuation", "square"]] [HH.text "□"]]]
      , HH.div [classNames ["subnode", "punctuation", "colon"]] [HH.text ":"]
      , HH.div [classNames ["node", "typesubscript"]]
          [ HH.div [classNames ["node"]] [HH.span [classNames ["datatype"]] [HH.text str_dataty]] ]
      , HH.div [classNames ["subnode", "punctuation", "rbrace"]] [HH.text "}"]
      ]]

italic :: forall w i. String -> HH.HTML w i
italic = HH.i_ <<< pure <<< text

bold :: forall w i. String -> HH.HTML w i
bold = HH.b [] <<< pure <<< text

text :: forall w i. String -> HH.HTML w i
text = HH.text

data MatchMd 
  = ReplaceMatchMd String HTML
  | FunctionMatchMd String (String -> HTML)

tryMatchMd :: MatchMd -> String -> Maybe (HTML /\ String)
tryMatchMd (ReplaceMatchMd label html) str = do
  str' <- String.stripPrefix (String.Pattern ("♯" <> label)) str
  pure $ html /\ str'
tryMatchMd (FunctionMatchMd label toHtml) str = do
  str' <- String.stripPrefix (String.Pattern ("♯" <> label <> "⟦")) str
  let i = String.indexOf (String.Pattern "⟧") str' # fromJust' "no closing \"⟧\""
  let {before, after} = String.splitAt i str'
  let str'' = String.drop 1 after
  pure $ toHtml before /\ str''

parseMd :: String -> Array HTML
parseMd = go [] []
  where
  go :: Array HTML -> Array Char -> String -> Array HTML
  go htmls work str = case matches # map (flip tryMatchMd str) >>> Array.catMaybes >>> Array.head of
    Nothing -> case CodeUnits.uncons str of
      Nothing -> force htmls_work
      Just {head: c, tail: str'} -> go htmls (Array.snoc work c) str'
    Just (html /\ str') -> go (force htmls_work `Array.snoc` html) [] str'
    where
    htmls_work = defer \_ -> htmls # if Array.null work then identity else (_ `Array.snoc` text (CodeUnits.fromCharArray work))

  matches :: Array MatchMd
  matches = 
    [ ReplaceMatchMd "br" HH.br_
    , ReplaceMatchMd "Pantograph" $ HH.span [classNames ["TutorialWord-Pantograph"]] [text "Pantograph"]
    , FunctionMatchMd "bold" $ bold
    , FunctionMatchMd "italic" $ italic
    , FunctionMatchMd "code" $ HH.code_ <<< pure <<< text
    , FunctionMatchMd "button" $ dummyButton
    , FunctionMatchMd "dataTyHole" $ dummyDataTyHole
    , FunctionMatchMd "greyError" $ HH.div [classNames ["Tutorial-greyError"], HP.style "display: inline-block"] <<< pure <<< HH.div [classNames ["inline", "grey", "error"], HP.style "display: inline-block"] <<< pure <<< HH.span [classNames [], HP.style "display: inline-block"] <<< pure <<< text
    , FunctionMatchMd "task" $ HH.div [classNames ["TutorialTask"]] <<< Array.cons (bold "Task:") <<< pure <<< text
    ]

