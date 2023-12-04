module Tutorial.CurriedTutorial where

import Data.Tuple.Nested
import Prelude
import Tutorial.EditorTutorial2

import Bug (bug)
import Bug as Bug
import Data.Array as Array
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Debug as Debug
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Language.Pantograph.Specific.Currying as Currying
import Util (fromJust')

type HTML = HH.HTML Unit Unit

{-
A specific tutorial for the Currying.purs language
-}

prog :: String -> Grammar.DerivTerm Currying.PreSortLabel Currying.RuleLabel
prog str = Grammar.decodeSerializedZipper2 Currying.editorSpec.clipboardSort str

makeLesson :: String -> Array String -> Lazy HTML -> Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel)
makeLesson progString paths instructions = defer \_ ->
    let program = prog progString in
    {
        program
        , paths: map (Grammar.deserializePath program) paths
        , instructions: force instructions
    }

dummyButton :: forall w2 i3. String -> HH.HTML w2 i3
dummyButton s = HH.button [classNames ["TutorialControlButton TutorialControlButtonDummy"]] [HH.text s]

dummyDataTyHole :: forall w2 i3. String -> HH.HTML w2 i3
dummyDataTyHole str_dataty =
  HH.code_
    [HH.div [classNames ["node"]]
      [ HH.div [classNames ["subnode", "punctuation", "lbrace"]] [HH.text "{"]
      , HH.div [classNames ["node", "holeInterior"]] [HH.div [classNames ["node", "holeInterior-inner"]] [HH.div [classNames ["subnode", "punctuation", "square"]] [HH.text "▪"]]]
      , HH.div [classNames ["subnode", "punctuation", "colon"]] [HH.text ":"]
      , HH.div [classNames ["node", "typesubscript"]]
          [ HH.div [classNames ["node"]] [HH.span [classNames ["datatype"]] [HH.text str_dataty]] ]
      , HH.div [classNames ["subnode", "punctuation", "rbrace"]] [HH.text "}"]
      ]]

italic = HH.i_ <<< pure <<< text
bold = HH.b [] <<< pure <<< text
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

selectionsWarning = """
♯italic⟦Note⟧. 
♯bold⟦Be very precise when making selections!⟧
If you get stuck, you can ♯button⟦Reset⟧ or undo (ctrl+z).

♯br♯br
"""

lessons :: Array (Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel))
lessons = -- Array.reverse 
  [
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Introduction"]
        , HH.p_ $ parseMd """
          This tutorial is an introduction to a new ♯bold⟦structure editor⟧: ♯Pantograph.
          Although the code looks like text, it actually isn't.

          ♯task⟦Hover over the program to reveal its structure.⟧
          ♯task⟦Click to place the cursor.⟧

          ♯Pantograph helps you ♯bold⟦write⟧ and ♯bold⟦modify⟧ your program while ♯italic⟦never breaking its structure⟧.
          In other words, Pantograph prevents ♯bold⟦syntax errors⟧ and ♯bold⟦type errors⟧.

          ♯br♯br

          During this tutorial, remember these tips:
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ Read the ♯bold⟦instructions⟧ fully. """
            , HH.li_ $ parseMd """ Press "Ctrl+Z" to ♯bold⟦undo⟧ the last edit. """
            , HH.li_ $ parseMd """ Press "Escape" to ♯bold⟦escape⟧ a menu. """
            , HH.li_ $ parseMd """Click ♯button⟦Reset⟧ if you get ♯bold⟦stuck⟧.""" ]
        , HH.p_ $ parseMd """
          Click ♯button⟦Next Lesson⟧ to move on.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Filling a Hole"]
        , HH.p_ $ parseMd """
          A ♯bold⟦hole⟧ is a placeholder term, displayed as ♯dataTyHole⟦T⟧, where ♯code⟦T⟧ is the type of the hole.
          ♯br
          To ♯bold⟦fill in a hole⟧,
          move the cursor to the ♯code⟦▪⟧ inside the hole,
          then write the value to fill,
          and finally press "Enter".
          
          ♯task⟦Fill each hole.⟧

          As you type, ♯Pantograph will list all the possible terms you could fill the hole with.
          Observe that ♯Pantograph will ♯italic⟦only⟧ let you fill a hole with something of the ♯italic⟦correct type⟧.
          Values of the ♯italic⟦wrong type⟧ won't appear in the list.
          """
        ]
  ,
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Basic Editing"]
        , HH.p_ $ parseMd """
          In this lesson, we will edit ♯code⟦fib⟧ to take its base case as an additional ♯italic⟦input⟧.
          
          ♯br♯br

          To ♯bold⟦delete a term⟧, press "Delete". This replaces the term with a hole.
          ♯task⟦Delete the base case value.⟧

          ♯br

          To ♯bold⟦add an argument⟧ to a function, write "fun" at the function body and then press "Enter".
          ♯task⟦Add an argument to fib.⟧
          When you do this, ♯Pantograph automatically updates ♯code⟦fib⟧'s type and each ♯code⟦fib⟧ call.

          ♯br♯br

          To ♯bold⟦name or rename a variable⟧, write the new name at the variable and then press "Enter".
          ♯task⟦Name the new argument "m".⟧
          ♯task⟦Fill in all holes in fib with m.⟧
          """
        ]
  ,
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Basic Editing"]
        , HH.p_ $ parseMd """
          So far, you have learned how to:
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
            , HH.li_ $ parseMd """ ♯bold⟦Delete a term⟧: press "Delete" """
            , HH.li_ $ parseMd """ ♯bold⟦Add a function argument⟧: write "fun" at the function body """
            , HH.li_ $ parseMd """ ♯bold⟦Name a variable⟧: write the variable's new name at the variable """
            ]
        , HH.p_ $ parseMd """
          In addition to adding a function argument with "fun", you can also insert other constructs.
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ ♯bold⟦Add a new definition⟧: write "let" """
            ]
        , HH.p_ $ parseMd """
          Using your knowledge so far, write the function ♯code⟦plus⟧ from scratch:
          ♯task⟦Add a new defintion called "plus"⟧
          ♯task⟦At the body of plus (after the "="), add two function arguments named "x" and "y".⟧
          ♯task⟦Fill the hole with "+". Pantograph will automatically create holes for its two arguments.⟧
          ♯task⟦Fill the two arguments of + with x and y⟧
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Selections"]
        , HH.p_ $ parseMd """
          So far you've been editing with only the ♯bold⟦cursor⟧.
          
          ♯br♯br

          In ♯Pantograph you can make a ♯bold⟦selection⟧ by dragging over a region.
          ♯task⟦Select triple's function argument x by dragging from the "fun x" to the "(3 * x)"⟧
          ♯task⟦Select max's function arguments x and y by dragging from the "fun x" to the "if (x > y)"⟧
          ♯task⟦Select the definition of triple by dragging from the "let triple" to the "let square"⟧
          ♯task⟦Select the definitions of triple and square by dragging from the "let triple" to the "let max"⟧

          ♯br

          Try making some other selections to get a feel for it.
          Observe that you can only make selections between ♯italic⟦terms⟧.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Rearranging Definitions"]
        , HH.p_ $ parseMd $ selectionsWarning <> """
          Use ♯bold⟦selection⟧ and ♯bold⟦cut + paste⟧ to rearrange the definitions of ♯code⟦triple⟧ and ♯code⟦square⟧.
          ♯task⟦Select the definition of triple by dragging from the "let triple" to the "let square".⟧
          ♯task⟦Cut (ctrl+x) the definition of triple.⟧
          ♯italic⟦Observe⟧ that since ♯code⟦triple⟧ is now undefined, the ♯code⟦triple⟧ call in ♯code⟦(max (triple y) (square y))⟧ is ♯greyError⟦underlined⟧.
          ♯task⟦Move the cursor between the definitions of square and max (right before "let max").⟧
          ♯task⟦Paste (ctrl+v) the definition of triple.⟧
          ♯italic⟦Observe⟧ that since ♯code⟦triple⟧ is now defined again, the ♯code⟦triple⟧ call is not underlined anymore.
          
          ♯br♯br
          
          ♯task⟦Rearrange the definitions of square and max.⟧
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Rearranging Function Calls"]
        , HH.p_ $ parseMd $ selectionsWarning <> """
          In the ♯bold⟦last line⟧ of the program,
          use ♯bold⟦selection⟧ and ♯bold⟦cut + paste⟧ to rearrange the ♯code⟦triple⟧ and ♯code⟦square⟧ function calls.
          ♯task⟦Cut the triple function call: drag from the "(" before "triple" to "y", then cut.⟧
          ♯task⟦Paste onto the "y" right after "square".⟧
          ♯task⟦Cut the square function call: drag from the "(" right before "triple" to the "(" right before "square", then cut.⟧
          ♯task⟦Paste on the "y" right after "max".⟧

          ♯br

          The last line of the program should now be ♯code⟦(max (square y) (triple y))⟧.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Rearranging Function Arguments"]
        , HH.p_ $ parseMd $ selectionsWarning <> """
          The gray circles between function call arguments are "handles" that let you select sequences of arguments.
          ♯task⟦Select the first argument of max: drag from the gray circle right before "(triple y)" to the gray circle right after it.⟧
          ♯task⟦Select both arguments of max: drag from the gray circle right before "(triple y)" to the ")" right after "(square y)"⟧

          In the ♯bold⟦last line⟧ of the program,
          use a ♯bold⟦selection⟧ and ♯bold⟦cut + paste⟧ to rearrange the ♯code⟦max⟧ call's arguments.

          ♯task⟦Select the second argument of max, then cut.⟧
          ♯task⟦Paste onto "max".⟧

          TODO: handle boundaries and grayed args
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Type Boundaries"]
        , HH.p_ $ parseMd """
          ♯Pantograph prevents you from making type errors.
          But you can still make edits that that introduce a temporary ♯bold⟦type boundary⟧, 
          where the ♯italic⟦actual⟧ type of the term in the boundary doesn't match its ♯italic⟦expected⟧ type.

          Edit ♯code⟦f⟧ to take a ♯code⟦Bool⟧ as input instead of an ♯code⟦Int⟧.

          ♯task⟦Delete the Int in "fun x : Int => ...".⟧
          ♯task⟦Write "Bool" at "?0".⟧

          ♯italic⟦Observe⟧ that the ♯code⟦y⟧ in ♯code⟦(f y)⟧ now has a ♯bold⟦type boundary⟧ around it.
          ♯bold⟦Hover⟧ over it to get more info.

          ♯br

          Edit ♯code⟦y⟧ to have type ♯code⟦Bool⟧.

          ♯task⟦Delete the Int in "let y : Int = ...".⟧
          ♯task⟦Write "Bool" at "?0".⟧

          We fixed ♯code⟦(f y)⟧, but now there is a type boundary around the value of ♯code⟦y⟧.
          Fix it.

          ♯task⟦Delete the ⦃10⦄ in "let y : Bool = ⦃10⦄".⟧
          ♯task⟦Fill the hole with a boolean value.⟧
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ErrorBoundary"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ErrorBoundary"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Deleting a Type Boundary"]
        , HH.p_ $ parseMd """
          The type boundary around ♯code⟦10⟧ indicates that the expected type is ♯code⟦Bool⟧ but the actual type is ♯code⟦Int⟧.
          This time, instead of changing ♯code⟦10⟧ to something of type ♯code⟦Bool⟧, edit the program so that ♯code⟦Int⟧ ♯italic⟦is⟧ the expected type of ♯code⟦10⟧.

          ♯br♯br

          To accomplish this, delete the type boundary itself.
          ♯task⟦Select the type boundary (drag from the "⟦" to the "10").⟧
          ♯task⟦Delete the type boundary.⟧
          ♯italic⟦Observe⟧ that ♯Pantograph automatically changed the type of ♯code⟦y⟧ to ♯code⟦Int⟧ in order to match ♯code⟦10⟧, 
          and the type boundary around ♯code⟦y⟧ in ♯code⟦(f y)⟧ was resolved since ♯code⟦y⟧ now has the same type that ♯code⟦f⟧ expects for its argument.
          
          ♯br♯br
          
          When you delete a type boundary, ♯Pantograph updates the program around the type boundary to respect the actual type of the term.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Checkpoint"]
        , HH.p_ $ parseMd """
          You now know the basics of editing with ♯Pantograph!
          Now, take a moment to experiment with this novel style of editing.
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
            , HH.li_ $ parseMd """ ♯bold⟦Delete a term or selection⟧: press "Delete" """
            , HH.li_ $ parseMd """ ♯bold⟦Add a function argument⟧: write "fun" at the function body """
            , HH.li_ $ parseMd """ ♯bold⟦Name a variable⟧: write the variable's new name at the variable """
            , HH.li_ $ parseMd """ ♯bold⟦Add a new definition⟧: write "let" """
            ]
        , HH.p_ $ parseMd """        
          The next lessons are simple exercises to practice what you've learned.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["sum"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ListRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["l"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ListRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"NilRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Exercise: sum"]
        , HH.p_ $ parseMd """
          The function ♯code⟦sum⟧ should compute the sum of a list of integers.
          Write the implementation of ♯code⟦sum⟧.
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
            , HH.li_ $ parseMd """ ♯bold⟦Delete a term or selection⟧: press "Delete" """
            , HH.li_ $ parseMd """ ♯bold⟦Add a function argument⟧: write "fun" at the function body """
            , HH.li_ $ parseMd """ ♯bold⟦Name a variable⟧: write the variable's new name at the variable """
            , HH.li_ $ parseMd """ ♯bold⟦Add a new definition⟧: write "let" """
            , HH.li_ $ parseMd """ ♯bold⟦Built-in functions⟧: """
            , HH.ul_
                [ HH.li_ $ parseMd """ ♯code⟦match⟧: pattern-match on the ♯code⟦nil⟧ and ♯code⟦cons⟧ case of a list """
                , HH.li_ $ parseMd """ ♯code⟦+⟧: add two integers """
                ]
            ]
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["map"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ListRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["l"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ListRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"ConsRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"NilRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Exercise: map"]
        , HH.p_ $ parseMd """
          The function ♯code⟦map⟧ should apply a function to each element of a list.
          Write the implementation of ♯code⟦map⟧.
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
            , HH.li_ $ parseMd """ ♯bold⟦Delete a term or selection⟧: press "Delete" """
            , HH.li_ $ parseMd """ ♯bold⟦Add a function argument⟧: write "fun" at the function body """
            , HH.li_ $ parseMd """ ♯bold⟦Name a variable⟧: write the variable's new name at the variable """
            , HH.li_ $ parseMd """ ♯bold⟦Add a new definition⟧: write "let" """
            , HH.li_ $ parseMd """ ♯bold⟦Built-in functions⟧: """
            , HH.ul_
                [ HH.li_ $ parseMd """ ♯code⟦match⟧: pattern-match on the ♯code⟦nil⟧ and ♯code⟦cons⟧ case of a list """
                ]
            ]
        ]
  ]