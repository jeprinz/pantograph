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
import Language.Pantograph.Generic.Rendering.Rendering as Rendering
import Unsafe.Coerce (unsafeCoerce)
import Data.Expr as Expr
import Data.List (List(..))

{-
A specific tutorial for the Currying.purs language
-}

prog :: String -> Grammar.DerivTerm Currying.PreSortLabel Currying.RuleLabel
prog str = Grammar.decodeSerializedZipper2 Currying.editorSpec.clipboardSort str

makeLesson :: String -> Array String -> Lazy (HH.HTML Unit Unit) -> Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel)
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

type MdMatch = (String -> Maybe (Int /\ (Array (HH.HTML Unit Unit) -> Array (HH.HTML Unit Unit)) /\ String))

-- parseMd' :: String -> Array (HH.HTML Unit Unit)
-- parseMd' = go [] <<< CodeUnits.toCharArray
--   where
--   go :: Array Char -> Array Char -> Array (HH.HTML Unit Unit)
--   go = ?a 


parseMd :: String -> Array (HH.HTML Unit Unit)
parseMd str = case matches # map (\m -> m str) >>> Array.catMaybes >>> Array.sortBy (\(i /\ _) (j /\ _) -> compare i j) >>> Array.head of
  Nothing -> if String.null str then [] else [text str]
  Just (_ /\ f /\ str') -> f (parseMd str')
  where
  makeEnclosedMatch str_start toHtml str_end = \str -> do
    i <- String.indexOf (String.Pattern str_start) str
    let {before: str_before, after: str_after_before_} = String.splitAt i str
    let str_after_before = String.drop (String.length str_start) str_after_before_
    j <- String.indexOf (String.Pattern str_end) str_after_before
    let {before: str_match, after: str_after_match} = String.splitAt j str_after_before
    let str' = String.drop (String.length str_end) str_after_match
    pure $ i /\ ( (Array.cons (toHtml str_match)) >>> (if String.null str_before then identity else Array.cons (text str_before)) ) /\ str'

  makeReplaceMatch str_marker html = \str -> do
    i <- String.indexOf (String.Pattern str_marker) str
    let {before, after: after_} = String.splitAt i str
    let after = String.drop (String.length str_marker) after_
    pure $ i /\ ( (Array.cons html) >>> (if String.null before then identity else Array.cons (text before)) ) /\ after

  matches :: Array MdMatch
  matches =
    [ makeReplaceMatch "#br" HH.br_
    , makeReplaceMatch "#Pantograph" $ HH.span [classNames ["TutorialWord-Pantograph"]] [text "Pantograph"]
    , makeEnclosedMatch "*" bold "*"
    , makeEnclosedMatch "~" italic "~"
    , makeEnclosedMatch "`" (HH.code_ <<< pure <<< text) "`"
    , makeEnclosedMatch "#button[" dummyButton "]"
    , makeEnclosedMatch "#dataTyHole[" dummyDataTyHole "]"
    , makeEnclosedMatch "#task[" (HH.div [classNames ["TutorialTask"]] <<< Array.cons (bold "Task:") <<< pure <<< text) "]"
    ]

lessons :: Array (Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel))
lessons = 
  Array.reverse [
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Introduction"]
        , HH.p_ $ parseMd """
          This tutorial is an introduction to a new *structure editor*: #Pantograph.
          Although the code looks like text, it actually isn't.

          #task[Hover over the program to reveal its structure.]
          #task[Click to place the cursor.]

          #Pantograph helps you *write* and *modify* your program while ~never breaking its structure~.
          In other words, Pantograph prevents *syntax errors* and *type errors*.

          #br#br

          During this tutorial, remember these tips:
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ Read the *instructions* fully. """
            , HH.li_ $ parseMd """ Press "Ctrl+Z" to undo the last edit. """
            , HH.li_ $ parseMd """ Press "Escape" to *escape* a menu. """
            , HH.li_ $ parseMd """Click #button[Reset] if you get *stuck*.""" ]
        , HH.p_ $ parseMd """
          Click #button[Next Lesson] to move on.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Filling a Hole"]
        , HH.p_ $ parseMd """
          A *hole* is a placeholder term, displayed as #dataTyHole[T], where `T` is the type of the hole.
          #br
          To *fill in a hole*,
          move the cursor to the `▪` inside the hole,
          then write the value to fill,
          and finally press "Enter".
          
          #task[Fill each hole.]

          As you type, #Pantograph will list all the possible terms you could fill the hole with.
          Observe that #Pantograph will ~only~ let you fill a hole with something of the ~correct type~.
          Values of the ~wrong type~ won't appear in the list.
          """
        ]
  ,
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Basic Editing"]
        , HH.p_ $ parseMd """
          In this lesson, we will edit `fib` to take its base case as an additional ~input~.
          
          #br#br

          To *delete a term*, press "Delete". This replaces the term with a hole.
          #task[Delete the base case value.]

          #br

          To *add an argument* to a function, write "fun" at the function body and then press "Enter".
          #task[Add an argument to fib.]
          When you do this, #Pantograph automatically updates `fib`'s type and each `fib` call.

          #br#br

          To *name or rename a variable*, write the new name at the variable and then press "Enter".
          #task[Name the new argument "m".]
          #task[Fill in all holes in fib with m.]
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
            [ HH.li_ $ parseMd """ *Fill in a hole*: write the value in the hole """
            , HH.li_ $ parseMd """ *Delete a term*: press "Delete" """
            , HH.li_ $ parseMd """ *Add a function argument*: write "fun" at the function body """
            , HH.li_ $ parseMd """ *Name a variable*: write the variable's new name at the variable """
            ]
        , HH.p_ $ parseMd """
          In addition to adding a function argument with "fun", you can also insert other constructs.
          """
        , HH.ul_
            [ HH.li_ $ parseMd """ *Add a new definition*: write "let" """
            ]
        , HH.p_ $ parseMd """
          Using your knowledge so far, write the function `plus` from scratch:
          #task[Add a new defintion called "plus"]
          #task[At the body of plus (after the "="), add two function arguments named "x" and "y".]
          #task[Fill the hole with "+". Pantograph will automatically create holes for its two arguments.]
          #task[Fill the two arguments of + with x and y]
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Selections"]
        , HH.p_ $ parseMd """
          So far you've been editing with only the *cursor*.
          
          #br#br

          In #Pantograph you can make a *selection* by dragging over a region.
          #task[Select triple's function argument x by dragging from the "fun x" to the "(3 * x)"]
          #task[Select max's function arguments x and y by dragging from the "fun x" to the "if (x > y)"]
          #task[Select the definition of triple by dragging from the "let triple" to the "let square"]
          #task[Select the definitions of triple and square by dragging from the "let triple" to the "let max"]

          #br

          Try making some other selections to get a feel for it.
          Observe that you can only make selections between ~terms~.
          """
        ]
  , 
    makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["max"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpGreater"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      [] $
      defer \_ -> HH.div_
        [ HH.h1_ [HH.text "Rearranging Definitions"]
        , HH.p_ $ parseMd """
          Use a selection and cut + paste to rearrange the definitions of `triple` and `square`.
          #task[Select the definition of triple by dragging from the "let triple" to the "let square".]
          #task[Cut (ctrl+x) the definition of triple.]
          ~Observe~ that since `triple` is now undefined, the `triple` call in `(max (triple y) (square y))` is underlined.
          #task[Move the cursor between the definitions of square and max (right before "let max").]
          #task[Paste (ctrl+v) the definition of triple.]
          ~Observe~ that since `triple` is now defined again, the `triple` call is not underlined anymore.
          
          #br#br
          
          #task[Rearrange the definitions of square and max.]
          """
        ]
  ]