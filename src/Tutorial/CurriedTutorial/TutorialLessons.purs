module Tutorial.CurriedTutorial.TutorialLessons where

import Prelude
import Tutorial.Markdown (parseMd)
import Tutorial.CurriedTutorial.Common (editActions, makeLesson, selectionsWarning)
import Data.Lazy (Lazy, defer)
import Halogen.HTML as HH
import Language.Pantograph.Specific.Currying as Currying
import Tutorial.EditorTutorial2 (Lesson)

lessons :: Array (Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel))
lessons =
  [ makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            $ [ HH.h1_ [ HH.text "Introduction" ]
              , HH.p_
                  $ parseMd
                      """
          This tutorial is an introduction to ♯Pantograph, a structure editor.
          Although the code looks like text, it actually isn't.

          ♯br♯br

          Just like a text editor, you can navigate in Pantograph using a cursor.
          Unlike a text editor, the cursor goes around entire expressions.
          
          ♯task⟦Hover over the program to reveal its structure.⟧
          ♯task⟦Click to place the cursor.⟧

          ♯br
          
          Then, click ♯button⟦Next Lesson⟧ to move on.
          """
              ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Holes" ]
            , HH.p_
                $ parseMd
                    """
          Here is the same program, but with a couple of the terms removed.
          Two of the numbers from the program have been deleted, leaving holes in their place.
          ♯br♯br
          A ♯bold⟦hole⟧ is a placeholder term, displayed as ♯dataTyHole⟦T⟧, where ♯code⟦T⟧ is the type of the hole.
          ♯br♯br
          To ♯bold⟦fill⟧  in a hole,
          move the cursor to the ♯code⟦□⟧ inside the hole,
          then write the value to fill,
          and finally press "Tab".
          
          ♯task⟦Fill each hole.⟧

          As you type, ♯Pantograph will list all the possible terms of the right type to fit in the hole.
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["double"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["result"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[100],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Insertion" ]
            , HH.p_
                $ parseMd
                    """
          In Pantograph, you can also wrap forms around existing expressions in the program.
          You will wrap a call to ♯code⟦double⟧ around ♯code⟦100⟧

          ♯task⟦Click on "100"⟧

          ♯task⟦Type "double" and press Tab.⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpAnd"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstFalse"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpAnd"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Edit practice" ]
            , HH.p_
                $ parseMd
                    """
        There are two ways to insert nodes in pantograph:
        """
            , HH.ul_
                [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: click on ♯code⟦□⟧ """
                , HH.li_ $ parseMd """ ♯bold⟦Wrap around a term⟧: click on the term """
                ]
            , HH.p_
                $ parseMd
                    """
        ♯br

        Using these two kinds of edits,
        ♯task⟦Edit each "task" to match each "given"⟧
        """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[7],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpAnd"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstFalse"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpAnd"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Deletion" ]
            , HH.p_
                $ parseMd
                    """
        You can delete a term with "Delete" or "Backspace".
        ♯br♯br
        Using deletion and filling holes,
        ♯task⟦Edit each "task" to match each "given"⟧
        """
            ]
--  , makeLesson
--      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["fib"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["n"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpLessEq"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[2],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
--      []
--      $ defer \_ ->
--          HH.div_
--            [ HH.h1_ [ HH.text "Extended Editing Example" ]
--            , HH.p_
--                $ parseMd
--                    """
--          Putting together what you have learned so far, we will perform a larger edit on a program.
--
--          ♯br
--
--          In this lesson, we will edit ♯code⟦fib⟧ to take its base case as an additional input rather than being fixed as ♯code⟦1⟧.
--
--          ♯br♯br
--
--          To ♯bold⟦delete⟧ a term, press "Delete" or "Backspace". This replaces the term with a hole.
--          ♯task⟦Delete the base case value: Delete the "1" in "then 1".⟧
--
--          ♯br
--
--          To wrap a ♯code⟦fun⟧ around the term at your cursor, query (a prefix of) "lambda" and press Tab.
--
--          ♯task⟦Place your cursor by clicking "fun", then wrap a second "fun" around the body of fib.⟧
--          When you do this, ♯Pantograph automatically updates ♯code⟦fib⟧'s type and each ♯code⟦fib⟧ call.
--
--          ♯br♯br
--
--          To name or rename a variable, write the new name at the variable and then press "Tab".
--          ♯task⟦Name the new argument "m".⟧
--          ♯task⟦Fill in all holes in fib's body with m.⟧
--          """
--            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Practice" ]
            , HH.p_
                $ parseMd
                    """
          So far, you have learned how to:
          """
            , HH.ul_
                [ HH.li_ $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
                , HH.li_ $ parseMd """ ♯bold⟦Wrap a form around a term⟧: write the value at the term """
                , HH.li_ $ parseMd """ ♯bold⟦Delete a term⟧: press "Delete/Backspace" """
                , HH.li_ $ parseMd """ ♯bold⟦Name a variable⟧: Input the variable's name """
                ]
            , HH.p_
                $ parseMd
                    """
          Using your knowledge so far, write the function ♯code⟦plus⟧, which takes two inputs and uses "+" to add them together, from scratch.
          ♯br♯br
          """
            , HH.p_
                $ parseMd
                    """
          ♯task⟦Write the function "plus".⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["double"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square_or_triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["b"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Introducing Selections" ]
            , HH.p_
                $ parseMd
                    """
          So far you've been editing with only the ♯bold⟦cursor⟧.
          
          ♯br♯br

          In ♯Pantograph you can also make a ♯bold⟦selection⟧ between two terms by clicking and dragging the mouse from one term to the other.
          This selects the area ♯bold⟦inside one term, but outside the other⟧.

          ♯br♯br

          Try making some selections to get a feel for it. In the next lessons, we will explore selections in detail.
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpAnd"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstFalse"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpMinus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[6],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["given"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["task "],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Selection Practice" ]
            , HH.p_
                $ parseMd
                    """

          For each task, make a single selection and press "Backspace/Delete".

          ♯task⟦Edit each "task" to match each "given"⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["double"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square_or_triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["b"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Rearranging Definitions" ]
            , HH.p_ $ parseMd $ selectionsWarning
                <> """
          One thing that you can do with selections is to rearrange definitions.
          We will swap the order of the ♯code⟦square⟧ and ♯code⟦triple⟧ definition.

          ♯task⟦Select the definition of square: drag from the "let" in "let square" to the "let" in "let triple".⟧

          ♯task⟦Cut (ctrl+x).⟧
          
          ♯task⟦Move the cursor below the definition of triple: click on the "let" in "let square_or_triple".⟧

          ♯task⟦Paste (ctrl+v).⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["double"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["square_or_triple"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["b"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"If"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"ConstTrue"}],"tag":"ConstantRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Rearranging Function Calls" ]
            , HH.p_ $ parseMd $ selectionsWarning
                <> """
          You will use a selection to rearrange the function calls on the last line from
          ♯br
          ♯code⟦square_or_triple (double 10) true⟧ to ♯br
          ♯code⟦double (square_or_triple 10 true)⟧

          ♯br ♯br

          To do this, first note that to place your cursor on a function application, you click on the parenthesis around the
          function call.

          ♯task⟦Select the "square_or_triple" function call: drag from the "(" to the left of "double" to the "10".⟧
          
          ♯task⟦Cut (ctrl+x).⟧
          ♯task⟦Paste onto the "(" in "(square_or_triple".⟧

          ♯br ♯br

          ♯bold⟦Bonus:⟧ try moving the ♯code⟦square_or_triple⟧ call. Notice that you can make a selection that wouldn't be possible in a text editor!
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["average"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["z"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpDivide"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["result"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[20],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["scaled_result"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Partial Function Calls" ]
            , HH.p_ $ parseMd $ selectionsWarning
                <> """
          The gray circles between function call arguments are "handles" that let you select sequences of arguments.

          Delete the last argument of the call ♯code⟦(average 1 4 20)⟧.
          
          ♯task⟦Select the last argument: drag from the gray circle right before "20" to the ")" right after it.⟧
          ♯task⟦Delete.⟧

          ♯code⟦average⟧ is now only called with its first two arguments.
          As a result, ♯Pantograph automatically updated the type of ♯code⟦result⟧ to be ♯code⟦Int → Int⟧ 
          and added a hole for the missing argument in ♯code⟦10 * (result { □ : Int })⟧.

          ♯br♯br

          Now, delete both remaining arguments of the call ♯code⟦(average 1 4)⟧.
          ♯task⟦Select both arguments: drag from the gray circle right before "1" to the ")" right after "4".⟧
          ♯task⟦Delete.⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["average"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Newline"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["z"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpDivide"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpPlus"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[3],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["result"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[1],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[4],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[20],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["scaled_result"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"OpTimes"}],"tag":"InfixRule"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Rearranging Function Inputs" ]
            , HH.p_ $ parseMd $ selectionsWarning
                <> """
          Selections can be used to rearrage a function's arguments.

          You will switch the order of inputs ♯code⟦x⟧ and ♯code⟦y⟧ in the definition of ♯code⟦average⟧.

          ♯task⟦Select the "x" input: drag from the "fun" in "fun x" to the "fun" in "fun y".⟧
          ♯task⟦Cut.⟧

          Since ♯code⟦average⟧'s first input was removed, the argument ♯code⟦1⟧ in ♯code⟦(average 1 4 20)⟧ is ♯greyError⟦out-of-place⟧.

          ♯task⟦Move to the "z" argument: click on the "fun" in "fun z".⟧
          ♯task⟦Paste.⟧

          Now, fix the out-of-place argument ♯code⟦1⟧ in the ♯code⟦average⟧ call in the body of ♯code⟦result⟧ by cut-pasting it to the right place.
          ♯task⟦Select the out-of-place argument: Click on "1"⟧
          ♯task⟦Cut.⟧
          ♯task⟦Move to the □ in {□ : Int}⟧
          ♯task⟦Paste.⟧

          Notice that the out of place argument disappears.
          """
--          ♯task⟦Select the out-of-place argument: drag from the gray circle right before "1" to the gray circle right after it⟧
--          ♯task⟦Cut.⟧
--          ♯task⟦Move to the gray circle right after ♯dataTyHole⟦Int⟧: click on that gray circle⟧
--          ♯task⟦Paste.⟧
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Type Boundaries" ]
            , HH.p_
                $ parseMd
                    """
          ♯Pantograph prevents you from making type errors.
          But you can still make edits that that introduce a temporary ♯bold⟦type boundary⟧, 
          where the type of the term inside the boundary doesn't fit with the code around it.

          ♯br♯br

          Edit ♯code⟦f⟧ to take a ♯code⟦Bool⟧ as input instead of an ♯code⟦Int⟧.

          ♯task⟦Replace the input "Int" of "f" with "Bool"⟧

          The ♯code⟦y⟧ in ♯code⟦(f y)⟧ now has a type boundary around it.
          Try hovering over it.

          ♯br

          Next, you will update ♯code⟦y⟧ to fix the boundary.

          ♯task⟦Replace the type of y with Bool⟧

          We fixed ♯code⟦(f y)⟧, but now there is a type boundary around the value of ♯code⟦y⟧.
          Type boundaries are themselves terms, and you can place your cursor on them by
          clicking the ♯code⟦⦃⟧ ♯code⟦⦄⟧.

          ♯task⟦Delete the ⦃10⦄ in "let y : Bool = ⦃10⦄".⟧
          ♯task⟦Fill the hole with a boolean value.⟧
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["f"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["y"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Bool"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ErrorBoundary"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"App"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Suc"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ErrorBoundary"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Var"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"Zero"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            [ HH.h1_ [ HH.text "Deleting a Type Boundary" ]
            , HH.p_
                $ parseMd
                    """
          The types in this program don't line up, and there are two type boundaries.
          This time, you will edit the program so that the 10 remains in place.

          ♯br♯br

          Because type boundaries are themselves terms, you can select the type boundary itself.
          ♯task⟦Select the type boundary: drag from the "⦃" or "⦄" to the "10".⟧
          ♯task⟦Delete.⟧
          ♯Pantograph automatically changed the type of ♯code⟦y⟧ to ♯code⟦Int⟧ in order to match ♯code⟦10⟧, 
          and this fixed the boundary later in the program as well.
          
          ♯br♯br
          
          When you delete a selection, ♯Pantograph updates the program around the ouside the selection to respect the
          type of the new term now in that place.
          """
            ]
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["map"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            $ [ HH.h1_ [ HH.text "Exercise: map" ]
              , HH.p_
                  $ parseMd
                      """
          Implement ♯code⟦map⟧, a function that applies a function to each element of a list.

          ♯br♯br

          Example behavior: ♯code⟦map (fun x => (x + 1)) (cons 0 (cons 1 (cons 2 nil))) = (cons 1 (cons 2 (cons 3)))⟧

          ♯br♯br

          Type ♯code⟦match⟧ to make a match expression that matches on a list.
          """
              ]
--            <> editActions
  , makeLesson
      """{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}"""
      []
      $ defer \_ ->
          HH.div_
            $ [ HH.h1_ [ HH.text "End of tutorial" ]
              , HH.p_
                  $ parseMd
                      """
          You now know the basics of editing with ♯Pantograph!
          If you have extra time, feel free to experiment on your own.
          """
              ]
            <> editActions
--  , makeLesson
--      """{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}"""
--      []
--      $ defer \_ ->
--          HH.div_
--            $ [ HH.h1_ [ HH.text "Checkpoint before Exercises" ]
--              , HH.p_
--                  $ parseMd
--                      """
--          You now know the basics of editing with ♯Pantograph!
--          Take a moment to experiment with this novel style of editing.
--          """
--              ]
--            <> editActions
--            <> [ HH.p_
--                  $ parseMd
--                      """
--          The next lessons are simple exercises to practice what you've learned.
--          """
--              ]
--  , makeLesson
--      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["sum"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
--      []
--      $ defer \_ ->
--          HH.div_
--            $ [ HH.h1_ [ HH.text "Exercise: sum" ]
--              , HH.p_
--                  $ parseMd
--                      """
--          Implement ♯code⟦sum⟧, a function that computes the sum of a list of integers.
--
--          ♯br♯br
--
--          Example behavior: ♯code⟦sum (cons 0 (cons 1 (cons 2 nil))) = 3⟧
--          """
--              ]
--            <> editActions
--  , makeLesson
--      """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["map"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""
--      []
--      $ defer \_ ->
--          HH.div_
--            $ [ HH.h1_ [ HH.text "Exercise: map" ]
--              , HH.p_
--                  $ parseMd
--                      """
--          Implement ♯code⟦map⟧, a function that applies a function to each element of a list.
--
--          ♯br♯br
--
--          Example behavior: ♯code⟦map (fun x => (x + 1)) (cons 0 (cons 1 (cons 2 nil))) = (cons 1 (cons 2 (cons 3)))⟧
--          """
--              ]
--            <> editActions
  ]
