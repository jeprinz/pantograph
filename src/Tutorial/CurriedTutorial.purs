module Tutorial.CurriedTutorial where

import Prelude
import Tutorial.Tutorial as Tutorial
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Language.Pantograph.Generic.Rendering.Base as Base
import Bug as Bug
import Tutorial.PantographTutorial
import Halogen.HTML as HH
import Language.Pantograph.Specific.Currying as Currying
import Effect (Effect)
import Data.Lazy (Lazy, defer, force)
import Tutorial.EditorTutorial2

{-
A specific tutorial for the Currying.purs language
-}
test1 :: String
test1 = """{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["this"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"ArrowRule"}},[{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"String"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["x"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"String"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[10],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["is"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["a"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Let"}},[{"values":[{"tag":"Right","value":{"values":["test"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[{"values":[],"tag":"Int"}],"tag":"DataTypeRule"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[5],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TermHole"}},[{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""

test2 = """{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["another"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"Lam"}},[{"values":[{"tag":"Right","value":{"values":["lesson"],"tag":"DataString"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"TypeHole"}},[]],"tag":"Expr"},{"values":[{"tag":"Left","value":{"values":[],"tag":"IntegerLiteral"}},[{"values":[{"tag":"Right","value":{"values":[100],"tag":"DataInt"}},[]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}]],"tag":"Expr"}"""

prog :: String -> Lazy (Grammar.DerivTerm Currying.PreSortLabel Currying.RuleLabel)
prog str = defer (\_ -> Grammar.decodeSerializedZipper2 Currying.editorSpec.clipboardSort str)

--makePath :: String -> Grammar.DerivTerm Currying.PreSortLabel Currying.RuleLabel -> Lazy (Grammar.DerivPath Dir.Up Currying.PreSortLabel Currying.RuleLabel)
--makePath string dterm = Grammar.deserializePath string dterm

paths1 = defer $ \_ -> map (Grammar.deserializePath (force (prog test1))) ["[3,3,2,0]"]

--lessons :: Array Tutorial.Lesson
--lessons = [
--    pantographLesson Currying.editorSpec (prog test2) (defer \_ -> []) (HH.text "lesson 1111")
--  ,  pantographLesson Currying.editorSpec (prog test1) (defer \_ -> []) (HH.text "lesson 222")
----    , pantographLesson Currying.editorSpec (prog test1) paths1 (HH.text "lesson 333")
--]

lessons :: Array (Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel))
lessons = [
--    defer \_ -> {program: prog test1, paths: force paths1, instructions: "lesson1"}
    defer \_ -> {program: force (prog test1), paths: [], instructions: "lesson 1"}
    , defer \_ -> {program: force (prog test2), paths: [], instructions: "lesson 2"}
    , defer \_ -> {program: force (prog test1), paths: force paths1, instructions: "lesson 3"}
]