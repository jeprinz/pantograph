module Tutorial.CurriedTutorial where

import Prelude
import Tutorial.Tutorial
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Editor as Editor
import Language.Pantograph.Generic.Rendering.Base as Base
import Bug as Bug
import Tutorial.PantographTutorial
import Halogen.HTML as HH
import Language.Pantograph.Specific.Currying as Currying
import Effect (Effect)

{-
A specific tutorial for the Currying.purs language
-}

lessons :: Array Lesson
lessons = [
    pantographLesson Currying.editorSpec (HH.text "lesson 1111")
    , {component: \_ -> exampleLesson, instructions: HH.text (longString <> longString <> longString)}
    , {component: \_ -> exampleLesson2, instructions: HH.text "lesson2"}
    , {component: \_ -> exampleLesson, instructions: HH.text "lesson3"}
    , {component: \_ -> exampleLesson, instructions: HH.text "lesson4"}
]
