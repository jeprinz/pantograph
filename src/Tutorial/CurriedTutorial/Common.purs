module Tutorial.CurriedTutorial.Common where

import Prelude
import Tutorial.Markdown (HTML, parseMd)
import CSS as CSS
import CSS.Size as CSSSize
import Data.Bifunctor (bimap)
import Data.Expr as Expr
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.Utilities (classNames)
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Base as Base
import Language.Pantograph.Generic.Rendering.Rendering as Rendering
import Language.Pantograph.Specific.Currying as Currying
import Tutorial.EditorTutorial2 (Lesson)

prog :: String -> Grammar.DerivTerm Currying.PreSortLabel Currying.RuleLabel
prog str = Grammar.decodeSerializedZipper2 Currying.editorSpec.clipboardSort str

renderProgram :: String -> HH.HTML Unit Unit
renderProgram program =
  HH.div [ classNames [ "program" ] ]
    $ pure
    $ bimap (const unit) (const unit)
    $ Rendering.renderDerivTerm
        (Base.trivialEditorLocals Currying.editorSpec)
        false
        false
        (Expr.Zipper (Expr.Path Nil) (Grammar.decodeSerializedZipper2 Currying.editorSpec.clipboardSort program))
        (Base.defaultRenderingContext "TEST")
          { isInteractive = false }

makeLesson :: String -> Array String -> Lazy HTML -> Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel)
makeLesson progString paths instructions =
  defer \_ ->
    let
      program = prog progString
    in
      { program
      , paths: map (Grammar.deserializePath program) paths
      , instructions: force instructions
      }

makeLesson' :: String -> Array String -> Lazy (HH.HTML Unit Unit) -> Lazy (Lesson Currying.PreSortLabel Currying.RuleLabel)
makeLesson' progString paths instructions =
  defer \_ ->
    let
      program = prog progString
    in
      { program
      , paths: map (Grammar.deserializePath program) paths
      , instructions: force instructions
      }

selectionsWarning :: String
selectionsWarning =
  if false then
    """
    ♯italic⟦Note⟧. 
    ♯bold⟦Be very precise when making selections!⟧
    If you get stuck, you can ♯button⟦Reset⟧ or undo (ctrl+z).
    ♯br♯br
    """
  else
    ""

editActions :: Array (HH.HTML Unit Unit)
editActions =
  [ HH.div
      [ HCSS.style do
          CSS.border CSS.inset borderWidth CSS.black
          (let s = CSS.em 0.5 in CSS.padding s s s s)
          CSS.marginTop $ CSS.em 1.0
          (let s = CSS.em 0.5 in CSS.borderRadius s s s s)
      ]
      [ HH.div
          [ HCSS.style do
              CSS.rule $ CSS.Property (CSS.fromString "font-variant-caps") (CSS.Value (CSS.fromString "small-caps"))
              CSS.rule $ CSS.Property (CSS.fromString "text-align") (CSS.Value (CSS.fromString "center"))
          ]
          [ HH.text "Pantograph Cheatsheet" ]
      , HH.hr
          [ HCSS.style do
              CSS.rule $ CSS.Property (CSS.fromString "border") (CSS.Value (CSS.fromString "none"))
              CSS.borderBottom CSS.inset borderWidth CSS.black
          ]
      , let
          li =
            HH.li
              [ HCSS.style do
                  (let s = CSSSize.unitless 0.0 in CSS.margin s s s s)
                  (let s = CSSSize.unitless 0.0 in CSS.padding s s s s)
                  CSS.marginLeft (CSS.em (-1.0))
              ]
        in
          HH.ul
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.flexDirection CSS.column
                CSS.rule $ CSS.Property (CSS.fromString "gap") (CSS.value (CSS.em 1.0))
            ]
            [ li $ parseMd """ ♯bold⟦Fill in a hole⟧: write the value in the hole """
            , li $ parseMd """ ♯bold⟦Wrap a form around an term⟧: write the value at the term """
            , li $ parseMd """ ♯bold⟦Delete a term or selection⟧: press "Delete" """
            , li $ parseMd """ ♯bold⟦Name a variable⟧: write the variable's new name at the variable """
            , li $ parseMd """ ♯bold⟦Built-in functions⟧: """
            , HH.ul_
                [ li $ parseMd """ ♯code⟦+⟧, ♯code⟦-⟧, ♯code⟦*⟧, ♯code⟦/⟧, ♯code⟦%⟧: integer operations """
                , li $ parseMd """ ♯code⟦if⟧: branch on a boolean condition """
                , li $ parseMd """ ♯code⟦match⟧: pattern-match on the ♯code⟦nil⟧ and ♯code⟦cons⟧ case of a list """
                ]
            ]
      ]
  ]
  where
  -- borderWidth = CSS.em 0.2
  borderWidth = CSS.px 2.0

divFlexColumn :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
divFlexColumn =
  HH.div
    [ HCSS.style do
        CSS.display CSS.flex
        CSS.flexDirection CSS.column
    ]
