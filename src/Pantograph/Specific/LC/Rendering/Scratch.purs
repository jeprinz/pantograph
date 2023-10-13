-- module Pantograph.Specific.LC.Rendering.Scratch where
module Pantograph.Specific.LC.Rendering.Scratch (renderer) where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Specific.LC.Language
import Prelude
import Control.Monad.State as State
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language as PGL
import Pantograph.Generic.Rendering as PGR

type RenderCtx = ()

type RenderEnv = ()

type Renderer = PGR.Renderer SN EL RenderCtx RenderEnv

renderer :: Renderer
renderer = PGR.Renderer
  { name: "scratch"
  , language
  , topCtx: {}
  , topEnv: {}
  , arrangeExpr:
      let puncSpan str = PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "puncSpan"]] [HH.text str]] in
      let puncDiv str = PGR.HtmlArrangeKid [HH.div [HP.classes [HH.ClassName "puncDiv"]] [HH.text str]] in
      \node@(PGL.AnnExprNode {label}) ->
        let msg = "arrangeExpr " <> "{" <> "label: " <> show label <> "}" in
        let ass = assertValidTreeKids msg node in
        case label of
          StringRule -> ass \[] -> do
            let Tree {node: PGL.SortNode StringSort, kids: [Tree {node: PGL.SortNode (StringValue str)}]} = PGL.getExprNodeSort language node
            -- Debug.traceM $ "sort = " <> show (PGL.getExprNodeSort language node)
            -- pure [PGR.HtmlArrangeKid [HH.text str]]
            pure [PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "string"]] [HH.text str]]]
          VarRule -> ass \[mx] -> do
            x_ /\ _x <- mx
            pure [puncDiv "#", PGR.ExprKidArrangeKid x_]
          LamRule -> ass \[mx, mb] -> do
            x_ /\ _x <- mx 
            b_ /\ _b <- mb 
            pure [puncDiv "λ", PGR.ExprKidArrangeKid x_, puncSpan ".", PGR.ExprKidArrangeKid b_]
          AppRule -> ass \[mf, ma] -> do
            f_ /\ _f <- mf 
            a_ /\ _a <- ma 
            pure [puncDiv "$", PGR.ExprKidArrangeKid f_, puncSpan " ", PGR.ExprKidArrangeKid a_]
          LetRule -> ass \[mx, ma, mb] -> do
            x_ /\ _x <- mx
            a_ /\ _a <- ma
            b_ /\ _b <- mb
            pure [puncDiv "let", PGR.ExprKidArrangeKid x_, puncSpan "=", PGR.ExprKidArrangeKid a_, puncDiv "in", PGR.ExprKidArrangeKid b_]
          HoleRule -> ass \[] -> do
            holeIndex <- State.gets _.holeCount
            State.modify_ _ {holeCount = holeIndex + 1}
            pure [puncDiv "?", PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "holeIndex"]] [HH.text (show holeIndex)]]]
          FormatRule _ -> ass \[ma] -> do
            a_ /\ a <- ma
            pure [PGR.IndentationArrangeKid [], PGR.ExprKidArrangeKid a_]
   , beginsLine: \_ -> false
 }