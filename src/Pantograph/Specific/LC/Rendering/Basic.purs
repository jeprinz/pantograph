module Pantograph.Specific.LC.Rendering.Basic (renderer) where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Specific.LC.Language
import Prelude

import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.List (List(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language as PGL
import Pantograph.Generic.Rendering as PGR
import Record as R
import Type.Proxy (Proxy(..))

type RenderCtx = (indentLevel :: Int)

type RenderEnv = ()

type Renderer = PGR.Renderer SN EL RenderCtx RenderEnv

renderer :: Renderer
renderer = PGR.Renderer
  { name: "basic"
  , language
  , topCtx: {indentLevel: 0}
  , topEnv: {}
  , arrangeExpr:
      let punc str = PGR.HtmlArrangeKid [HH.span_ [HH.text str]] in
      let indent i = PGR.IndentationArrangeKid (Array.replicate (i + 1) (HH.text "  ")) in
      let newline i = PGR.IndentationArrangeKid (Array.replicate i (HH.text "  ")) in
      \node@(PGL.AnnExprNode {label}) ->
        let msg = "arrangeExpr " <> "{" <> "label: " <> show label <> "}" in
        let ass = assertValidTreeKids msg node in
        case label of
          StringRule -> ass \[] -> do
            let Tree (PGL.SortNode StringSort) [Tree (PGL.SortNode (StringValue str)) []] = PGL.getExprNodeSort language node
            pure [PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "string"]] [HH.text str]]]
          VarRule -> ass \[mx] -> do
            x_ /\ _x <- mx
            pure [punc "#", PGR.ExprKidArrangeKid x_]
          LamRule -> ass \[mx, mb] -> do
            x_ /\ _x <- mx 
            b_ /\ _b <- mb 
            pure [punc "(", punc "λ", PGR.ExprKidArrangeKid x_, punc ".", PGR.ExprKidArrangeKid b_, punc ")"]
          AppRule -> ass \[mf, ma] -> do
            f_ /\ _f <- mf 
            a_ /\ _a <- ma 
            pure [punc "(", PGR.ExprKidArrangeKid f_, punc " ", PGR.ExprKidArrangeKid a_, punc ")"]
          LetRule -> ass \[mx, ma, mb] -> do
            ctx <- ask
            x_ /\ _x <- mx
            a_ /\ _a <- ma
            b_ /\ _b <- mb
            pure [punc "(", punc "let", punc " ", PGR.ExprKidArrangeKid x_, punc " ", punc "=", punc " ", PGR.ExprKidArrangeKid a_, punc " ", punc "in", punc " ", PGR.ExprKidArrangeKid b_, punc ")"]
          HoleRule -> ass \[] -> do
            holeIndex <- State.gets _.holeCount
            State.modify_ _ {holeCount = holeIndex + 1}
            pure [PGR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "holeIndex"]] [HH.text ("?" <> show holeIndex)]]]
          FormatRule IndentedNewline -> ass \[ma] -> do
            ctx <- ask
            a_ /\ _a <- local (R.modify (Proxy :: Proxy "indentLevel") (1 + _)) ma
            pure [indent ctx.indentLevel, PGR.ExprKidArrangeKid a_]
          FormatRule Newline -> ass \[ma] -> do
            ctx <- ask
            a_ /\ _a <- ma
            pure [newline ctx.indentLevel, PGR.ExprKidArrangeKid a_]
  , beginsLine: case _ of
      Cursor {outside: Path (Cons (Tooth (PGL.AnnExprNode {label: FormatRule IndentedNewline}) _ _) _), orientation: Outside} -> true
      Cursor {outside: Path (Cons (Tooth (PGL.AnnExprNode {label: FormatRule Newline}) _ _) _), orientation: Outside} -> true
      _ -> false
  }

