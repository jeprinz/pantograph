module Pantograph.Specific.LC.Rendering.Basic (renderer) where

import Data.Tree
import Data.Tuple.Nested
import Pantograph.Specific.LC.Language
import Prelude

import Control.Monad.Reader (ask, local)
import Control.Monad.State as State
import Data.Array as Array
import Data.List (List(..))
import Data.Traversable (sequence)
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pantograph.Generic.Language as PL
import Pantograph.Generic.Rendering as PR
import Record as R
import Todo (todo)
import Type.Proxy (Proxy(..))

type RenderCtx = (indentLevel :: Int)

type RenderEnv = ()

type Renderer = PR.Renderer SN EL RenderCtx RenderEnv

renderer :: Renderer
renderer = PR.Renderer
  { name: "basic"
  , language
  , topCtx: {indentLevel: 0}
  , topEnv: {}
  , arrangeExpr:
      let punc str = PR.HtmlArrangeKid [HH.span_ [HH.text str]] in
      let indent i = PR.IndentationArrangeKid (Array.replicate (i + 1) (HH.text "  ")) in
      let newline i = PR.IndentationArrangeKid (Array.replicate i (HH.text "  ")) in
      \node@(PL.AnnExprNode {label}) ->
        let ass = assertValidTreeKids "arrangeExpr" (PL.shrinkAnnExprNode node :: PL.ExprNode SN EL) in
        case label of
          StringRule -> ass \[] -> do
            let Tree (PL.SortNode StringSort) [Tree (PL.SortNode (StringValue str)) []] = PL.getExprNodeSort language node
            pure [PR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "string"]] [HH.text str]]]
          VarRule -> ass \[mx] -> do
            x_ /\ _x <- mx
            pure [punc "#", PR.ExprKidArrangeKid x_]
          LamRule -> ass \[mx, mb] -> do
            x_ /\ _x <- mx 
            b_ /\ _b <- mb 
            pure [punc "(", punc "λ", PR.ExprKidArrangeKid x_, punc ".", PR.ExprKidArrangeKid b_, punc ")"]
          AppRule -> ass \[mf, ma] -> do
            f_ /\ _f <- mf 
            a_ /\ _a <- ma 
            pure [punc "(", PR.ExprKidArrangeKid f_, punc " ", PR.ExprKidArrangeKid a_, punc ")"]
          LetRule -> ass \[mx, ma, mb] -> do
            ctx <- ask
            x_ /\ _x <- mx
            a_ /\ _a <- ma
            b_ /\ _b <- mb
            pure [punc "(", punc "let", punc " ", PR.ExprKidArrangeKid x_, punc " ", punc "=", punc " ", PR.ExprKidArrangeKid a_, punc " ", punc "in", punc " ", PR.ExprKidArrangeKid b_, punc ")"]
          HoleRule -> ass \[] -> do
            holeIndex <- State.gets _.holeCount
            State.modify_ _ {holeCount = holeIndex + 1}
            pure [PR.HtmlArrangeKid [HH.span [HP.classes [HH.ClassName "holeIndex"]] [HH.text ("?" <> show holeIndex)]]]
          FormatRule _ -> \mkids -> do
            as /\ nodes <- Array.unzip <$> sequence mkids
            Debug.traceM $ "renderExpr (FormatRule IndentedNewline): nodes = " <> show (PL.shrinkAnnExprNode <$> nodes :: Array (PL.ExprNode SN EL))
            todo unit
          -- FormatRule IndentedNewline -> ass \[ma] -> do
          --   ctx <- ask
          --   a_ /\ _a <- local (R.modify (Proxy :: Proxy "indentLevel") (1 + _)) ma
          --   pure [indent ctx.indentLevel, PR.ExprKidArrangeKid a_]
          -- FormatRule Newline -> ass \[ma] -> do
          --   ctx <- ask
          --   a_ /\ _a <- ma
          --   pure [newline ctx.indentLevel, PR.ExprKidArrangeKid a_]
  , beginsLine: case _ of
      Cursor {outside: Path (Cons (Tooth (PL.AnnExprNode {label: FormatRule IndentedNewline}) _ _) _), orientation: Outside} -> true
      Cursor {outside: Path (Cons (Tooth (PL.AnnExprNode {label: FormatRule Newline}) _ _) _), orientation: Outside} -> true
      _ -> false
  }

