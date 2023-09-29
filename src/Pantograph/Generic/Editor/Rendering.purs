module Pantograph.Generic.Editor.Rendering where

import Pantograph.Generic.Editor.Common
import Pantograph.Generic.Language
import Prelude

import Bug (bug)
import Control.Monad.Reader (ask)
import Data.Derivative (integrate)
import Data.Fix (Fix(..))
import Data.Traversable (sequence, traverse)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)

renderExprHelper rule classNames attrs jExprHtml = do
  ctx <- ask
  let jHtmlExpr = Html (HU.freshElementId unit) (jExprHtml <#> _.htmlExpr)
  htmls <- arrangeExpr jExprHtml
  pure 
    { htmlExpr: Fix jHtmlExpr
    , html:
        HH.div
          ( [ HP.classes ((HH.ClassName <$> (["Expr"] <> classNames)) <> toClassNames rule)
            -- , HE.onClick \mouseEvent -> do
            --     HK.raise ctx.outputToken 
            ] <> attrs )
          htmls }

renderExpr :: forall ctx env rule joint tooth. Editor rule joint tooth =>
  Fix (PrgmExprJoint rule joint joint) ->
  RenderExprHtml ctx env rule joint tooth
renderExpr (Fix jExpr@(Expr rule _ _)) = renderExpr `traverse` jExpr >>= renderExprHelper rule [] []

renderCursor :: forall ctx env rule joint tooth. Editor rule joint tooth =>
  Fix (PrgmCursorJoint rule joint joint tooth) ->
  RenderExprHtml ctx env rule joint tooth
renderCursor (Fix (CursorThere tExpr@(ExprTooth rule _ _) cursor)) = sequence (integrate (renderCursor cursor) (renderExpr <$> tExpr) :: PrgmExprJoint rule joint joint _) >>= renderExprHelper rule [] []
renderCursor (Fix (CursorHere (Fix jExpr@(Expr rule _ _)))) = renderExpr `traverse` jExpr >>= renderExprHelper rule ["Cursor"] []

renderSelect :: forall ctx env rule joint tooth. Editor rule joint tooth =>
  Fix (PrgmSelectJoint rule joint joint tooth) ->
  RenderExprHtml ctx env rule joint tooth
renderSelect (Fix (CursorThere tExpr@(ExprTooth rule _ _) select)) = do
  jExprHtml <- sequence (integrate (renderSelect select) (renderExpr <$> tExpr) :: PrgmExprJoint rule joint joint _)
  renderExprHelper rule [] [] jExprHtml
renderSelect (Fix (CursorHere (Fix (CursorThere tExpr@(ExprTooth rule _ _) selectInner)))) = sequence (integrate (renderSelectInner selectInner) (renderExpr <$> tExpr) :: PrgmExprJoint rule joint joint _) >>= renderExprHelper rule ["SelectOuter"] []
renderSelect (Fix (CursorHere (Fix (CursorHere _)))) = bug $ "[renderSelect] Empty selection."

renderSelectInner :: forall ctx env rule joint tooth. Editor rule joint tooth =>
  Fix (PrgmCursorJoint rule joint joint tooth) ->
  RenderExprHtml ctx env rule joint tooth
renderSelectInner (Fix (CursorThere tExpr@(ExprTooth rule _ _) selectInner)) = sequence (integrate (renderSelectInner selectInner) (renderExpr <$> tExpr) :: PrgmExprJoint rule joint joint _) >>= renderExprHelper rule [] []
renderSelectInner (Fix (CursorHere (Fix jExpr@(Expr rule _ _)))) = renderExpr `traverse` jExpr >>= renderExprHelper rule ["SelectInner"] []