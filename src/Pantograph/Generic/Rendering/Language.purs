module Pantograph.Generic.Rendering.Language where

import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Prelude
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.Utilities as HU

-- renderExpr :: forall ctx env r n n'. Rendering r n n' =>
--   Expr r n' (Sort n) ->
--   RenderM ctx env r n (Html r n /\ Expr r (RenderNode n') (Sort n))
-- renderExpr (Expr (ExprNode r n sigma) kids) = do
--   let elemId = HU.freshElementId unit

--   let renderKids = Array.mapWithIndex Tuple kids <#> \(i /\ e) -> do
--         html <- renderExpr e
--         pure $ ((i /\ html) /\ e)
--   nodes <- arrangeExpr (ExprNode r n sigma) renderKids

--   let htmls = nodes # Array.foldMap case _ of
--         Left (_ /\ html /\ _) -> [html]
--         Right htmls' -> htmls'
--   let html = HH.div 
--         [ HU.id elemId
--           -- TODO: other attrs
--         ]
--         htmls

--   let kids' = nodes # Array.foldMap case _ of
--         Left (_ /\ _ /\ kid') -> pure kid'
--         Right _ -> []
--   let rend = Expr (ExprNode r (RenderNode elemId n) sigma) kids'

--   pure $ html /\ rend

