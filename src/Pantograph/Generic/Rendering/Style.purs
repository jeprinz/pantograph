module Pantograph.Generic.Rendering.Style where

import Prelude

import Data.Tree (Orientation(..))
import Halogen.HTML as HH

className =
  { expr: HH.ClassName "Expr"
  , previewExpr: HH.ClassName "PreviewExpr"
  , toolboxExpr: HH.ClassName "ToolboxExpr"
  , outsideCursor, insideCursor, orientationCursor
  , outsideSelect, insideSelect, orientationSelect
  }
  where
  outsideCursor = HH.ClassName "OutsideCursor"
  insideCursor = HH.ClassName "InsideCursor"
  orientationCursor = case _ of
    Outside -> outsideCursor
    Inside -> insideCursor

  outsideSelect = HH.ClassName "OutsideSelect"
  insideSelect = HH.ClassName "InsideSelect"
  orientationSelect = case _ of
    Outside -> outsideSelect
    Inside -> insideSelect
