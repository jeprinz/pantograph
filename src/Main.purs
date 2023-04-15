module Main where

import Prelude
import Data.Cleat (Expr(..))
import Data.Const (Const(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let
    e1 = Expr { pre: "1", left: Const unit, tan: unit, right: mempty }

    e2 = Expr { pre: "2", left: Const unit, tan: unit, right: mempty }

    e3 = Expr { pre: "3", left: Const unit, tan: unit, right: List.fromFoldable (Just <$> [ e1, e2 ]) }

    e4 =
      Expr
        { pre: "4"
        , left: Const unit
        , tan: unit
        , right: List.fromFoldable (Just <$> [ e3, e3 ])
        }
  traverse_ log e4
