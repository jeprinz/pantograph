module Test.Main where

import Data.Tuple.Nested
import Prelude
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Text.Pretty (class Pretty, parens, pretty)

main :: Effect Unit
main = log "write a REAL test framework"

-- data L = Base | Wrap String

-- derive instance Generic L _
-- derive instance Eq L
-- derive instance Ord L
-- instance Show L where show x = genericShow x
-- instance Pretty L where
--   pretty Base = "base"
--   pretty (Wrap str) = "wrap " <> str
-- instance IsExprLabel L where
--   prettyExprF'_unsafe (Base /\ []) = "point"
--   prettyExprF'_unsafe (Wrap str /\ [kid]) = parens $ "wrap " <> str <> " " <> kid
--   expectedKidsCount Base = 0
--   expectedKidsCount (Wrap _) = 1

-- main :: Effect Unit
-- main = do
--   -- let
--   --   pathUp :: Path Up L
--   --   pathUp = Path $ List.fromFoldable 
--   --     [ Tooth (Wrap "bot") mempty
--   --     , Tooth (Wrap "top") mempty
--   --     ]

--   --   pathDown :: Path Down L
--   --   pathDown = Path $ List.fromFoldable 
--   --     [ Tooth (Wrap "top") mempty
--   --     , Tooth (Wrap "bot") mempty
--   --     ]

--   -- log "pathUp:"
--   -- log $ pretty pathUp
--   -- log $ foldMapPath "PathEnd" (\th -> ((pretty th <> "  -  " )<> _)) pathUp
  
--   -- log ""

--   -- log "pathDown:"
--   -- log $ pretty pathDown
--   -- log $ foldMapPath "PathEnd" (\th -> ((pretty th <> "  -  ") <> _)) pathDown

--   log $ pretty $ 
--     Expr (Inject (Wrap "A")) [Expr (Plus (Tooth (Wrap "B") mempty)) [Expr (Inject Base) []]]
