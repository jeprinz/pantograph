module Main where

import Data.Tree
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.List as List
import Data.Tree.Traverse (traverseCursor)
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Pantograph.Generic.Rendering as PR
import Pantograph.Specific.FSTLC as FSTLC
import Pantograph.Specific.LC as LC
import Text.Pretty (pretty)
import Util (debug)

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  FSTLC.runEditor (PR.EditorOptions {verbosity: 0})

-- main :: Effect Unit
-- main = do
--   let
--     th h = Tooth (Cons h) (0 /\ [])
--     cons h t = Tree (Cons h) [t]
--     nil = Tree Nil []
--     _ = 
--       traverseCursor
--         (\{outside, middle, inside} -> debug "main" {outside: pretty outside, middle: pretty middle, inside: pretty inside} \_ -> unit)
--         (Cursor 
--           { outside: Path $ List.fromFoldable 
--               [th "C", th "B", th "A"]
--           , inside:
--               nil
--           , orientation: Outside })
--   pure unit

-- data ListNode = Cons String | Nil

-- instance TreeNode ListNode where
--   kidsCount (Cons _) = 1
--   kidsCount Nil = 0

-- instance PrettyTreeNode ListNode where
--   prettyTreeNode (Cons s) [a] = s <> ", " <> a
--   prettyTreeNode Nil [] = "[]"
--   prettyTreeNode _ _ = bug "invalid"
