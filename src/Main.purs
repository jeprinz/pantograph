module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Pantograph.Generic.App as App
import Pantograph.Generic.Rendering as P
import Pantograph.Specific.FSTLC as FSTLC
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  Console.log "[main]"
  App.runEditor
    (Proxy :: Proxy FSTLC.SN)
    (P.EditorOptions {verbosity: 0})

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
