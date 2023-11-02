module Test.Tests.TreeSwivel where

import Data.Tree
import Data.Tree.Swivel
import Data.Tuple.Nested
import Prelude hiding ((/))

import Data.Array as Array
import Data.List as List
import Effect (Effect)
import Effect.Class.Console as Console
import Text.Pretty (class Pretty, pretty)

newtype N = N String

derive newtype instance Eq N
instance Show N where show (N string) = string
instance Pretty N where pretty = show

instance TreeNode N where
  validKidsCount _ _ = true

instance PrettyTreeNode N where
  prettyTreeNode (N s) [] = s
  prettyTreeNode (N s) ss = "(" <> s <> " " <> Array.intercalate " " ss <> ")"

mkTree s = Tree (N s)
infix 1 mkTree as /

mkTooth s = Tooth (N s)
infix 1 mkTooth as /-

main :: Effect Unit
main = do
  let p = Path $ List.fromFoldable $
        [ "A" /- 1 /\ ["A₀"/[], "A₂"/[]]
        , "B" /- 1 /\ ["B₀"/[], "B₂"/[]]
        , "C" /- 1 /\ ["C₀"/[], "C₂"/[]]
        ]
  let t = "D"/["D₀"/[], "D₁"/[]]
  let c = Cursor {outside: p, inside: t, orientation: Outside}
  Console.log $ pretty $ c
  Console.log "====[ swivelPrev ]===="
  Console.log $ pretty $ swivelPrev $ c
  Console.log "====[ swivelNext ]===="
  Console.log $ pretty $ swivelNext $ c

  -- Console.log "====[ swivelDownAt 0 ]===="
  -- Console.log $ "getLeftIndex = " <> pretty (getLeftIndex c)
  -- Console.log $ "getRightIndex = " <> pretty (getRightIndex c)
  -- Console.log $ "getDownLeftmostIndex = " <> pretty (getDownLeftmostIndex c)
  -- Console.log $ "getDownRightmostIndex = " <> pretty (getDownRightmostIndex c)
  -- Console.log "====[ swivelDownLeftmost ]===="
  -- Console.log $ pretty $ swivelDownLeftmost $ c
  -- Console.log "====[ swivelDownRightmost ]===="
  -- Console.log $ pretty $ swivelDownRightmost $ c
  pure unit


