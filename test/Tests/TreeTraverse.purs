module Tests.TreeTraverse where

import Data.Tree
import Data.Tree.Traverse
import Data.Tuple.Nested
import Prelude
import Prelude
import Util
import Util

import Data.Array as Array
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
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

main :: Effect Unit
main = do
  let t s = Tree (N s)
  let th s i ts = Tooth (N s) (i /\ ts)

  let g = CursorGyro $ Cursor 
        -- { outside: Path $ List.fromFoldable [th "a" 1 [t "a0" [], t "a2" []], th "b" 1 [t "b0" [], t "b2" []]]
        { outside: Path $ List.fromFoldable [th "1" 0 [], th "2" 0 [], th "3" 0 [], th "4" 0 []]
        , inside: t "inside" []
        , orientation: Outside }

  debugM ("[main] g  = " <> pretty g) {}

  g' <- g # traverseGyro \{inside: Tree n _} -> pure n

  debugM ("[main] g' = " <> pretty g') {}

  if (g == g')
    then debugM "EQUAL" {}
    else debugM "NOT EQUAL" {}

  pure unit

