module Pantograph.Generic.Language.Smallstep where

import Pantograph.Generic.Language.Common
import Prelude

import Bug (bug)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tree (class PrettyTreeNode, class TreeNode, Change(..), Path(..), Tooth(..), Tree(..))
import Data.Tree as Tree
import Data.Tree.Change (endpoints)
import Hole (hole)
import Pantograph.Generic.Language (getExprNodeSort)
import Text.Pretty (class Pretty, braces, pretty)
import Util (fromJust')

-- -- SsExpr

-- data SsExpr sn el
--   = Boundary {direction :: Direction, change :: SortChange sn, kid :: SsExpr sn el}
--   | Inject {expr :: ExprNode ExprNode, maybeMarker :: Maybe Marker, kids :: Array (SsExpr sn el)}

-- type SsExpr sn el = Tree (SsExprNode sn el)

-- data SsExprNode (sn :: Type) (el :: Type)
--   = InjectSsExprNode {expr :: ExprNode sn el, maybeMarker :: Maybe Marker}
--   | BoundarySsExprNode {direction :: Direction, change :: SortChange sn}

-- derive instance Generic (SsExprNode sn el) _
-- instance (Show sn, Show el) => Show (SsExprNode sn el) where show x = genericShow x
-- instance (Eq sn, Eq el) => Eq (SsExprNode sn el) where eq x = genericEq x

-- instance TreeNode el => TreeNode (SsExprNode sn el) where
--   kidsCount (InjectSsExprNode {expr}) = Tree.kidsCount expr
--   kidsCount (BoundarySsExprNode _) = 1
--   -- kidsCount (MarkerSsExprNode n) = n

-- instance (Show sn, Show el, PrettyTreeNode el) => PrettyTreeNode (SsExprNode sn el) where
--   prettyTreeNode (InjectSsExprNode {expr}) kids = Tree.prettyTreeNode expr kids
--   prettyTreeNode (BoundarySsExprNode boundary) [kid] = pretty boundary.direction <> braces kid
--   -- prettyTreeNode (MarkerSsExprNode _) [] = "⌶"
--   prettyTreeNode node kids = bug $ "prettyTreeNode: `node` should not have this many `kids`; where `node = " <> show node <> "`, `kids = " <> show kids <> "`"

-- data Marker = CursorMarker

-- derive instance Generic Marker _
-- instance Show Marker where show x = genericShow x
-- instance Eq Marker where eq x = genericEq x

-- makeSsTerm label sigma kids = Tree {node: InjectSsExprNode {expr: AnnExprNode {label, sigma}, maybeMarker: Nothing}, kids}

-- getSsExprSort language (Tree {node: (InjectSsExprNode {expr})}) = getExprNodeSort language expr
-- -- getSsExprSort _ t@(Tree {node: MarkerSsExprNode _}) = bug $ "Can't `getSsExprSort` of a maybeMarker: " <> show t
-- getSsExprSort _ (Tree {node: (BoundarySsExprNode boundary)}) =
--   case boundary.direction of
--     Up -> (endpoints boundary.change).left
--     Down -> (endpoints boundary.change).right

-- eraseMarkers = map case _ of
--   InjectSsExprNode {expr} -> InjectSsExprNode {expr, maybeMarker: Nothing}
--   node -> node

-- -- SsRule

-- type SsRule sn el = SsExpr sn el -> Maybe (SsExpr sn el)

-- -- Code for converting `Expr`, `ExprPath` to and from `SsExpr`

-- fromExpr :: forall sn el. Expr sn el -> SsExpr sn el
-- fromExpr = map (InjectSsExprNode <<< {expr: _, maybeMarker: Nothing})

-- fromExprGyro :: forall sn el. ExprGyro sn el -> SsExpr sn el
-- fromExprGyro = hole "TODO"

-- -- toExpr :: forall sn el. Show sn => SsExpr sn el -> String \/ Expr sn el
-- -- toExpr = traverse case _ of
-- --   (InjectSsExprNode node) -> pure node
-- --   (BoundarySsExprNode boundary) -> throwError $ "boundary: " <> show boundary
-- --   (MarkerSsExprNode n) -> throwError $ "maybeMarker (with " <> show n <> " kids)"

-- toExprGyro :: forall sn el. Show sn => SsExpr sn el -> String \/ ExprGyro sn el
-- toExprGyro = hole "TODO"
-- -- toExprGyro (Tree {node, kids}) = do
-- --   kids' <- toExprGyro `traverse` kids
-- --   case node of
-- --     (InjectSsExprNode node) -> ?a
-- --     (BoundarySsExprNode boundary) -> ?a
-- --     (MarkerSsExprNode n) -> ?a

-- unTooth (Tooth {node: expr, i, kids}) kid = Tree {node: InjectSsExprNode {expr, maybeMarker: Nothing}, kids: fromJust' "unTooth" $ Array.insertAt i kid $ map fromExpr kids}

-- unPath (Path Nil) = identity
-- unPath (Path (Cons th ths)) = unPath (Path ths) <<< unTooth th

-- enBoundary direction change kid = Tree {node: BoundarySsExprNode {direction, change}, kids: [kid]}

-- -- setups

-- -- TODO: insert marker

-- setupInsert :: forall sn el.
--   { outside :: ExprPath sn el
--   , outerChange :: SortChange sn
--   , middle :: ExprPath sn el
--   , innerChange :: SortChange sn
--   , inside :: Expr sn el } ->
--   SsExpr sn el
-- setupInsert args =
--   unPath args.outside $
--   enBoundary Up args.outerChange $
--   unPath args.middle $
--   enBoundary Down args.innerChange $
--   fromExpr args.inside

-- setupReplace :: forall sn el.
--   { outside :: ExprPath sn el
--   , change :: SortChange sn
--   , inside :: Expr sn el } ->
--   SsExpr sn el
-- setupReplace args = 
--   unPath args.outside $
--   enBoundary Up args.change $
--   fromExpr args.inside

-- -- smallstep engine

-- type SmallstepM sn el = ReaderT (List (SsRule sn el)) Identity

-- runSmallstepM :: forall sn el a. List (SsRule sn el) -> SmallstepM sn el a -> a
-- runSmallstepM rules = flip runReaderT (builtinRules <> rules) >>> unwrap

-- -- | Attempts a single step.
-- step :: forall sn el. SsExpr sn el -> SmallstepM sn el (Maybe (SsExpr sn el))
-- step = hole "TODO"

-- -- | Fixpoint of stepping.
-- stepFixpoint :: forall sn el. SsExpr sn el -> SmallstepM sn el (SsExpr sn el)
-- stepFixpoint e = step e >>= case _ of
--   Nothing -> pure e
--   Just e' -> stepFixpoint e'

-- -- built-in SsRules

-- builtinRules = List.fromFoldable 
--   [passThroughRule, combineUpRule, combineDownRule]

-- passThroughRule :: forall sn el. SsRule sn el
-- passThroughRule e = hole "TODO"

-- combineUpRule :: forall sn el. SsRule sn el
-- combineUpRule e = hole "TODO"

-- combineDownRule :: forall sn el. SsRule sn el
-- combineDownRule e = hole "TODO"
