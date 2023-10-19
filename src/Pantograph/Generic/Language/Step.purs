module Pantograph.Generic.Language.Step where

import Data.Either.Nested
import Data.Tree
import Data.Tuple
import Data.Tuple.Nested
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Language
import Prelude

import Bug (bug)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Hole (hole)
import Record as R
import Type.Proxy (Proxy(..))
import Util (fromJust', fromRight')

-- utilities

getStepExprSort :: forall sn el. Language sn el -> StepExpr sn el -> Sort sn
getStepExprSort _ (Boundary boundary) = 
  let {left, right} = endpoints boundary.change in
  case boundary.direction of
    Up -> left
    Down -> right
getStepExprSort language (InjectStepExpr {node}) =
  getExprNodeSort language node

-- toStepExpr

class ToStepExpr a sn el | a -> sn el where
  toStepExpr :: a -> StepExpr sn el

instance ToStepExpr (AnnExpr sn el r) sn el where
  toStepExpr (Tree tree) = InjectStepExpr {node: shrinkAnnExprNode tree.node, maybeMarker: Nothing, kids: toStepExpr <$> tree.kids}

instance ToStepExpr (AnnExprCursor sn el r) sn el where
  toStepExpr (Cursor cursor) =
    wrapExprPath (shrinkAnnExprPath cursor.outside) $ 
    modifyMarker (const $ Just $ CursorMarker cursor.orientation) $
    toStepExpr cursor.inside

-- fromStepExpr

fromStepExpr :: forall sn el. StepExpr sn el -> ExprCursor sn el \/ Expr sn el
fromStepExpr (Boundary _boundary) = bug $ "encountered a `Boundary` during `fromStepExpr`"
fromStepExpr (InjectStepExpr {node, kids}) =
  let
    f = case _ of
      Nothing /\ kids' -> case _ of
        i /\ Left cursor -> Just (i /\ cursor) /\ kids'
        _ /\ Right kid' -> Nothing /\ Array.cons kid' kids'
      Just i_cursor /\ kids' -> case _ of
        _ /\ Left _cursor -> bug $ "encountered multiple cursors during `fromStepExpr`"
        _ /\ Right kid' -> Just i_cursor /\ Array.cons kid' kids'
    maybe_i_cursor /\ kids = Array.foldr (flip f) (Nothing /\ []) $ Array.mapWithIndex Tuple $ fromStepExpr <$> kids
  in
  case maybe_i_cursor of
    Just (i /\ Cursor cursor) -> Left $ Cursor {outside: consPath cursor.outside (Tooth {node, i, kids}), inside: cursor.inside, orientation: cursor.orientation}
    Nothing -> Right $ Tree {node, kids}

-- manipulate StepExpr

wrapExprPath :: forall sn el. ExprPath sn el -> StepExpr sn el -> StepExpr sn el
wrapExprPath p = case unconsPath p of
  Nothing -> identity
  Just {outer, inner} -> wrapExprPath outer <<< wrapExprTooth inner

wrapExprTooth :: forall sn el. ExprTooth sn el -> StepExpr sn el -> StepExpr sn el
wrapExprTooth (Tooth {node, i, kids}) e = InjectStepExpr {maybeMarker: Nothing, node, kids: fromJust' "wrapExprTooth" $ Array.insertAt i e $ toStepExpr <$> kids}

modifyMarker :: forall sn el. (Maybe Marker -> Maybe Marker) -> StepExpr sn el -> StepExpr sn el
modifyMarker f = case _ of
  (Boundary boundary) -> Boundary $ R.modify (Proxy :: Proxy "kid") (modifyMarker f) boundary
  (InjectStepExpr expr) -> InjectStepExpr $ R.modify (Proxy :: Proxy "maybeMarker") f expr

wrapBoundary direction change kid = Boundary {direction, change, kid}

-- setup SteExpr

setupInsert :: forall sn el.
  { outside :: ExprPath sn el
  , outerChange :: SortChange sn
  , middle :: ExprPath sn el
  , innerChange :: SortChange sn
  , inside :: Expr sn el
  , orientation :: Orientation } ->
  StepExpr sn el
setupInsert args =
  wrapExprPath args.outside $
  wrapBoundary Up args.outerChange $
  wrapExprPath args.middle $
  wrapBoundary Down args.innerChange $
  toStepExpr args.inside

setupReplace :: forall sn el.
  { outside :: ExprPath sn el
  , change :: SortChange sn
  , inside :: Expr sn el } ->
  StepExpr sn el
setupReplace args = 
  wrapExprPath args.outside $
  wrapBoundary Up args.change $
  toStepExpr args.inside

-- stepping engine

type StepM sn el = ReaderT (List (SteppingRule sn el)) Identity

runStepM :: forall sn el a. List (SteppingRule sn el) -> StepM sn el a -> a
runStepM rules = flip runReaderT (builtinRules <> rules) >>> unwrap

-- | Attempts a single step.
step :: forall sn el. StepExpr sn el -> StepM sn el (Maybe (StepExpr sn el))
step = hole "TODO"

-- | Fixpoint of stepping.
stepFixpoint :: forall sn el. StepExpr sn el -> StepM sn el (StepExpr sn el)
stepFixpoint e = step e >>= case _ of
  Nothing -> pure e
  Just e' -> stepFixpoint e'

-- builtin SteppingRules

builtinRules = List.fromFoldable 
  [passThroughRule, combineUpRule, combineDownRule]

passThroughRule :: forall sn el. SteppingRule sn el
passThroughRule = hole "TODO"

combineUpRule :: forall sn el. SteppingRule sn el
combineUpRule = hole "TODO"

combineDownRule :: forall sn el. SteppingRule sn el
combineDownRule = hole "TODO"

-- SteppingRule builder

-- buildSteppingRule :: forall sn el.
