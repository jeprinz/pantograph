module Pantograph.Generic.Language.Step where

import Data.Either.Nested
import Data.Match
import Data.Tree
import Data.Tuple
import Data.Tuple.Nested
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Language
import Prelude

import Bug (bug)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tree.Change (lub')
import Pantograph.Generic.Language.Step.Pattern as P
import Record as R
import Text.Pretty (pretty, ticks)
import Type.Proxy (Proxy(..))
import Util (findMapM, fromJust, fromJust', fromRight, fromRight', uP)

-- utilities

getStepExprSort :: forall sn el. Language sn el => StepExpr sn el -> Sort sn
getStepExprSort (Boundary dir ch kid) = 
  let {left, right} = endpoints ch in
  case dir of
    Up -> left
    Down -> right
getStepExprSort (StepExpr _ node _) =
  getExprNodeSort node

-- toStepExpr

class ToStepExpr a sn el | a -> sn el where
  toStepExpr :: a -> StepExpr sn el

instance ToStepExpr (AnnExpr sn el r) sn el where
  toStepExpr (Tree node kids) = StepExpr Nothing (shrinkAnnExprNode node) (toStepExpr <$> kids)

instance ToStepExpr (AnnExprCursor sn el r) sn el where
  toStepExpr (Cursor cursor) =
    wrapExprPath (shrinkAnnExprPath cursor.outside) $ 
    modifyMarker (const $ Just $ CursorMarker cursor.orientation) $
    toStepExpr cursor.inside

-- fromStepExpr

fromStepExpr :: forall sn el. PrettyTreeNode el => PrettyTreeNode sn => StepExpr sn el -> ExprCursor sn el \/ Expr sn el
fromStepExpr e0 = go e0
  where
  go :: StepExpr sn el -> ExprCursor sn el \/ Expr sn el
  go (Boundary _ _ _) = bug $ "encountered a `Boundary` during `fromStepExpr`: " <> pretty e0
  go (StepExpr (Just (CursorMarker orientation)) node kids) =
    let kids' = kids <#> go >>> fromRight' ("encountered multiple cursors during `fromStepExpr`: " <> pretty e0) in
    Left $ Cursor {outside: mempty, inside: Tree node kids', orientation}
  go (StepExpr Nothing node kids) =
    let
      f = case _ of
        Nothing /\ kids' -> case _ of
          i /\ Left cursor -> Just (i /\ cursor) /\ kids'
          _ /\ Right kid' -> Nothing /\ Array.cons kid' kids'
        Just i_cursor /\ kids' -> case _ of
          _ /\ Left _cursor -> bug $ "encountered multiple cursors during `fromStepExpr`: " <> pretty e0
          _ /\ Right kid' -> Just i_cursor /\ Array.cons kid' kids'
      maybe_i_cursor /\ kids = Array.foldr (flip f) (Nothing /\ []) $ Array.mapWithIndex Tuple $ go <$> kids
    in
    case maybe_i_cursor of
      Just (i /\ Cursor cursor) -> Left $ Cursor {outside: consPath cursor.outside (Tooth node i kids), inside: cursor.inside, orientation: cursor.orientation}
      Nothing -> Right $ Tree node kids


-- manipulate StepExpr

wrapExprPath :: forall sn el. ExprPath sn el -> StepExpr sn el -> StepExpr sn el
wrapExprPath p = case unconsPath p of
  Nothing -> identity
  Just {outer, inner} -> wrapExprPath outer <<< wrapExprTooth inner

wrapExprTooth :: forall sn el. ExprTooth sn el -> StepExpr sn el -> StepExpr sn el
wrapExprTooth (Tooth node i kids) e = StepExpr Nothing node (fromJust' "wrapExprTooth" $ Array.insertAt i e $ toStepExpr <$> kids)

modifyMarker :: forall sn el. (Maybe Marker -> Maybe Marker) -> StepExpr sn el -> StepExpr sn el
modifyMarker f = case _ of
  (Boundary dir ch kid) -> Boundary dir ch $ modifyMarker f kid
  (StepExpr maybeMarker node kids) -> StepExpr (f maybeMarker) node kids

boundary direction change kid = Boundary direction change kid

-- setup SteExpr

setupInsert :: forall sn el.
  { outside :: ExprPath sn el
  , outerChange :: SortChange sn
  , middle :: ExprNonEmptyPath sn el
  , innerChange :: SortChange sn
  , inside :: Expr sn el
  , orientation :: Orientation } ->
  StepExpr sn el
setupInsert args =
  wrapExprPath args.outside $
  boundary Up args.outerChange $
  wrapExprPath (toPath args.middle) $
  boundary Down args.innerChange $
  toStepExpr args.inside

setupReplace :: forall sn el.
  { outside :: ExprPath sn el
  , outerChange :: SortChange sn
  , inside :: Expr sn el } ->
  StepExpr sn el
setupReplace args = 
  wrapExprPath args.outside $
  boundary Up args.outerChange $
  toStepExpr args.inside

-- stepping engine

type StepM sn el = ReaderT (Array (SteppingRule sn el)) Identity

runStepM :: forall sn el a. Eq sn => Eq el => Show sn => PrettyTreeNode sn => 
  Array (SteppingRule sn el) -> StepM sn el a -> a
runStepM rules = flip runReaderT (builtinRules <> rules) >>> unwrap 

runStepExpr :: forall sn el. Language sn el =>
  StepExpr sn el ->
  Maybe (ExprGyro sn el)
runStepExpr stepExpr =
  let stepExpr' = runStepM steppingRules $ stepFixpoint stepExpr in
  case fromStepExpr stepExpr' of
    Left cursor -> Just $ CursorGyro cursor
    Right expr -> Just $ RootGyro expr

-- | Attempts a single step.
step :: forall sn el. StepExpr sn el -> StepM sn el (Maybe (StepExpr sn el))
step e = findMapM (pure <<< flip applySteppingRule e) =<< ask

-- | Fixpoint of stepping.
stepFixpoint :: forall sn el. StepExpr sn el -> StepM sn el (StepExpr sn el)
stepFixpoint e = step e >>= maybe (pure e) stepFixpoint

-- builtin SteppingRules

builtinRules =
  [ passThroughRule ]
--   , combineUpRule
--   , combineDownRule ]

passThroughRule :: forall sn el. Eq sn => PrettyTreeNode sn => SteppingRule sn el
passThroughRule = SteppingRule case _ of
  Boundary Down down (Boundary Up up kid) -> Just
    let hypotenuse = lub' down up in
    let up' = invert down <> hypotenuse in
    let down' = invert up <> hypotenuse in
    Boundary Up up' (Boundary Down down' kid)
  _ -> Nothing

-- passThroughRule = buildSteppingRule
--   (P.boundary Down (var "down") (P.boundary Up (var "up") (var "kid"))) $
--   uP \[Right (Left down), Right (Left up), Left kid] -> Just $
--   let hypotenuse = lub' down up in
--   let up' = invert down <> hypotenuse in
--   let down' = invert up <> hypotenuse in
--   (boundary Up up' $ boundary Down down' kid)

-- combineDownRule = buildSteppingRule
--   (P.boundary Down (var "c1") (P.boundary Down (var "c2") (var "kid"))) $
--   uP \[Right (Left c1), Right (Left c2), Left kid] -> Just
--   (boundary Down (c1 <> c2) kid)

-- combineUpRule = buildSteppingRule
--   (P.boundary Up (var "c1") (P.boundary Up (var "c2") (var "kid"))) $
--   uP \[Right (Left c1), Right (Left c2), Left kid] -> Just
--   (boundary Up (c1 <> c2) kid)

-- -- SteppingRule builder

-- buildSteppingRule :: forall sn el. Eq sn => Eq el => Show sn =>
--   StepExprPattern sn el ->
--   (Array (StepExprMatch sn el Void) -> Maybe (StepExpr sn el)) ->
--   SteppingRule sn el
-- buildSteppingRule pat k = SteppingRule $ case_ 
--   [ pat /\ Just <<< k
--   , wild /\ Just <<< const Nothing ]
