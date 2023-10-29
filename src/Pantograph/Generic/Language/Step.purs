module Pantograph.Generic.Language.Step where

import Data.Either.Nested
import Data.Tree
import Data.Tuple
import Data.Tuple.Nested
import Pantograph.Generic.Language.Common
import Pantograph.Generic.Language.Language
import Prelude
import Util

import Bug (bug)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tree.Change (lub')
import Debug as Debug
import Pantograph.Generic.Rendering.Terminal.TerminalItems as TerminalItems
import Text.Pretty (pretty, (<+>))
import Todo (todo)

-- utilities

getStepExprSort :: forall sn el. Language sn el => StepExpr sn el -> Sort sn
getStepExprSort (StepExpr node _) = getExprNodeSort node
getStepExprSort (Boundary (dir /\ ch) kid) = 
  let {left, right} = endpoints ch in
  case dir of
    Up -> left
    Down -> right
getStepExprSort (Marker e) = getStepExprSort e

-- toStepExpr

class ToStepExpr a sn el | a -> sn el where
  toStepExpr :: a -> StepExpr sn el

instance ToStepExpr (AnnExpr sn el r) sn el where
  toStepExpr (Tree node kids) = StepExpr (shrinkAnnExprNode node) (toStepExpr <$> kids)

instance ToStepExpr (AnnExprCursor sn el r) sn el where
  toStepExpr (Cursor cursor) =
    wrapExprPath (shrinkAnnExprPath cursor.outside) $ 
    marker $
    toStepExpr cursor.inside

-- fromStepExpr

-- fromStepExpr :: forall sn el. Language sn el => StepExpr sn el -> ExprCursor sn el \/ Expr sn el
-- fromStepExpr e0 = go mempty e0
--   where
--   goExpr :: StepExpr sn el -> Expr sn el
--   goExpr (Boundary _ _ _) = bug $ "encountered a `Boundary` during `fromStepExpr`: " <> pretty e0
--   goExpr (StepExpr (Just _) _ _) = bug $ "encountered multiple `Marker`s during `fromStepExpr`: " <> pretty e0
--   goExpr (StepExpr Nothing node kids) = Tree node (kids <#> goExpr)

--   go :: ExprPath sn el -> StepExpr sn el -> ExprCursor sn el \/ Expr sn el
--   go _ (Boundary _ _ _) = bug $ "encountered a `Boundary` during `fromStepExpr`: " <> pretty e0
--   go outside (StepExpr (Just (CursorMarker orientation)) node kids) = Left $ Cursor {outside, inside: Tree node (kids <#> goExpr), orientation}
--   go outside (StepExpr Nothing node kids) = 
--     let
--       f = case _ of
--         Nothing /\ kids' -> case _ of
--           i /\ Left cursor -> Just (i /\ cursor) /\ kids'
--           _ /\ Right kid' -> Nothing /\ Array.cons kid' kids'
--         Just i_cursor /\ kids' -> case _ of
--           _ /\ Left _cursor -> bug $ "encountered multiple cursors during `fromStepExpr`: " <> pretty e0
--           _ /\ Right kid' -> Just i_cursor /\ Array.cons kid' kids'
--       maybe_i_cursor /\ kids = Array.foldr (flip f) (Nothing /\ []) $ Array.mapWithIndex Tuple $ tooths (Tree ?a ?a) <#> ?a -- go <$> kids
--     in
--     case maybe_i_cursor of
--       Just (i /\ Cursor cursor) -> Left $ Cursor {outside: consPath cursor.outside (Tooth node i kids), inside: cursor.inside, orientation: cursor.orientation}
--       Nothing -> Right $ Tree node kids

fromStepExpr :: forall sn el. Language sn el => StepExpr sn el -> ExprCursor sn el \/ Expr sn el
fromStepExpr e0 = case go e0 of
    Left {tooths, inside, orientation} -> Left (Cursor {outside: Path (List.reverse tooths), inside, orientation})
    Right expr -> Right expr
  where
  goExpr :: StepExpr sn el -> Expr sn el
  goExpr (Boundary _ _) = bug $ "encountered a `Boundary` during `fromStepExpr`: " <> pretty e0
  goExpr (Marker _) = bug $ "encountered multiple `Marker`s during `fromStepExpr`: " <> pretty e0
  goExpr (StepExpr node kids) = Tree node (kids <#> goExpr)

  go :: StepExpr sn el -> {tooths :: List (ExprTooth sn el), inside :: Expr sn el, orientation :: Orientation} \/ Expr sn el
  go (Boundary _ _) = bug $ "encountered a `Boundary` during `fromStepExpr`: " <> pretty e0
  go (Marker e) = Left $ {tooths: mempty, inside: goExpr e, orientation: Outside}
  go (StepExpr node kids) =
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
      Just (i /\ {tooths, inside, orientation}) -> Left $ {tooths: List.Cons (Tooth node (i /\ kids)) tooths, inside, orientation}
      Nothing -> Right $ Tree node kids

-- manipulate StepExpr

wrapExprPath :: forall sn el. ExprPath sn el -> StepExpr sn el -> StepExpr sn el
wrapExprPath p = case unconsPath p of
  Nothing -> identity
  Just {outer, inner} -> wrapExprPath outer <<< wrapExprTooth inner

wrapExprTooth :: forall sn el. ExprTooth sn el -> StepExpr sn el -> StepExpr sn el
wrapExprTooth (Tooth node (i /\ kids)) e = StepExpr node (fromJust' "wrapExprTooth" $ Array.insertAt i e $ toStepExpr <$> kids)

marker :: forall sn el. StepExpr sn el -> StepExpr sn el
marker e = Marker e

boundary :: forall sn el. Direction -> Change (SortNode sn) -> StepExpr sn el -> StepExpr sn el
boundary dir ch kid = Boundary (dir /\ ch) kid

-- setup SteExpr

setupEdit :: forall sn el. ExprCursor sn el -> Edit sn el -> StepExpr sn el
setupEdit (Cursor cursor) (Edit edit) =
  wrapExprPath cursor.outside $
  edit.outerChange # maybe identity (boundary Up) $
  edit.middle # maybe identity (wrapExprPath <<< toPath)  $
  edit.innerChange # maybe identity (boundary Down)  $
  marker $
  toStepExpr (edit.inside # maybe cursor.inside identity)

-- stepping engine

type StepM sn el = ReaderT (Array (SteppingRule sn el)) Identity

runStepM :: forall sn el a. Language sn el =>
  Array (SteppingRule sn el) -> StepM sn el a -> a
runStepM rules = flip runReaderT (builtinRules <> rules) >>> unwrap 

runStepExpr :: forall sn el. Language sn el =>
  StepExpr sn el ->
  Maybe (ExprGyro sn el)
runStepExpr stepExpr =
  -- Debug.trace ("[step]" <+> pretty stepExpr) \_ ->
  TerminalItems.add (todo"") \_ ->
  let stepExpr' = runStepM steppingRules $ stepFixpoint stepExpr in
  case fromStepExpr stepExpr' of
    Left cursor -> Just $ CursorGyro cursor
    Right expr -> Just $ RootGyro expr

-- | Attempts a single step.
step :: forall sn el. Language sn el => StepExpr sn el -> StepM sn el (Maybe (StepExpr sn el))
step e = ask >>= findMapM (pure <<< flip applySteppingRule e) >>= case _ of
  Just e' -> pure (Just e')
  Nothing -> case e of
    StepExpr node kids -> StepExpr node <$$> stepFirstKid kids
    Boundary (dir /\ ch) e' -> Boundary (dir /\ ch) <$$> step e'
    Marker e' -> Marker <$$> step e'

stepFirstKid :: forall sn el. Language sn el => Array (StepExpr sn el) -> StepM sn el (Maybe (Array (StepExpr sn el)))
stepFirstKid kids = go 0
  where
  go i = case Array.index kids i of
    Nothing -> pure Nothing
    Just kid -> step kid >>= case _ of
      Nothing -> go (i + 1)
      Just kid' -> pure $ Just $ fromJust $ Array.modifyAt i (const kid') kids

-- | Fixpoint of stepping.
stepFixpoint :: forall sn el. Language sn el => StepExpr sn el -> StepM sn el (StepExpr sn el)
stepFixpoint e = step e >>= case _ of
  Nothing -> pure e
  Just e' -> do
    Debug.traceM $ "[step]" <+> pretty e'
    stepFixpoint e'

-- builtin SteppingRules

builtinRules :: forall sn el. Eq sn => Show sn => PrettyTreeNode sn => Array (SteppingRule sn el)
builtinRules =
  [ passThroughRule
  , combineUpRule
  , combineDownRule ]

passThroughRule :: forall el sn. Eq sn => Show sn => PrettyTreeNode sn => SteppingRule sn el
passThroughRule = SteppingRule case _ of
  Boundary (Down /\ down) (Boundary (Up /\ up) kid) -> Just
    let hypotenuse = lub' down up in
    let up' = invert down <> hypotenuse in
    let down' = invert up <> hypotenuse in
    Boundary (Up /\ up') (Boundary (Down /\ down') kid)
  _ -> Nothing

combineDownRule :: forall sn el. Eq sn => SteppingRule sn el
combineDownRule = SteppingRule case _ of
  Boundary (Down /\ down1) (Boundary (Down /\ down2) kid) -> Just $
    Boundary (Down /\ (down1 <> down2)) kid
  _ -> Nothing

combineUpRule :: forall sn el. Eq sn => SteppingRule sn el
combineUpRule = SteppingRule case _ of
  Boundary (Up /\ up1) (Boundary (Up /\ up2) kid) -> Just $
    Boundary (Up /\ (up1 <> up2)) kid
  _ -> Nothing
