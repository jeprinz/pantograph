module Pantograph.Generic.Dynamics.Run where

import Data.Display
import Data.Tree
import Pantograph.Generic.Dynamics.Common
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Text.Pretty
import Util

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Halogen.Elements as El
import Pantograph.Generic.Dynamics.BuiltinSteppingRules (builtinSteppingRules)
import Pantograph.Generic.GlobalMessageBoard as GMB

type StepM sn el = ReaderT (Array (SteppingRule sn el)) Identity

runStepM :: forall sn el a. Language sn el =>
  Array (SteppingRule sn el) -> StepM sn el a -> a
runStepM rules = flip runReaderT (builtinSteppingRules <> rules) >>> unwrap 

runStepExpr :: forall sn el ctx env. 
  Dynamics sn el ctx env =>
  StepExpr sn el ->
  Maybe (ExprGyro sn el)
runStepExpr e = do
  GMB.debugM $ El.ι [El.π "runStepExpr input", El.β [display e]]
  let e' = runStepM steppingRules $ stepFixpoint e
  GMB.debugM $ El.ι [El.π "runStepExpr output", El.β [display e']]
  cursor <- fromStepExprToExprCursor e'
  GMB.debugM $ El.ι [El.π "runStepExpr output cursor", El.β [El.τ $ pretty cursor]]
  Just $ CursorGyro cursor

-- | Attempts a single step.
step :: forall sn el ctx env. Dynamics sn el ctx env => StepExpr sn el -> StepM sn el (Maybe (StepExpr sn el))
step e = ask >>= findMapM (pure <<< flip applySteppingRule e) >>= case _ of
  Just e' -> pure (Just e')
  Nothing -> case e of
    StepExpr node kids -> StepExpr node <$$> stepFirstKid kids
    Boundary (dir /\ ch) e' -> Boundary (dir /\ ch) <$$> step e'
    Marker e' -> Marker <$$> step e'

stepFirstKid :: forall sn el ctx env. Dynamics sn el ctx env => Array (StepExpr sn el) -> StepM sn el (Maybe (Array (StepExpr sn el)))
stepFirstKid kids = go 0
  where
  go i = case Array.index kids i of
    Nothing -> pure Nothing
    Just kid -> step kid >>= case _ of
      Nothing -> go (i + 1)
      Just kid' -> pure $ Just $ fromJust $ Array.modifyAt i (const kid') kids

-- | Fixpoint of stepping.
stepFixpoint :: forall sn el ctx env. Dynamics sn el ctx env => StepExpr sn el -> StepM sn el (StepExpr sn el)
stepFixpoint e = step e >>= case _ of
  Nothing -> pure e
  Just e' -> stepFixpoint e'
