module Language.Pantograph.Generic.Edit where

import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Smallstep
import Language.Pantograph.Generic.Unification
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertI, just)
import Control.Plus (empty)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Expr ((%),(%<))
import Data.Expr as Expr
import Data.Lazy (Lazy, defer)
import Data.List as List
import Data.List.Zip as ZipList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Hole (hole)
import Text.Pretty (pretty)
import Type.Direction (Up)
import Debug (traceM)
import Util (fromJust')

--------------------------------------------------------------------------------
-- Edit, Action
--------------------------------------------------------------------------------

type Edit l r =
  { label :: String
  , action :: Lazy (Action l r)
  }

data Action l r
  -- = SetCursorAction (Lazy (DerivZipper l r))
  -- | SetSSTermAction (Lazy (SSTerm l r))
  = FillAction {sub :: Sub (SortLabel l), dterm :: DerivTerm l r}
  | ReplaceAction {topChange :: SortChange l, dterm :: DerivTerm l r}
  | WrapAction {topChange :: SortChange l, dpath :: DerivPath Up l r, botChange :: SortChange l,
        sub :: Sub (SortLabel l), cursorGoesInside :: Boolean}

newTermFromRule :: forall l r. IsRuleLabel l r => r -> DerivTerm l r
newTermFromRule r = do
    let Rule mvars hyps' _con = TotalMap.lookup r language
    let sigma = freshenRuleMetaVars mvars
    let hyps = Expr.subMetaExprPartially sigma <$> hyps'
    let term1 = DerivLabel r sigma % (map (fromJust' "yes" <<< defaultDerivTerm) hyps)
    let sub = fromJust' "ntfr" $ infer term1
    subDerivTerm sub term1

newPathFromRule :: forall l r. IsRuleLabel l r => r -> Int -> DerivPath Up l r /\ Sort l
newPathFromRule r kidIx = do
  let tooth /\ sub = newToothFromRule r kidIx
  Expr.Path (List.singleton tooth) /\ sub

newToothFromRule :: forall l r. IsRuleLabel l r => r -> Int -> DerivTooth l r /\ Sort l
newToothFromRule r kidIx = do
  let Rule mvars hyps' _con = TotalMap.lookup r language
  let sigma = freshenRuleMetaVars mvars
  let hyps = Expr.subMetaExprPartially sigma <$> hyps'

  -- `hypSort` is the sort of what should got at position `kidIx`
  let hypSortPath /\ hypSort = assertI $ just "newPathFromRule.hpySortPath" $
        ZipList.zipAt kidIx (List.fromFoldable hyps)

  -- Each kid of the tooth is a default deriv
  let defaultHypDerivPath :: _ (DerivTerm l r)
      defaultHypDerivPath = assertI $ just "newPathFromRule.defaultHypDerivPath" $
        sequence (defaultDerivTerm <$> hypSortPath)

  -- Some of the children might have more specialized types, so we need to unify by calling infer. (e.g. in lambda, we call defaultDerivTerm on sort (Name ?x), but we actually get something of sort (Name ""))
  let tooth = (DerivLabel r sigma %< defaultHypDerivPath)
  let path1 = Expr.Path (List.singleton tooth)
  let sub = fromJust' "path didn't typecheck in newPathFromRule" $ inferPath (freshMetaVarSort "pathInside") path1
  let toothSubbed = subDerivTooth sub tooth
  toothSubbed /\ Expr.subMetaExprPartially sub  hypSort
