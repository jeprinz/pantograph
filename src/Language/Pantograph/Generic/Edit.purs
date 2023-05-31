module Language.Pantograph.Generic.Edit where

import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Smallstep
import Language.Pantograph.Generic.Unification
import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Expr as Expr
import Data.Lazy (Lazy, defer)
import Data.List as List
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.TotalMap as TotalMap
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Text.Pretty (pretty)
import Type.Direction (Up)

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
  | WrapAction {topChange :: SortChange l, dpath :: DerivPath Up l r, botChange :: SortChange l}

defaultEditsAtDerivZipper :: forall l r. IsRuleLabel l r => Sort l -> DerivZipper l r -> Array (Edit l r)
defaultEditsAtDerivZipper topSort dz =
  let dterm = Expr.zipperExpr dz in
  Array.concat $
  (case isHoleDerivTerm dterm of
    Nothing -> []
    Just _ -> []) -- !TODO digEdit dz)
  Array.:
  flip Array.foldMap (enumFromTo bottom top :: Array r) \r -> do
    let Rule mvars hyps con = TotalMap.lookup r language
    -- For each hyp, there is an edit that wraps with a tooth into that hyp,
    -- where the other kids are holes
    case ZipList.zips (List.fromFoldable hyps) of
      Nothing -> []
      -- `hyp` is what _would_ be at the bottom of the tooth
      Just hypZips -> Array.fromFoldable $ hypZips <#> \(hypPath /\ _hyp) -> do
        let rho = genFreshener mvars
        case sequence (defaultDerivTerm <$> hypPath) of
          Nothing -> []
          Just defaultHypPath -> do
            -- Each kid of the tooth is a default deriv
            let tooth0 = freshen' rho $ Expr.Tooth (DerivLabel r con) defaultHypPath

            -- In `isValidTooth`, we do only the necessary computation to check
            -- if the tooth is valid to wrap around the cursor. In particular,
            -- we don't yet apply the unifying substitutions to the while
            -- program.
            let isValidTooth = do
                  -- Unify sort of the tooth with the sort of the bottom of the
                  -- path above the cursor
                  _ /\ sigma1 <- unify (derivToothSort tooth0) (derivPathSort topSort (Expr.zipperPath dz))
                  let tooth1 = mapDerivLabelSort (Expr.subMetaExprPartially sigma1) <$> tooth0
                  -- Unify sort of the tooth interior with the sort of the
                  -- expression at the cursor
                  _ /\ sigma2 <- unify (derivToothInteriorSort tooth1) (derivTermSort (Expr.zipperExpr dz))
                  let tooth2 = mapDerivLabelSort (Expr.subMetaExprPartially sigma2) <$> tooth1

                  pure (composeSub sigma2 sigma1 /\ tooth2)

            -- !TODO OLD
            -- -- In `newCursor`, we do the rest of the computations involved in
            -- -- computing the new cursor, in particular inserting the tooth and
            -- -- applying the unifying substitutions to the whole program. It's
            -- -- `Lazy` so that we only invoke this when actually _doing_ the
            -- -- action.
            -- let newCursor sigma tooth = defer \_ -> do
            --       let path = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> Expr.zipperPath dz
            --       let expr = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> Expr.zipperExpr dz
            --       Expr.Zipper (Expr.stepPath tooth path) expr

            case isValidTooth of
              Nothing -> []
              Just (_sigma /\ tooth) -> pure
                { label: pretty r 
                , action: defer \_ -> WrapAction
                    -- - !TODO compute actual changes at the top and bottom
                    -- - !TODO we ignore `sigma` here since the substitution
                    --   will be applied just by smallstep propogation
                    { topChange: Expr.injectExprChange (derivToothSort tooth)
                    , dpath: Expr.Path (pure tooth)
                    , botChange: Expr.injectExprChange (derivToothInteriorSort tooth) }
                }

defaultEditsAtHoleInterior :: forall l r. IsRuleLabel l r => DerivPath Up l r -> Sort l -> Array (Edit l r)
defaultEditsAtHoleInterior up sort =
  -- For each rule, there is an edit that fills the hole with that constructor,
  -- where all the kids are hole derivs
  flip Array.foldMap (enumFromTo bottom top :: Array r) \r -> do
  let Rule mvars hyps con = TotalMap.lookup r language
  let rho = genFreshener mvars
  let mb_defaultHyps = sequence $ defaultDerivTerm <$> hyps
  case mb_defaultHyps of
    Nothing -> []
    Just defaultHyps -> do
      let fill0 = freshen' rho $
            -- Each kid is a hole deriv
            Expr.Expr (DerivLabel r con) defaultHyps
      
      -- In `isValidFill`, only do computation necessary to check if fill is
      -- valid
      let isValidFill = do
            -- Unify sort of the fill with the sort of the hole
            _ /\ sigma <- unify (derivTermSort fill0) sort
            let fill1 = mapDerivLabelSort (Expr.subMetaExprPartially sigma) <$> fill0

            pure (sigma /\ fill1)

      case isValidFill of
        Nothing -> []
        Just (sigma /\ fill) -> pure
          { label: pretty r
          , action: defer \_ -> FillAction {sub: sigma, dterm: fill}
          }

-- digEdit :: forall l r. IsRuleLabel l r => DerivZipper l r -> Array (Edit l r)
-- digEdit dz = do
--   case defaultDerivTerm ((derivTermSort (Expr.zipperExpr dz)) :: Sort l) of
--     Nothing -> empty
--     Just dterm -> pure
--       { label: "dig"
--       -- , action: SetCursorAction $ defer \_ ->
--       --     Expr.Zipper (Expr.zipperPath dz) dterm
--       , action: defer \_ -> (DigAction :: Action l r)
--       }
