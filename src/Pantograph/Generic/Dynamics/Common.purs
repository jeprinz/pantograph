module Pantograph.Generic.Dynamics.Common where

import Data.Display
import Data.Either.Nested
import Data.Traversable
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Text.Pretty
import Util

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Newtype (unwrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Supertype (class Supertype)
import Data.Supertype as Supertype
import Data.Tuple (Tuple(..), fst, snd)
import Debug as Debug
import Halogen.Elements as El
import Pantograph.Generic.Rendering.TerminalItems as TI
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Util (debugM)

-- Dynamics

class Rendering sn el ctx env <= Dynamics sn el ctx env | sn -> el ctx env where
  steppingRules :: Array (SteppingRule sn el)

-- StepExpr

data StepExpr sn el
  = StepExpr (ExprNode sn el) (Array (StepExpr sn el))
  | Boundary (Direction /\ SortChange sn) (StepExpr sn el)
  | Marker (StepExpr sn el)

infix 5 StepExpr as %.
infix 5 Boundary as %.|

derive instance Generic (StepExpr sn el) _
instance (Show sn, Show el) => Show (StepExpr sn el) where show x = genericShow x
instance (Eq sn, Eq el) => Eq (StepExpr sn el) where eq x y = genericEq x y

instance (Show sn, PrettyTreeNode el, PrettyTreeNode sn) => Pretty (StepExpr sn el) where
  pretty = case _ of
    StepExpr node kids -> prettyTreeNode node (pretty <$> kids)
    Boundary (dir /\ ch) e -> "{{ " <> pretty dir <> " | " <> pretty ch <> " | " <> pretty e <> " }}"
    Marker kid -> braces2 $ pretty kid

instance Supertype (AnnExpr sn el ()) (StepExpr sn el) where
  inject (Tree node kids) = StepExpr node (Supertype.inject <$> kids)
  project (StepExpr node kids) = Tree node <$> Supertype.project `traverse` kids
  project _ = Nothing

makeStepExpr :: forall sn el. Language sn el => el -> Array (String /\ (Tree (SortNode sn))) -> Array (StepExpr sn el) -> StepExpr sn el
makeStepExpr label sigma_ = 
  let node = makeExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr node kids

buildStepExpr :: forall r sn el. Homogeneous r (Sort sn) => Language sn el => el -> Record r -> Array (StepExpr sn el) -> StepExpr sn el
buildStepExpr label sigma_ = 
  let node = buildExprNode label sigma_ in
  assertValidTreeKids "makeStepExpr" node \kids -> 
    StepExpr node kids

-- | Erases markers in `StepExpr`
fromStepExprToExpr :: forall sn el. StepExpr sn el -> Expr sn el
fromStepExprToExpr (StepExpr node kids) = Tree node (fromStepExprToExpr <$> kids)
fromStepExprToExpr (Boundary _ e) = fromStepExprToExpr e
fromStepExprToExpr (Marker e) = fromStepExprToExpr e

-- Direction

data Direction = Up | Down

derive instance Generic Direction _
instance Show Direction where show = genericShow
instance Eq Direction where eq x y = genericEq x y
instance Ord Direction where compare x y = genericCompare x y
instance Pretty Direction where
  pretty Up = "↑"
  pretty Down = "↓"

-- SteppingRule

data SteppingRule sn el 
  = SteppingRule String (Language sn el => StepExpr sn el -> Maybe (StepExpr sn el))

applySteppingRule :: forall sn el. Language sn el => SteppingRule sn el -> StepExpr sn el -> Maybe (StepExpr sn el)
applySteppingRule (SteppingRule name f) e = do
  e' <- f e
  debugM ("[applySteppingRule] " <> name) {expr: pretty e}
  -- TODO: can't use renderStepExpr since cyclic module dependencies
  -- TI.addM $ El.ι [El.π ("[applySteppingRule:" <> name <> "]"), renderStepExpr e]
  TI.addM $ El.ι [El.π ("[applySteppingRule:" <> name <> "]"), El.π $ pretty e]
  pure e'

-- Utilities
-- TODO: reorganize stuff into other modules

runRenderStepExpr :: forall sn el ctx env.
  Rendering sn el ctx env =>
  StepExpr sn el ->
  Array Html
runRenderStepExpr e = fst $ snd (runRenderM :: Proxy sn /\ _) $ renderStepExpr e

renderStepExpr :: forall sn el ctx env. 
  Rendering sn el ctx env =>
  StepExpr sn el ->
  RenderM sn el ctx env (Array Html)
renderStepExpr e@(StepExpr node kids) = do
  arrangedKids <- arrangeExpr node
    (kids <#> \kid ->
      renderStepExpr kid <#> (_ /\ getStepExprNode kid))
  let htmls = arrangedKids # foldMap case _ of
        ArrangeKid htmls' -> htmls'
        ArrangeHtml htmls' -> htmls'
  pure $ [El.ℓ (propsStepExpr e) htmls]
renderStepExpr e@(Boundary (dir /\ ch) kid) = do
  htmls <- renderStepExpr kid
  pure 
    [ El.ℓ [El.Classes [El.StepExprBoundaryInfo]]
        [ El.ℓ [El.Classes [El.StepExprBoundaryDirection]] [El.text $ pretty dir]
        , El.ℓ [El.Classes [El.StepExprBoundaryChange]] [display ch]]
    , El.ℓ (propsStepExpr e) htmls ]
renderStepExpr e@(Marker kid) = do
  htmls <- renderStepExpr kid
  pure $ [El.ℓ (propsStepExpr e) htmls]

propsStepExpr :: forall sn el. StepExpr sn el -> El.Props Unit
propsStepExpr e = 
  [ El.Classes case e of
      StepExpr _ _ -> [El.StepExpr] 
      Boundary _ _ -> [El.StepExprBoundary]
      Marker _ -> [El.StepExprMarker]
  , El.StrictHover (const unit)
  ]

-- getStepExprNode :: forall sn el. StepExpr sn el -> ExprNode sn el
-- getStepExprNode (StepExpr node _) = node
-- getStepExprNode (Boundary _ kid) = getStepExprNode kid
-- getStepExprNode (Marker kid) = getStepExprNode kid

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

fromStepExpr :: forall sn el ctx env. Rendering sn el ctx env => StepExpr sn el -> ExprCursor sn el \/ Expr sn el
fromStepExpr e0 = case go e0 of
    Left {tooths, inside, orientation} -> Left (Cursor {outside: Path (List.reverse tooths), inside, orientation})
    Right expr -> Right expr
  where
  goExpr :: StepExpr sn el -> Expr sn el
  goExpr (Boundary _ _) = TI.bug $ El.ℓ [El.Classes [El.Inline]] $ [El.text "encountered a `Boundary` during `goExpr`: "] <> runRenderStepExpr e0
  goExpr (Marker _) = TI.bug $ El.ℓ [El.Classes [El.Inline]] $ [El.text "encountered multiple `Marker`s during `goExpr`: "] <> runRenderStepExpr e0
  goExpr (StepExpr node kids) = Tree node (kids <#> goExpr)

  go :: StepExpr sn el -> {tooths :: List (ExprTooth sn el), inside :: Expr sn el, orientation :: Orientation} \/ Expr sn el
  go (Boundary _ _) = TI.bug $ El.inline $ [El.text "encountered a `Boundary` during `fromStepExpr`: "] <> runRenderStepExpr e0
  go (Marker e) = Left $ {tooths: mempty, inside: goExpr e, orientation: Outside}
  go (StepExpr node kids) =
    let
      f = case _ of
        Nothing /\ kids' -> case _ of
          i /\ Left cursor -> Just (i /\ cursor) /\ kids'
          _ /\ Right kid' -> Nothing /\ Array.cons kid' kids'
        Just i_cursor /\ kids' -> case _ of
          _ /\ Left _cursor -> TI.bug $ El.inline $ [El.text "encountered multiple `Marker`s during `goExpr`: "] <> runRenderStepExpr e0
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

getStepExprNode :: forall sn el. StepExpr sn el -> ExprNode sn el
getStepExprNode (StepExpr node _) = node
getStepExprNode (Boundary _ kid) = getStepExprNode kid
getStepExprNode (Marker kid) = getStepExprNode kid

-- setup SteExpr

setupEdit :: forall sn el. Language sn el => ExprCursor sn el -> Edit sn el -> StepExpr sn el
setupEdit (Cursor cursor) (Edit edit) =
  debug "setupEdit" {cursor_outside: pretty cursor.outside, cursor_inside: pretty cursor.inside, edit_outerChange: pretty edit.outerChange, edit_middle: pretty edit.middle, edit_innerChange: pretty edit.innerChange, edit_sigma: pretty edit.sigma} \_ ->
  wrapExprPath (maybe identity applySortVarSubst edit.sigma cursor.outside) $
  edit.outerChange # maybe identity (boundary Up) $
  edit.middle # maybe identity (wrapExprPath <<< toPath)  $
  edit.innerChange # maybe identity (boundary Down)  $
  marker $
  toStepExpr (edit.inside # maybe' (\_ -> maybe identity applySortVarSubst edit.sigma cursor.inside) identity)

-- stepping engine

type StepM sn el = ReaderT (Array (SteppingRule sn el)) Identity

runStepM :: forall sn el a. Language sn el =>
  Array (SteppingRule sn el) -> StepM sn el a -> a
runStepM rules = flip runReaderT (builtinRules <> rules) >>> unwrap 

runStepExpr :: forall sn el ctx env. Dynamics sn el ctx env =>
  StepExpr sn el ->
  Maybe (ExprGyro sn el)
runStepExpr expr = do
  TI.addM (El.inline (runRenderStepExpr expr))
  let expr' = runStepM steppingRules $ stepFixpoint expr
  case fromStepExpr expr' of
    Left cursor -> Just $ CursorGyro cursor
    Right expr' -> Just $ RootGyro expr'

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
  , combineDownRule
  , idRule
  , threadUpBoundaryThroughMarker
  , threadDownBoundaryThroughMarker
  ]

passThroughRule :: forall el sn. Eq sn => Show sn => PrettyTreeNode sn => SteppingRule sn el
passThroughRule = SteppingRule "passThroughRule" case _ of
  Boundary (Down /\ down) (Boundary (Up /\ up) kid) -> Just
    let hypotenuse = lub' down up in
    let up' = invert down <> hypotenuse in
    let down' = invert up <> hypotenuse in
    Boundary (Up /\ up') (Boundary (Down /\ down') kid)
  _ -> Nothing

threadUpBoundaryThroughMarker :: forall el sn. SteppingRule sn el
threadUpBoundaryThroughMarker = SteppingRule "threadUpBoundaryThroughMarker" case _ of
  Marker (Boundary (Up /\ ch) e) -> Just $ Boundary (Up /\ ch) (Marker e)
  _ -> Nothing

threadDownBoundaryThroughMarker :: forall el sn. SteppingRule sn el
threadDownBoundaryThroughMarker = SteppingRule "threadUpBoundaryThroughMarker" case _ of
  Boundary (Down /\ ch) (Marker e) -> Just $ Marker (Boundary (Down /\ ch) e)
  _ -> Nothing

combineDownRule :: forall sn el. Eq sn => SteppingRule sn el
combineDownRule = SteppingRule "combineDownRule" case _ of
  Boundary (Down /\ down1) (Boundary (Down /\ down2) kid) -> Just $
    Boundary (Down /\ (down1 <> down2)) kid
  _ -> Nothing

combineUpRule :: forall sn el. Eq sn => SteppingRule sn el
combineUpRule = SteppingRule "combineUpRule" case _ of
  Boundary (Up /\ up1) (Boundary (Up /\ up2) kid) -> Just $
    Boundary (Up /\ (up1 <> up2)) kid
  _ -> Nothing

idRule :: forall sn el. Eq sn => SteppingRule sn el
idRule = SteppingRule "idRule" case _ of
  Boundary (_ /\ ch) e | isIdentity ch -> Just e
  _ -> Nothing
