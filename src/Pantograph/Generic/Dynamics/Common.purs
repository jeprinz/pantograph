module Pantograph.Generic.Dynamics.Common where

import Data.Display
import Data.Tree
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering
import Prelude
import Util

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Supertype (class Supertype)
import Data.Supertype as Supertype
import Data.Traversable (foldMap, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Halogen.Elements as El
import Pantograph.Generic.GlobalMessageBoard as GMB
import Text.Pretty (class Pretty, braces2, pretty)
import Type.Proxy (Proxy)
import Type.Row.Homogeneous (class Homogeneous)

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

instance Supertype (StepExpr sn el) (AnnExpr sn el ()) where
  inject (Tree node kids) = StepExpr node (Supertype.inject <$> kids)
  project = case _ of
    StepExpr node kids -> Tree node <$> Supertype.project `traverse` kids
    project -> Nothing

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
  = SteppingRule String (forall ctx env. Dynamics sn el ctx env => StepExpr sn el -> Maybe (StepExpr sn el))

applySteppingRule :: forall sn el ctx env. Dynamics sn el ctx env => SteppingRule sn el -> StepExpr sn el -> Maybe (StepExpr sn el)
applySteppingRule (SteppingRule name f) e = do
  e' <- f e
  GMB.debugM $ El.matrix 
    [ [El.β [display e]]
    , [El.π ("applySteppingRule: " <> name)]
    , [El.β [display e']] ]
  pure e'

-- Display (StepExpr sn el)

instance Rendering sn el ctx env => Display (StepExpr sn el) where
  display = El.ι <<< fst <<< snd (runRenderM :: Proxy sn /\ _) <<< renderStepExpr

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
    [ El.ℓ [El.Classes [El.StepExprBoundaryContainer]]
        [ El.ℓ [El.Classes [El.StepExprBoundaryInfo]]
            [ El.ℓ [El.Classes [El.StepExprBoundaryDirection]] [El.τ $ pretty dir]
            , El.ℓ [El.Classes [El.StepExprBoundaryChange]] [display ch]]
        , El.ℓ (propsStepExpr e) htmls ] ]
renderStepExpr e@(Marker kid) = do
  htmls <- renderStepExpr kid
  pure $ [El.ℓ (propsStepExpr e) htmls]

propsStepExpr :: forall sn el. StepExpr sn el -> El.Props (Aff Unit)
propsStepExpr e = 
  [ El.Classes case e of
      StepExpr _ _ -> [El.StepExpr] 
      Boundary _ _ -> [El.StepExprBoundary]
      Marker _ -> [El.StepExprMarker]
  , El.StrictHover mempty
  ]

-- utilities

getStepExprSort :: forall sn el. Language sn el => StepExpr sn el -> Sort sn
getStepExprSort (StepExpr node _) = getExprNodeSort node
getStepExprSort (Boundary (dir /\ ch) _) = 
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

-- | Convert a `StepExpr` into an `ExprCursor`. The `StepExpr` must have
-- | _exactly_ 1 `Marker` and exactly `0` `Boundary`s.
fromStepExprToExprCursor :: forall sn el ctx env. Rendering sn el ctx env => StepExpr sn el -> Maybe (ExprCursor sn el)
fromStepExprToExprCursor e0 = case goCursorOrExpr e0 of
    Just (Left {tooths, inside, orientation}) -> Just $ Cursor {outside: Path (List.reverse tooths), inside, orientation}
    Just (Right e) -> GMB.error $ El.ι [El.τ "no `Marker` found during `fromStepExprToExprCursor`: ", El.β [El.τ "e0: ", display e0], El.β [El.τ "e: ", displayAnnExpr e]]
    Nothing -> Nothing
  where
  goExpr :: StepExpr sn el -> Maybe (Expr sn el)
  goExpr (Boundary _ _) = GMB.debug (El.matrix [[El.τ "encountered a `Boundary` during `goExpr`"], [display e0]]) \_ -> Nothing
  goExpr (Marker _) = GMB.debug (El.matrix [[El.τ "encountered multiple `Marker`s during `goExpr`"], [display e0]]) \_ -> Nothing
  goExpr (StepExpr node kids) = Tree node <$> (goExpr `traverse` kids)

  goCursorOrExpr :: StepExpr sn el -> Maybe ({tooths :: List (ExprTooth sn el), inside :: Expr sn el, orientation :: Orientation} \/ Expr sn el)
  goCursorOrExpr (Boundary _ _) = GMB.debug (El.matrix $ [[El.τ "encountered a `Boundary` during `fromStepExprToExprCursor`"], [display e0]]) \_ -> Nothing
  goCursorOrExpr (Marker e) = do
    inside <- goExpr e
    Just $ Left $ {tooths: mempty, inside, orientation: Outside}
  goCursorOrExpr (StepExpr node kids) = do
    let f = case _ of
          Nothing /\ kids' -> case _ of
            i /\ Left cursor -> Just (i /\ cursor) /\ kids'
            _ /\ Right kid' -> Nothing /\ Array.cons kid' kids'
          Just i_cursor /\ kids' -> case _ of
            _ /\ Left _cursor -> GMB.error $ El.matrix $ [[El.τ "encountered multiple `Marker`s during `goExpr`"], [display e0]]
            _ /\ Right kid' -> Just i_cursor /\ Array.cons kid' kids'
    
    coeKids <- goCursorOrExpr `traverse` kids
    let maybe_i_cursor /\ kids' = Array.foldr (flip f) (Nothing /\ []) $ Array.mapWithIndex Tuple $ coeKids

    case maybe_i_cursor of
      Just (i /\ {tooths, inside, orientation}) -> Just $ Left $ {tooths: List.Cons (Tooth node (i /\ kids')) tooths, inside, orientation}
      Nothing -> Just $ Right $ Tree node kids'

{-
    let
      f = case _ of
        Nothing /\ kids' -> case _ of
          i /\ Left cursor -> Just $ Just (i /\ cursor) /\ kids'
          _ /\ Right kid' -> Just $ Nothing /\ Array.cons kid' kids'
        Just i_cursor /\ kids' -> case _ of
          _ /\ Left _cursor -> GMB.debug (El.matrix $ [[El.τ "encountered multiple `Marker`s during `goExpr`"], [display e0]]) \_ -> Nothing
          _ /\ Right kid' -> Just $ Just i_cursor /\ Array.cons kid' kids'
    kids' <- goCursorOrExpr `traverse` kids
    maybe_i_cursor /\ kids'' <- Array.foldM f (Nothing /\ []) $ Array.mapWithIndex Tuple kids'
    case maybe_i_cursor of
      Just (i /\ {tooths, inside, orientation}) -> Just $ Left $ {tooths: List.Cons (Tooth node (i /\ kids'')) tooths, inside, orientation}
      Nothing -> Just $ Right $ Tree node kids''
-}

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
