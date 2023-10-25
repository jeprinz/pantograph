module Language.Pantograph.Generic.Rendering.Rendering where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Rendering.Base
import Language.Pantograph.Generic.Rendering.Elements
import Prelude

import Bug (bug)
import Bug.Assertion (assert, assertInput_, just)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Expr (wellformedExpr, wellformedExprF, (%))
import Data.Expr as Expr
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Zippable as Zippable
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Language.Pantograph.Generic.Rendering.Preview (previewComponent)
import Language.Pantograph.Generic.Smallstep (SSTerm, StepExprLabel(..))
import Language.Pantograph.Generic.Smallstep as SmallStep
import Text.Pretty (pretty)
import Type.Direction (Up, leftDir, rightDir)
import Util (fromJust')
import Partial.Unsafe (unsafePartial)
import Debug (trace)
import Debug (traceM)

------------------------------------------------------------------------------
-- arrange
------------------------------------------------------------------------------

arrangeDerivTermSubs :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivZipper l r ->
  Array (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  Array (EditorHTML l r)
arrangeDerivTermSubs locs dzipper@(Expr.Zipper dpath dterm) kidCtxElems renCtx = assert (wellformedExpr "arrangeDerivTermSubs" dterm) \_ -> case dterm of
--  DerivLabel rule sigma % [] | isHoleRule rule -> do
--    let sort = getSortFromSub rule sigma
--    arrangeHoleExterior locs sort (renderHoleInterior locs false dpath sort) renCtx
  label@(DerivLabel rule sigma) % kids -> do
    let sort = getSortFromSub rule sigma
    let subCtxSymElems = locs.spec.arrangeDerivTermSubs unit {mb_parent: Nothing, renCtx, rule, sort, sigma}
    let kidCtxElems' =
            if isHoleRule rule then
                [renderHoleInterior locs false dzipper] <> kidCtxElems
                else kidCtxElems
    Array.concat $ subCtxSymElems <#> case _ of
      Left (renCtx' /\ kidIx) -> assert (just "arrangeDerivTermSubs" (Array.index kidCtxElems' kidIx)) \kidElem -> [kidElem renCtx']
      Right elems -> elems
  DerivString str % [] -> 
    [ if String.null str 
        then HH.div [classNames ["subnode", "string-inner", "empty-string"]] [HH.text "String"]
        else HH.div [classNames ["subnode", "string-inner"]] [HH.text str] ]

arrangeNodeSubs :: forall l r. IsRuleLabel l r => 
  EditorLocals l r ->
  Boolean ->
  HoleyDerivZipper l r ->
  Array (EditorHTML l r) ->
  Array (EditorHTML l r)
arrangeNodeSubs locs isCursor hdzipper subElems =
  Array.concat
  [ if not isCursor then [] else 
    [ HH.slot bufferSlot unit bufferComponent 
        { hdzipper
        , edits: editsAtHoleyDerivZipper locs.spec hdzipper <#>
            \edit -> 
              { lazy_preview: renderPreview locs hdzipper edit
              , edit }
        }
        locs.handleBufferOutput
    , HH.slot_ previewSlot leftDir previewComponent leftDir
    ]
  , subElems
  , if not isCursor then [] else
    [ HH.slot_ previewSlot rightDir previewComponent rightDir ]
  ]

------------------------------------------------------------------------------
-- render term
------------------------------------------------------------------------------

renderDerivTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Boolean ->
  DerivZipper l r ->
  RenderingContext ->
  EditorHTML l r -- /\ Tree Element
renderDerivTerm locs isCursor dzipper renCtx =
  HH.div
    (Array.concat
      [ [classNames $ ["node"] <> if isCursor then [cursorClassName] else []]
      , if not renCtx.isInteractive then [] else do
        let elemId = fromPathToElementId (Expr.zipperPath dzipper)
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (injectHoleyDerivZipper dzipper))
        , HE.onMouseOver (locs.onMouseOver (injectHoleyDerivZipper dzipper))
        ]
      ])
    (arrangeNodeSubs locs isCursor (injectHoleyDerivZipper dzipper)
      (arrangeDerivTermSubs locs dzipper (Zippable.zipDowns dzipper <#> renderDerivTerm locs false) renCtx))

------------------------------------------------------------------------------
-- render hole exterior and interior
------------------------------------------------------------------------------

renderHoleInterior :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Boolean ->
  DerivZipper l r ->
  RenderingContext ->
  EditorHTML l r
renderHoleInterior locs isCursor dzipper renCtx = do
--  let _ = trace ("renderHoleInterior called with dzipper = " <> pretty dzipper) \_ -> 5
  let hdzipper = (HoleyDerivZipper dzipper true)
  let hdpath = hdzipperHoleyDerivPath hdzipper
  HH.div
    (Array.concat
      [ [classNames $ ["node", "holeInterior"] <> if isCursor then [cursorClassName] else []]
      , if not renCtx.isInteractive then [] else do
        let elemId = fromHoleyDerivPathToElementId hdpath
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (HoleyDerivZipper dzipper true))
        , HE.onMouseOver (locs.onMouseOver (HoleyDerivZipper dzipper true))
        ]
      ])
    (arrangeNodeSubs locs isCursor hdzipper
      [ HH.div [classNames ["subnode", "holeInterior-inner"]]
        [interrogativeElem]
      ])

------------------------------------------------------------------------------
-- render path
------------------------------------------------------------------------------

renderPath :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivZipper l r ->
  (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  EditorHTML l r
renderPath locs dzipper interior =
  case Expr.zipUp dzipper of
    Nothing -> interior
    Just (th /\ dzipper2) ->
      renderPath locs dzipper2
        (\renCtx -> 
          HH.div
            (Array.concat
              [ [classNames ["node"]]
              , if not renCtx.isInteractive then [] else do
                let elemId = fromPathToElementId (Expr.zipperPath dzipper2)
                [ HP.id elemId
                , HE.onMouseDown (locs.onMouseDown (injectHoleyDerivZipper dzipper2))
                , HE.onMouseOver (locs.onMouseOver (injectHoleyDerivZipper dzipper2))
                ]
              ])
            (arrangeNodeSubs locs false (injectHoleyDerivZipper dzipper2)
              (arrangeDerivTermSubs locs 
                (Expr.Zipper (Expr.zipperPath dzipper2) (Expr.unTooth th (Expr.zipperExpr dzipper)))
                ( Array.fromFoldable $
                  ZipList.unpathAround interior do
                    let kidZippers = Expr.zipDownsTooth dzipper2 th
                    kidZippers <#> renderDerivTerm locs false )
                renCtx)))

------------------------------------------------------------------------------
-- render preview
------------------------------------------------------------------------------

renderPreview :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  HoleyDerivZipper l r ->
  Edit l r ->
  Lazy (EditPreviewHTML l r)
renderPreview locs hdzipper edit = edit.action <#> case _ of
  FillAction {dterm} -> FillEditPreview $ renderPreviewDerivTerm locs (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
  ReplaceAction {dterm} -> ReplaceEditPreview $ renderPreviewDerivTerm locs (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
  WrapAction {dpath} -> WrapEditPreview $ renderPreviewDerivPath locs (hdzipperDerivPath hdzipper) dpath (hdzipperDerivTerm hdzipper)

renderPreviewDerivPath :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivPath Up l r ->
  DerivPath Up l r ->
  DerivTerm l r ->
  {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
renderPreviewDerivPath locs _up (Expr.Path Nil) _dterm = {before: [], after: []}
renderPreviewDerivPath locs up (Expr.Path (th : ths)) dterm = do
  let next = renderPreviewDerivPath locs up (Expr.Path ths) (Expr.unTooth th dterm)
  let {before, after} = renderPreviewDerivTooth locs up th dterm
  {before: next.before <> before, after: after <> next.after}


-- !TODO maybe someday have this use rendering context
renderPreviewDerivTooth :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivPath Up l r ->
  DerivTooth l r ->
  DerivTerm l r ->
  {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
renderPreviewDerivTooth locs up dtooth@(Expr.Tooth dl kidsPath) dterm = do
  let rule /\ sigma = case dl of
        DerivLabel rule sigma -> rule /\ sigma
        DerivString _ -> bug "in `renderPreviewDerivTooth`: should not have a tooth with a non-DerivLabel DerivLabel"

  let dzipper = Expr.Zipper up (Expr.unTooth dtooth dterm)
  let kidDZippers = Zippable.zipDowns dzipper
  -- deferred since we know that the kid inside the tooth will not actually
  -- get forced to render
  let kidElems = kidDZippers <#> \dzipper' -> defer \_ -> renderPreviewDerivTerm locs dzipper'

  let renderSubElem = case _ of
        -- don't use renCtx' here because rendering previews doesn't use
        -- rendering context
        Left (_renCtx' /\ i) -> [force $ fromJust' "renderPreviewDerivTooth" $ kidElems Array.!! i]
        Right elems -> elems

  let kids = Array.fromFoldable $ ZipList.unpathAround dterm kidsPath
  let sort = getSortFromSub rule sigma
  let subCtxSymElems = assert (wellformedExprF "renderPreviewDerivTooth" pretty (DerivLabel rule sigma /\ kids)) \_ -> 
        locs.spec.arrangeDerivTermSubs unit {mb_parent: Nothing, renCtx: previewRenderingContext unit, rule, sort, sigma}
  let toothInteriorKidIx = ZipList.leftLength kidsPath
  let isToothInterior = case _ of
        Left (_renCtx' /\ i) -> i == toothInteriorKidIx
        _ -> false
  let toothInteriorSymElemIx = fromJust' "renderPreviewDerivTooth" $ Array.findIndex isToothInterior subCtxSymElems
  let before = Array.take toothInteriorSymElemIx subCtxSymElems
  let after = Array.drop (toothInteriorSymElemIx + 1) subCtxSymElems
        
  { before: Array.concat $ before <#> renderSubElem
  , after: Array.concat $ after <#> renderSubElem
  }

renderPreviewDerivTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivZipper l r ->
  EditorHTML l r
renderPreviewDerivTerm locs dzipper =
  HH.div
    [classNames ["node"]]
    (arrangeDerivTermSubs locs 
      dzipper 
      (Zippable.zipDowns dzipper <#> \kidDZipper _ -> renderPreviewDerivTerm locs kidDZipper) 
      (previewRenderingContext unit))

------------------------------------------------------------------------------
-- render small-step
------------------------------------------------------------------------------

renderSSTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  SSTerm l r ->
  RenderingContext ->
  EditorHTML l r
renderSSTerm locs = flip \renCtx -> assertInput_ (wellformedExpr "renderSSTerm") case _ of
  Inject (DerivLabel rule sort) % [] | isHoleRule rule ->
    HH.div
      [classNames ["node", "smallstep", "hole"]]
      [ interrogativeElem
      , colonElem
      , HH.text $ pretty sort
      ]
  Inject (DerivString str) % [] -> 
    HH.div
      [classNames ["node", "smallstep", "string"]]
      [ lparenElem
      , interrogativeElem
      , colonElem
      , HH.text $ "String " <> show str
      , rparenElem
      ]
  Inject (DerivLabel rule sigma) % kids | not (isHoleRule rule) ->
    HH.div
      [classNames ["node", "smallstep"]]
      let kidElems = renderSSTerm locs <$> kids in
      (Array.concat $ locs.spec.arrangeDerivTermSubs unit {mb_parent: Nothing, renCtx, rule, sort: getSortFromSub rule sigma, sigma} <#> case _ of
        Left (renCtx' /\ kidIx) -> assert (just "renderSSTerm" (Array.index kidElems kidIx)) \kid -> [kid renCtx']
        Right elems -> elems)
  -- TODO for Henry from Jacob: make this work with any number of kids n, instead of just 0 and 1. Or maybe it doesn't matter.
  Marker 0 % [] -> do HH.div [classNames ["node", "smallstep", "cursor"]] []
  Marker 1 % [kid] -> do
    HH.div
      [classNames ["node", "smallstep", "cursor"]]
      [renderSSTerm locs kid renCtx]
  Boundary dir sortCh % [kid] -> 
    HH.div
      [classNames ["node", "smallstep", "boundary"]]
      [ HH.span [classNames ["subnode", "smallstep", "changeDirection"]] 
          [case dir of
            SmallStep.Up -> upArrowElem
            SmallStep.Down -> downArrowElem]
      , lbracketElem
      , HH.span [classNames ["subnode", "smallstep", "sortChange"]] [HH.text $ pretty sortCh]
      , rbracketElem
      , lbraceElem
      , renderSSTerm locs kid renCtx
      , rbraceElem
      ]

