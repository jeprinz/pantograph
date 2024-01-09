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
import Data.Set as Set
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Zippable as Zippable
import Debug (trace)
import Debug (traceM)
import Debug as Debug
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Language.Pantograph.Generic.Rendering.Preview (previewComponent)
import Language.Pantograph.Generic.Smallstep (SSTerm, StepExprLabel(..))
import Language.Pantograph.Generic.Smallstep as SmallStep
import Partial.Unsafe (unsafePartial)
import Text.Pretty (pretty)
import Type.Direction (Up, leftDir, rightDir)
import Util (fromJust')

------------------------------------------------------------------------------
-- helper
------------------------------------------------------------------------------

applyCssClasses :: forall l r. Set.Set String -> EditorHTML l r -> EditorHTML l r
applyCssClasses classes html =
    if Set.isEmpty classes then html
    else HH.div [classNames (Array.fromFoldable (Set.insert "node" classes))] [html]

------------------------------------------------------------------------------
-- arrange
------------------------------------------------------------------------------

arrangeDerivTermSubs :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Boolean ->
  DerivZipper l r ->
  Array (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  Array (EditorHTML l r)
arrangeDerivTermSubs locs innerHoleIsCursor dzipper@(Expr.Zipper dpath dterm) kidCtxElems renCtx = assert (wellformedExpr "arrangeDerivTermSubs" dterm) \_ -> case dterm of
--  DerivLabel rule sigma % [] | hasInnerHole rule -> do
--    let sort = getSortFromSub rule sigma
--    arrangeHoleExterior locs sort (renderHoleInterior locs false dpath sort) renCtx
  label@(DerivLabel rule sigma) % kids -> do
    let sort = getSortFromSub rule sigma
    let subCtxSymElems = locs.spec.arrangeDerivTermSubs unit
            {mb_parent: Expr.zipperParent dzipper, renCtx, rule, sort, sigma, dzipper: Just dzipper,
                renderTerm: renderDerivTerm (trivialEditorLocals locs.spec) false false}
    let kidCtxElems' =
            if hasInnerHole rule then
                [renderHoleInterior locs innerHoleIsCursor dzipper] <> kidCtxElems
                else kidCtxElems
    Array.concat $ subCtxSymElems <#> case _ of
      Left (renCtx' /\ kidIx) -> assert (just "arrangeDerivTermSubs" (Array.index kidCtxElems' kidIx))
            \kidElem -> [applyCssClasses renCtx'.cssClasses (kidElem renCtx')]
      Right elems -> elems
  DerivLiteral (DataString str) % [] ->
    [ if String.null str 
        then HH.div [classNames ["subnode", "string-inner", "empty-string"]] [HH.div [classNames ["subnode"]] [HH.text "▪"]]
        else HH.div [classNames ["subnode", "string-inner"]] [HH.text str] ]
  DerivLiteral (DataInt n) % [] ->
    [ HH.div [classNames ["subnode", "string-inner"]] [HH.text (show n)] ]

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
        , extraEdits: locs.spec.extraQueryEdits (derivTermSort (hdzipperDerivTerm hdzipper))
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
  Boolean ->
  DerivZipper l r ->
  RenderingContext ->
  EditorHTML l r -- /\ Tree Element
renderDerivTerm locs isCursor innerHoleIsCursor dzipper renCtx =
  HH.div
    (Array.concat
      [ [ classNames $ ["node"] <> 
          (if isCursor then [cursorClassName] else []) <> 
          (if isEmptyString dzipper then [emptyStringClassName] else [])
        ]
      , if not renCtx.isInteractive then [] else do
        let elemId = fromPathToElementId renCtx.pathIdPrefix (Expr.zipperPath dzipper)
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (injectHoleyDerivZipper dzipper))
        , HE.onMouseOver (locs.onMouseOver (injectHoleyDerivZipper dzipper))
        ]
      ])
    (arrangeNodeSubs locs isCursor (injectHoleyDerivZipper dzipper)
      (arrangeDerivTermSubs locs innerHoleIsCursor dzipper (Zippable.zipDowns dzipper <#> renderDerivTerm locs false false) renCtx))
  where
  isEmptyString = case _ of
    Expr.Zipper _ (Expr.Expr (DerivLiteral (DataString str)) _) | String.null str -> true
--    Expr.Zipper _ (Expr.Expr (DerivString str) _) -> false
    _ -> false

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
        let elemId = fromHoleyDerivPathToElementId renCtx.pathIdPrefix hdpath
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (HoleyDerivZipper dzipper true))
        , HE.onMouseOver (locs.onMouseOver (HoleyDerivZipper dzipper true))
        ]
      ])
    (arrangeNodeSubs locs isCursor hdzipper
      [ HH.div [classNames ["subnode", "holeInterior-inner"]]
        [HH.text " "] -- [squareElem]
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
                let elemId = fromPathToElementId renCtx.pathIdPrefix (Expr.zipperPath dzipper2)
                [ HP.id elemId
                , HE.onMouseDown (locs.onMouseDown (injectHoleyDerivZipper dzipper2))
                , HE.onMouseOver (locs.onMouseOver (injectHoleyDerivZipper dzipper2))
                ]
              ])
            (arrangeNodeSubs locs false (injectHoleyDerivZipper dzipper2)
              (arrangeDerivTermSubs locs false
                dzipper2
                ( Array.fromFoldable $
                  ZipList.unpathAround interior do
                    let kidZippers = Expr.zipDownsTooth dzipper2 th
                    kidZippers <#> renderDerivTerm locs false false)
                renCtx)))

------------------------------------------------------------------------------
-- render preview
------------------------------------------------------------------------------

renderPreview :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  HoleyDerivZipper l r ->
  Edit l r ->
  Lazy (EditPreviewHTML l r)
renderPreview locs hdzipper edit = defer \_ ->
    case edit.action of
        Left errorMsg -> WrapEditPreview {before: [], after: []} -- This is a hack, there should be some kind of
            -- preview designed for error situations.
        Right action -> case force action of
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
        DerivLiteral _ -> bug "in `renderPreviewDerivTooth`: should not have a tooth with a non-DerivLabel DerivLabel"

  let dzipper = Expr.Zipper up (Expr.unTooth dtooth dterm)
  let kidDZippers = Zippable.zipDowns dzipper
  -- deferred since we know that the kid inside the tooth will not actually
  -- get forced to render
  let kidElems = kidDZippers <#> \dzipper' -> defer \_ -> renderPreviewDerivTerm locs dzipper'

  let renderSubElem = case _ of
        -- don't use renCtx' here because rendering previews doesn't use
        -- rendering context
        Left (renCtx' /\ i) -> [applyCssClasses renCtx'.cssClasses $ force $ fromJust' "renderPreviewDerivTooth" $ kidElems Array.!! i]
        Right elems -> elems

  let kids = Array.fromFoldable $ ZipList.unpathAround dterm kidsPath
  let sort = getSortFromSub rule sigma
  let subCtxSymElems = assert (wellformedExprF "renderPreviewDerivTooth" pretty (DerivLabel rule sigma /\ kids)) \_ -> 
        locs.spec.arrangeDerivTermSubs unit {mb_parent: Expr.zipperParent dzipper, renCtx: previewRenderingContext "shouldn't get used", rule, sort, sigma, dzipper: Just dzipper,
                    renderTerm: renderDerivTerm (trivialEditorLocals locs.spec) false false}
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
    (arrangeDerivTermSubs locs false
      dzipper 
      (Zippable.zipDowns dzipper <#> \kidDZipper _ -> renderPreviewDerivTerm locs kidDZipper) 
      (previewRenderingContext "shouldn't get used"))

------------------------------------------------------------------------------
-- render small-step
------------------------------------------------------------------------------

renderSSTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  SSTerm l r ->
  RenderingContext ->
  EditorHTML l r
renderSSTerm locs term renCtx = case term of
--  SSInj (DerivLabel rule sort) % [] | hasInnerHole rule ->
--    HH.div
--      [classNames ["node", "smallstep", "hole"]]
--      [ interrogativeElem
--      , colonElem
--      , HH.text $ pretty sort
--      ]
  SSInj (DerivLiteral (DataString str)) % [] ->
    HH.div
      [classNames ["node", "smallstep", "string"]]
      [ lparenElem
      , interrogativeElem
      , colonElem
      , HH.text $ "String " <> show str
      , rparenElem
      ]
  SSInj (DerivLiteral (DataInt n)) % [] ->
    HH.div [classNames ["subnode", "smallstep", "string-inner"]] [HH.text (show n)]
  SSInj (DerivLabel rule sigma) % kids ->
    HH.div
      [classNames ["node", "smallstep"]]
      let kidElems = (if hasInnerHole rule then [\_ -> HH.div_ [interrogativeElem, colonElem]] else []) <> (renderSSTerm locs <$> kids) in
      (Array.concat $ unsafePartial $ locs.spec.arrangeDerivTermSubs unit
            {mb_parent: Nothing, renCtx, rule, sort: getSortFromSub rule sigma, sigma, dzipper: Nothing,
                renderTerm: renderDerivTerm (trivialEditorLocals locs.spec) false false} <#> case _ of
        Left (renCtx' /\ kidIx) -> assert (just "renderSSTerm" (Array.index kidElems kidIx)) \kid -> [applyCssClasses renCtx'.cssClasses (kid renCtx')]
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
  other -> bug ("failed pattern matchin in renderSSTerm: " <> pretty other)

