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
import Data.Expr (wellformedExpr, (%))
import Data.Expr as Expr
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Zippable as Zippable
import Halogen.HTML (div, slot, slot_, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities (classNames)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Language.Pantograph.Generic.Rendering.Preview (previewComponent)
import Language.Pantograph.Generic.Smallstep (SSTerm, StepExprLabel(..))
import Text.Pretty (pretty)
import Type.Direction (Up, leftDir, rightDir)
import Util (fromJust')

------------------------------------------------------------------------------
-- arrange
------------------------------------------------------------------------------

arrangeDerivTermSubs :: forall l r. IsRuleLabel l r => 
  EditorLocals l r ->
  DerivZipper l r ->
  Array (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  Array (EditorHTML l r)
arrangeDerivTermSubs locs (Expr.Zipper dpath dterm) kidCtxElems renCtx = assert (wellformedExpr "arrangeDerivTermSubs" dterm) \_ -> case dterm of
  DerivLabel r sort % [] | isHoleRule r ->
    arrangeHoleExterior locs sort (renderHoleInterior locs false dpath sort) renCtx
  DerivLabel rule sort % kids -> do
    let subCtxSymElems = locs.spec.arrangeDerivTermSubs {renCtx, rule, sort, kids}
    Array.concat $ subCtxSymElems <#> case _ of
      Left (renCtx' /\ kidIx) -> assert (just "arrangeDerivTermSubs" (Array.index kidCtxElems kidIx)) \kidElem -> [kidElem renCtx']
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
arrangeNodeSubs locs isCursor hdzipper subElems = Array.concat
  [ if not isCursor then [] else 
    [ HH.slot bufferSlot unit bufferComponent 
        { hdzipper
        , edits: editsAtHoleyDerivZipper locs.spec hdzipper <#>
            \edit -> 
              { lazy_preview: renderEditPreview locs hdzipper edit
              , edit }
        }
        locs.handleBufferOutput
    , HH.slot_ previewSlot leftDir previewComponent leftDir
    ]
  , subElems
  , if not isCursor then [] else
    [ HH.slot_ previewSlot rightDir previewComponent rightDir ]
  ]

arrangeHoleExterior :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Sort l ->
  (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  Array (EditorHTML l r)
arrangeHoleExterior locs sort holeInteriorElem renCtx =
  [ HH.div [classNames ["subnode", "holeExterior-inner"]]
    [ HH.div [classNames ["subnode", "hole-interior"]] [holeInteriorElem renCtx]
    , colonElem
    , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
    ]
  ]


------------------------------------------------------------------------------
-- render term
------------------------------------------------------------------------------

renderDerivTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Boolean ->
  DerivZipper l r ->
  RenderingContext ->
  EditorHTML l r
renderDerivTerm locs isCursor dzipper renCtx =
  HH.div
    (Array.concat
      [ [classNames $ ["node"] <> if isCursor then [cursorClassName] else []]
      , if not renCtx.isInteractive then [] else do
        let elemId = fromPathToElementId (Expr.zipperPath dzipper)
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (locs.onMouseOver (InjectHoleyDerivZipper dzipper)) 
        ]
      ])
    (arrangeNodeSubs locs isCursor (InjectHoleyDerivZipper dzipper) 
      (arrangeDerivTermSubs locs dzipper (Zippable.zipDowns dzipper <#> renderDerivTerm locs false) renCtx))

------------------------------------------------------------------------------
-- render hole exterior and interior
------------------------------------------------------------------------------

-- !TODO I think this should actually use arrangeDerivTermSubs somehow
renderHoleExterior :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivPath Up l r ->
  Sort l ->
  (RenderingContext -> EditorHTML l r) ->
  RenderingContext ->
  EditorHTML l r
renderHoleExterior locs dpath sort holeInteriorElem renCtx = assert (just "renderHoleInterior" (defaultDerivTerm sort)) \dterm ->
  HH.div
    (Array.concat 
      [ [classNames ["node"]]
      , if not renCtx.isInteractive then [] else do
        let dzipper = Expr.Zipper dpath dterm
        let elemId = fromPathToElementId (Expr.zipperPath dzipper)
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (locs.onMouseOver (InjectHoleyDerivZipper dzipper)) 
        ]
      ])
    (arrangeHoleExterior locs sort holeInteriorElem renCtx)

renderHoleInterior :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  Boolean ->
  DerivPath Up l r ->
  Sort l ->
  RenderingContext ->
  EditorHTML l r
renderHoleInterior locs isCursor dpath sort renCtx = do
  let hdzipper = HoleInteriorHoleyDerivZipper dpath sort
  HH.div
    (Array.concat
      [ [classNames $ ["node", "holeInterior"] <> if isCursor then [cursorClassName] else []]
      , if not renCtx.isInteractive then [] else do
        let dzipper = hdzipperDerivZipper hdzipper
        let elemId = fromHoleyDerivPathToElementId (HoleInteriorHoleyDerivPath dpath)
        [ HP.id elemId
        , HE.onMouseDown (locs.onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (locs.onMouseOver (InjectHoleyDerivZipper dzipper)) 
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
                let elemId = fromPathToElementId (Expr.zipperPath dzipper)
                [ HP.id elemId
                , HE.onMouseDown (locs.onMouseDown (InjectHoleyDerivZipper dzipper2))
                , HE.onMouseOver (locs.onMouseOver (InjectHoleyDerivZipper dzipper2)) 
                ]
              ]) 
            (arrangeNodeSubs locs false (InjectHoleyDerivZipper dzipper2)
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

-- !TODO maybe someday have this use rendering context
renderPreviewDerivTooth :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  DerivPath Up l r ->
  DerivTooth l r ->
  DerivTerm l r ->
  {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
renderPreviewDerivTooth locs up dtooth@(Expr.Tooth dl kidsPath) dterm = do
  let rule /\ sort = case dl of
        DerivLabel rule sort -> rule /\ sort
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

  let subCtxSymElems = locs.spec.arrangeDerivTermSubs {renCtx: previewRenderingContext, rule, sort, kids: Array.fromFoldable $ ZipList.unpathAround dterm kidsPath}
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

renderEditPreview :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  HoleyDerivZipper l r ->
  Edit l r ->
  Lazy (EditPreviewHTML l r)
renderEditPreview locs hdzipper edit = edit.action <#> case _ of
  FillAction {dterm} -> FillEditPreview $ renderPreviewDerivTerm locs (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
  ReplaceAction {dterm} -> ReplaceEditPreview $ renderPreviewDerivTerm locs (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
  WrapAction {dpath} -> WrapEditPreview $ renderPreviewDerivPath locs (hdzipperDerivPath hdzipper) dpath (hdzipperDerivTerm hdzipper)

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
      previewRenderingContext)

renderSSTerm :: forall l r. IsRuleLabel l r =>
  EditorLocals l r ->
  SSTerm l r ->
  EditorHTML l r
renderSSTerm locs = assertInput_ (wellformedExpr "renderSSTerm") case _ of
  Inject dl % kids -> hole "TODO"
  Cursor % [kid] -> do
    let kidElem = renderSSTerm locs kid
    HH.div
      [classNames ["node", "smallstep", "cursor"]]
      [kidElem]
  Boundary dir sortCh % [kid] -> hole "TODO"