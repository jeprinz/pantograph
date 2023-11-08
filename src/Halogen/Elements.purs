module Halogen.Elements where

import Prelude

import Bug (bug)
import CSS (CSS)
import DOM.HTML.Indexed as HTML
import Data.Array as Array
import Data.Display (Html)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Subtype (class Subtype)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UUID (UUID)
import Data.UUID as UUID
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities as HU
import Todo (todo)
import Util (fromJust)
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element as Element
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

-- Prop

type Props i = Array (Prop i)

data Prop i
  = Classes (Array ClassName)
  | Id HU.ElementId
  | Ref H.RefLabel
  | OnMouseDown (MouseEvent -> i)
  | OnMouseUp (MouseEvent -> i)
  | OnMouseOver (MouseEvent -> i)
  | OnMouseOut (MouseEvent -> i)
  | StrictHover (MouseEvent -> i)
  | StyleCSS CSS
  | Style String

-- TODO: combine multiple onMouseDown, onMouseUp, etc.
compileProps :: forall i. Props i -> Array (HP.IProp HTML.HTMLdiv i)
compileProps props = Array.foldMap compileProp props

compileProp :: forall i. Prop i -> Array (HP.IProp HTML.HTMLdiv i)
compileProp (Id elemId) = [ HU.id elemId ]
compileProp (Ref refLabel) = [ HP.ref refLabel ]
compileProp (Classes classNames) = [HP.classes $ Array.foldMap compileClassName classNames]
compileProp (OnMouseDown k) = 
  [ HE.onMouseDown \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      pure $ k mouseEvent
  ]
compileProp (OnMouseUp k) =
  [ HE.onMouseUp \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      pure $ k mouseEvent
  ]
compileProp (StrictHover k) = do
  [ HE.onMouseOver \mouseEvent -> unsafePerformEffect do

      -- Debug.traceM "[StringHover] onMouseOver"
      -- Debug.traceM "[BEGIN] mouseEvent"
      -- Debug.traceM mouseEvent
      -- Debug.traceM "[END] mouseEvent"

      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      -- TODO: why do I have to take the parent??? no idea
      let target = case Event.target event >>= Element.fromEventTarget >>= HU.parentElement of
            Nothing -> bug "[StrictHover] invalid target"
            Just target_ -> target_
      updateElementClassName target Hover (Just true)
      pure $ k mouseEvent
  , HE.onMouseOut \mouseEvent -> unsafePerformEffect do
      -- Debug.traceM "[StringHover] onMouseOut"
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      let target = case Event.target event >>= Element.fromEventTarget >>= HU.parentElement of
            Nothing -> bug "[StrictHover] invalid target"
            Just target_ -> target_
      updateElementClassName target Hover (Just false)
      pure $ k mouseEvent
  ]
compileProp (OnMouseOver k) =
  [ HE.onMouseOver \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      pure $ k mouseEvent
  ]
compileProp (OnMouseOut k) =
  [ HE.onMouseOut \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      pure $ k mouseEvent
  ]
compileProp (StyleCSS css) = [ HCSS.style css ]
compileProp (Style str) = [ HP.style str ]

-- ClassName

data ClassName
  = ClassName String
  -- String
  | Whitespace | Punctuation
  -- Panel
  | Panel | EditorPanel | BufferPanel | TerminalPanel
  | PanelHeader | PanelHeaderInfo | PanelHeaderControl 
  | PanelContent | PanelContentInterior
  -- UI
  | Title | Subtitle | Button | Program
  -- Plain
  | Inline | Block | Matrix
  -- Language
  | Expr
  | VarSN 
  | Hole
  | VarRuleSortNode
  | RuleSortVar
  | SortVar | SortVarLabel | SortVarUuid
  | SortingRule | SortingRuleParameters | SortingRuleKids | SortingRuleParent
  | ArrangeHtml
  -- StepExpr
  | StepExpr | StepExprBoundary | StepExprMarker
  | StepExprBoundaryInfo | StepExprBoundaryDirection | StepExprBoundaryChange
  -- Preview
  | PreviewExpr | Preview 
  | PreviewLeft | PreviewLeftInsert | PreviewLeftPaste
  | PreviewRight | PreviewRightInsert | PreviewRightPaste
  -- Toolbox
  | ToolboxExpr | AdjacentEditClasp
  | Toolbox | ToolboxInterior | ToolboxInput | EditRows | Edit
  | EditRow | SelectedEditRow
  | EditHole
  -- Buffer
  | Cursor | OutsideCursor | InsideCursor
  | Select | OutsideSelect | InsideSelect
  -- BufferInfo
  | BufferInfo | BufferInfoItem | BufferInfoItemTitle | BufferInfoItemValue
  -- Tree
  | Change | ShiftChange | ShiftChangeInner | ReplaceChange | ReplaceChangeLeft 
  | ReplaceChangeRight | InjectChange
  -- Terminal
  | TerminalContent
  | GlobalMessageTag | DebugGlobalMessageTag | ErrorGlobalMessageTag | InfoGlobalMessageTag
  | GlobalMessages | GlobalMessage | GlobalMessageContent
  | GlobalMessageRecordKey
  -- Hover
  | Hover
  -- Misc
  | Closed | Bug | UuidSplotch

derive instance Generic ClassName _
derive instance Eq ClassName
derive instance Ord ClassName
instance Show ClassName where 
  show (ClassName string) = string
  show cn = genericShow cn

instance Subtype ClassName HH.ClassName where
  inject = HH.ClassName <<< show
  project = Just <<< ClassName <<< unwrap

ancestorClassNamesRelations :: Array (ClassName /\ Array ClassName)
ancestorClassNamesRelations =
  [ Expr /\ [PreviewExpr, ToolboxExpr, StepExpr]
  , ToolboxExpr /\ [AdjacentEditClasp]
  , Cursor /\ [OutsideCursor, InsideCursor]
  , Select /\ [OutsideSelect, InsideSelect]
  , Change /\ [ShiftChange, ReplaceChange, InjectChange]
  , Panel /\ [EditorPanel, BufferPanel, TerminalPanel]
  , Preview /\ [PreviewLeft, PreviewRight]
  , PreviewLeft /\ [PreviewLeftInsert, PreviewLeftPaste]
  , PreviewRight /\ [PreviewRightInsert, PreviewRightPaste]
  , EditRow /\ [SelectedEditRow]
  , GlobalMessageTag /\ [DebugGlobalMessageTag, ErrorGlobalMessageTag, InfoGlobalMessageTag]
  ]

ancestorClassNamesRelationsClosure :: ClassName -> Array ClassName
ancestorClassNamesRelationsClosure = Array.fromFoldable <<< go mempty
  where
  go cns cn = ancestorClassNamesRelations # Array.foldMap \(cnParent /\ cnKids) -> 
    if Array.elem cn cnKids && not (List.elem cnParent cns) 
      then go (cnParent List.: cns) cnParent 
      else cns

ancestorClassNamesRelationsMap :: Map ClassName (Array ClassName)
ancestorClassNamesRelationsMap =
  Map.fromFoldableWith (\cns1 cns2 -> Array.nub $ cns1 <> cns2) $
    ancestorClassNamesRelations # Array.foldMap \(cnParent /\ cnKids) -> cnKids <#> \cnKid -> cnKid /\ ([cnParent] <> ancestorClassNamesRelationsClosure cnParent)

ancestorClassNames :: ClassName -> Array ClassName
ancestorClassNames cn = fromMaybe [] $ Map.lookup cn ancestorClassNamesRelationsMap

compileClassName :: ClassName -> Array HH.ClassName
compileClassName className = 
  [HH.ClassName (show className)] <>
  (Array.foldMap compileClassName (ancestorClassNames className))

-- building elements    

element :: forall w i. Props i -> Array (HH.HTML w i) -> HH.HTML w i
element props = HH.div (compileProps props)

ℓ kids = element kids

whitespace string = ℓ [Classes [Whitespace]] [HH.text string]
ε string = whitespace string

punctuation string = ℓ [Classes [Punctuation]] [HH.text string]
π string = punctuation string

inline kids = ℓ [Classes [Inline]] kids
ι kids = inline kids

block kids = ℓ [Classes [Block]] kids
β kids = block kids

text string = HH.text string
τ string = text string

matrix rows = 
  HH.table [HP.classes [HH.ClassName "Matrix"]] $ rows <#> \row ->
    HH.tr_ $ row <#> \dat -> HH.td_ [dat]

br = HH.br_

-- manipulating elements

-- | Update the className of an element by ElementId
-- | toggle className: `updateClassName elemId className Nothing`
-- | add className: `updateClassName elemId className (Just true)`
-- | remove className: `updateClassName elemId className (Just false)`
updateClassName :: HU.ElementId -> ClassName -> Maybe Boolean -> Effect Unit
updateClassName elemId className mb_classValue = do
  elem <- HU.getElementById elemId
  updateElementClassName elem className mb_classValue

setClassName :: HU.ElementId -> ClassName -> Effect Unit
setClassName elemId className = do
  elem <- HU.getElementById elemId
  Element.setClassName (show className) elem

setClassNames :: HU.ElementId -> Array ClassName -> Effect Unit
setClassNames elemId classNames = do
  elem <- HU.getElementById elemId
  Element.setClassName (classNames # map show >>> Array.intercalate " ") elem

-- | Update the className of an Element
-- | toggle className: `updateClassName elem className Nothing`
-- | add className: `updateClassName elem className (Just true)`
-- | remove className: `updateClassName elem className (Just false)`
updateElementClassName :: Element.Element -> ClassName -> Maybe Boolean -> Effect Unit
updateElementClassName elem className mb_classValue = do
  classList <- Element.classList elem
  case mb_classValue of
    Nothing -> void $ DOMTokenList.toggle classList $ show className
    Just true -> void $ DOMTokenList.add classList $ show className
    Just false -> void $ DOMTokenList.remove classList $ show className

