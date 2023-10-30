module Halogen.Elements where

import Prelude

import Bug (bug)
import DOM.HTML.Indexed as HTML
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Subtype (class Subtype)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utilities as HU
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

-- TODO: combine multiple onMouseDown, onMouseUp, etc.
compileProps :: forall i. Props i -> Array (HP.IProp HTML.HTMLdiv i)
compileProps props = Array.foldMap compileProp props

compileProp :: forall i. Prop i -> Array (HP.IProp HTML.HTMLdiv i)
compileProp (Id elemId) = [ HU.id elemId ]
compileProp (Ref refLabel) = [ HP.ref refLabel ]
compileProp (Classes classNames) = [ HP.classes $ HH.ClassName <<< show <$> classNames ]
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
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      let target = case Event.target event >>= Element.fromEventTarget of
            Nothing -> bug "[StrictHover] invalid target"
            Just target_ -> target_
      updateElementClassName target Hover (Just true)
      pure $ k mouseEvent
  , HE.onMouseOut \mouseEvent -> unsafePerformEffect do
      let event = MouseEvent.toEvent mouseEvent
      Event.stopPropagation event
      let target = case Event.target event >>= Element.fromEventTarget of
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
  | Inline | Block
  -- Language
  | VarSN | Expr | Hole | ArrangeHtml
  -- StepExpr
  | StepExpr | Boundary | Marker
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
  -- Tree
  | Change | ShiftChange | ShiftChangeInner | ReplaceChange | ReplaceChangeLeft 
  | ReplaceChangeRight | InjectChange
  -- Terminal
  | TerminalContent | TerminalItemTag | DebugTerminalItemTag | TerminalItems | TerminalItem | TerminalItemContent
  -- Hover
  | Hover
  -- Misc
  | Closed

derive instance Generic ClassName _
derive instance Eq ClassName
derive instance Ord ClassName
instance Show ClassName where 
  show (ClassName string) = string
  show cn = genericShow cn

instance Subtype ClassName HH.ClassName where
  inject = HH.ClassName <<< show
  project = Just <<< ClassName <<< unwrap

ancestorClassNames :: ClassName -> Array ClassName
ancestorClassNames = \cn -> 
  case Map.lookup cn m of
    Nothing -> []
    Just cns -> cns
  where
  rels :: Array (ClassName /\ Array ClassName)
  rels =
    [ Expr /\ [PreviewExpr, ToolboxExpr]
    , ToolboxExpr /\ [AdjacentEditClasp]
    , Cursor /\ [OutsideCursor, InsideCursor]
    , Select /\ [OutsideSelect, InsideSelect]
    , Change /\ [ShiftChange, ReplaceChange, InjectChange]
    , Panel /\ [EditorPanel, BufferPanel, TerminalPanel]
    , Preview /\ [PreviewLeft, PreviewRight]
    , PreviewLeft /\ [PreviewLeftInsert, PreviewLeftPaste]
    , PreviewRight /\ [PreviewRightInsert, PreviewRightPaste]
    , EditRow /\ [SelectedEditRow]
    ]

  relClosure cn = rels # Array.foldMap \(cnParent /\ cnKids) -> if Array.elem cn cnKids then [cnParent] <> ancestorClassNames cnParent else []
  m = Map.fromFoldableWith (\cns1 cns2 -> Array.nub $ cns1 <> cns2) $
        rels # Array.foldMap \(cnParent /\ cnKids) -> cnKids <#> \cnKid -> cnKid /\ relClosure cnParent

compileClassName :: ClassName -> Array HH.ClassName
compileClassName className = 
  [HH.ClassName (show className)] <>
  (Array.foldMap compileClassName $ ancestorClassNames className)

-- building elements    

element :: forall w i. Props i -> Array (HH.HTML w i) -> HH.HTML w i
element props = HH.div (compileProps props)

ℓ = element

whitespace string = ℓ [Classes [Whitespace]] [HH.text string]
ε = whitespace

punctuation string = ℓ [Classes [Punctuation]] [HH.text string]
π = punctuation

inline = ℓ [Classes [Inline]]
ι = inline

block = ℓ [Classes [Block]]
β = block

text = HH.text
τ = text

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
