module Language.Pantograph.Generic.Rendering where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Gram (class GramLabel, Expr, Gram(..), MetaExpr, Path(..), Zipper, Path1, prettyPathUp, prettyZipper, unTooth, zipDowns, zipDownsTooth, zipUp)
import Data.Lazy (Lazy, defer, force)
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Rational as Rational
import Data.UUID as UUID
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, fromInputEventToTargetValue, setClassName, setClassNameByElementId)
import Language.Pantograph.Generic.ZipperMovement (MoveDir(..), moveZipper)
import Partial.Unsafe (unsafeCrashWith)
import Text.Pretty (class Pretty, pretty)
import Type.Direction (Dir(..), Down, Up)
import Type.Direction as Dir
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Common as HH
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.MouseEvent as MouseEvent

cursorClassName = "cursor"
highlightClassName = "highlight"

data Output l
  = ActionOutput (Action l)

data Query l a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean (Mode -> a)
  | MoveBufferQuery Dir a
  | SubmitBufferQuery a

type Edit l =
  { label :: String
  , preview :: String
  , action :: Action l
  }

data Action l
  = SetZipperAction (Lazy (Maybe (Zipper l)))

data Mode
  = BufferMode
  | CursorMode 
  | SelectMode 

data ModeState l
  = BufferModeState (BufferModeState l)
  | CursorModeState (CursorModeState l)
  | SelectModeState (SelectModeState l)
type BufferModeState l =
  { zipper :: Zipper l
  }
type CursorModeState l =
  { zipper :: Zipper l
  }
type SelectModeState l = 
  { dir :: Dir.Dir
  , pathAbove :: Path1 l
  , pathBelow :: Path1 l
  , expr :: Expr l 
  }

editorComponent :: forall q l.
  Eq l => Ord l => GramLabel l =>
  H.Component 
    q
    { zipper :: Zipper l
    , getEdits :: Zipper l -> Array (Edit l)
    , renderExprKids ::
        Expr l -> 
        Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query l) (Output l) String) Aff) -> 
        Array String /\ Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query l) (Output l) String) Aff)
    }
    Unit
    Aff
editorComponent = HK.component \tokens input -> HK.do
  
  zipper /\ zipper_id <- HK.useState input.zipper
  _ /\ maybeCursorZipper_ref <- HK.useRef (Just input.zipper)
  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing
  _ /\ pathElementIds_ref <- HK.useRef (Map.empty :: Map (Path Dir.Up l) String)
  _ /\ mode_ref <- HK.useRef CursorMode

  let 
    getElementIdByPath :: Path Dir.Up l -> HK.HookM Aff String
    getElementIdByPath path = do
      pathElementIds <- liftEffect $ Ref.read pathElementIds_ref
      case Map.lookup path pathElementIds of
        Nothing -> unsafeCrashWith $ "could not find element id for path: " <> prettyPathUp path "{}"
        Just elemId -> pure elemId

  let 
    getElementByPath :: Path Dir.Up l -> HK.HookM Aff DOM.Element
    getElementByPath path = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByPath path
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> unsafeCrashWith $ "could not find element by id: " <> elemId
        Just elem -> pure elem

  let 
    setCursor :: Maybe (Zipper l) -> HK.HookM Aff Unit
    setCursor maybeCursorZipper' = do
      -- unset the current cursor
      liftEffect (Ref.read maybeCursorZipper_ref) >>= case _ of
        Nothing -> do
          -- set the new cursor
          update
        Just cursorZipper -> do
          -- Debug.traceM $ "[setCursor] (old) cursorZipper = " <> prettyZipper cursorZipper
          -- ignore if the new cursor is the same as the old cursor
          if Just cursorZipper == maybeCursorZipper' then do
            -- Debug.traceM $ "[setCursor] no cursor update"
            pure unit
          else do
            -- Debug.traceM $ "[setCursor] unset old cursor"
            elem <- getElementByPath cursorZipper.path
            liftEffect $ setClassName elem cursorClassName false
            -- set the new cursor
            update
      where
      update = do
        liftEffect $ Ref.write maybeCursorZipper' maybeCursorZipper_ref
        case maybeCursorZipper' of
          Nothing -> pure unit
          Just cursorZipper' -> do
            Debug.traceM $ "[setCursor] (new) cursorZipper' = " <> prettyZipper cursorZipper'
            liftEffect $ Console.log "[setCursor] set new cursor"
            elem' <- getElementByPath cursorZipper'.path
            liftEffect $ setClassName elem' cursorClassName true

  let
    setHighlight :: Maybe (Path Dir.Up l) -> HK.HookM Aff Unit
    setHighlight maybePath' = do
      -- unset the current highlight
      liftEffect (Ref.read maybeHighlightPath_ref) >>= case _ of
        Nothing -> 
          -- set the new highlight
          update
        Just path -> do
          -- ignore if new highlight is same as old highlight
          if Just path == maybePath' then do
            -- Debug.traceM "[setHighlight] no highlight update"
            pure unit
          else do
            -- Debug.traceM "[setHighlight] unset old highlight"
            elem <- getElementByPath path
            liftEffect $ setClassName elem highlightClassName false
            -- set the new highlight
            update
      where
      update = do
        liftEffect $ Ref.write maybePath' maybeHighlightPath_ref
        case maybePath' of
          Nothing -> pure unit
          Just path' -> do
            -- Debug.traceM "[setHighlight] set new highlight"
            elem' <- getElementByPath path'
            liftEffect $ setClassName elem' highlightClassName true

  let
    requestActiveBuffer :: forall a. ((a -> a) -> Query l a) -> HK.HookM Aff (Maybe a)
    requestActiveBuffer req = do
      liftEffect (Ref.read maybeCursorZipper_ref) >>= case _ of
        Nothing -> pure Nothing
        Just cursorZipper -> do
          elemId <- getElementIdByPath cursorZipper.path
          HK.request tokens.slotToken bufferSlot elemId req

  let
    tellActiveBuffer msg = do
      liftEffect (Ref.read maybeCursorZipper_ref) >>= case _ of
        Nothing -> pure unit
        Just cursorZipper -> do
          elemId <- getElementIdByPath cursorZipper.path
          HK.tell tokens.slotToken bufferSlot elemId msg

  let 
    setBufferEnabled isEnabled = do
      -- Debug.traceM $ "[setBufferEnabled]"
      requestActiveBuffer (SetBufferEnabledQuery isEnabled) >>= case _ of
        Nothing -> pure unit
        Just mode' -> do
          liftEffect $ Ref.write mode' mode_ref
          pure unit

  let
    submitBuffer = do
      -- Debug.traceM $ "[submitBuffer]"
      tellActiveBuffer SubmitBufferQuery

  let
    handleAction = case _ of
      SetZipperAction lazy_zipper -> do
        -- compute new zipper
        case force lazy_zipper of
          Nothing -> pure unit
          Just zpr -> do
            setCursor Nothing -- reset cursor
            liftEffect $ Ref.write CursorMode mode_ref -- reset mode
            HK.put zipper_id zpr -- set new zipper
            setCursor (Just zpr) -- set new cursor

  let moveCursor dir = do
        liftEffect (Ref.read maybeCursorZipper_ref) >>= case _ of
          Nothing -> pure unit
          Just zpr -> case moveZipper dir zpr of
            Nothing -> pure unit
            Just zpr' -> setCursor (Just zpr')

  let
    handleKeyboardEvent :: KeyboardEvent.KeyboardEvent -> HK.HookM Aff Unit
    handleKeyboardEvent event = do
      -- !TODO keyboard actions
      -- Console.log $ "[event.key] " <> KeyboardEvent.key event
      mode <- liftEffect $ Ref.read mode_ref
      let key = KeyboardEvent.key event
      case mode of
        BufferMode -> do
          if key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            submitBuffer
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            setBufferEnabled false
          else if key == "ArrowUp" then do
            -- Debug.traceM $ "[moveBufferQuery Up]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            tellActiveBuffer $ MoveBufferQuery Up
          else if key == "ArrowDown" then do
            -- Debug.traceM $ "[moveBufferQuery Down]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            tellActiveBuffer $ MoveBufferQuery Down
          else pure unit
        CursorMode -> do
          if key == " " then
            setBufferEnabled true
          else if key == "ArrowUp" then moveCursor MoveUp
          else if key == "ArrowDown" then moveCursor MoveDown
          else if key == "ArrowLeft" then moveCursor MoveLeft
          else if key == "ArrowRight" then moveCursor MoveRight
          else pure unit
        SelectMode -> do
          unsafeCrashWith "TODO"

  HK.useLifecycleEffect do
    -- initialize
    doc <- liftEffect $ HTML.window >>= Window.document
    kbdSubId <- HK.subscribe do
      HQ.eventListener
        EventTypes.keydown
        (HTMLDocument.toEventTarget doc)
        (KeyboardEvent.fromEvent >>> map handleKeyboardEvent)

    pure $ Just do
      -- finalize
      HK.unsubscribe kbdSubId

  let 
    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
  
  let 
    renderExpr isCursor zpr = do
      let elemId = unsafePerformEffect do
            -- generate new elemId and add it to the map
            elemId_ <- UUID.toString <$> UUID.genUUID
            Ref.modify_ (Map.insert zpr.path elemId_) pathElementIds_ref
            pure elemId_
      let clsNames /\ kidElems = input.renderExprKids zpr.expr $ renderExpr false <$> zipDowns zpr
      HH.div
        [ classNames $ ["node"] <> clsNames <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setCursor (Just zpr)
        , HE.onMouseOver \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setHighlight (Just zpr.path)
            pure unit
        ] $
        Array.concat
        [ [ HH.slot bufferSlot elemId bufferComponent 
            { zipper: zpr
            , edits: input.getEdits zpr
            } 
            handleBufferOutput
          ]
        , kidElems
        ]

  let
    renderPath zipper1 interior  = do
      case zipUp zipper1 of
        Nothing -> interior
        Just (th /\ zipper2) -> do
          let elemId = unsafePerformEffect do
                -- generate new elemId and add it to the map
                elemId_ <- UUID.toString <$> UUID.genUUID
                Ref.modify_ (Map.insert zipper2.path elemId_) pathElementIds_ref
                pure elemId_
          let clsNames /\ kidElems = 
                input.renderExprKids (unTooth th zipper1.expr) $
                Array.fromFoldable $
                ZipList.unpathAround interior $ do
                  let kidZippers = zipDownsTooth zipper2 th
                  let ZipList.Path p = kidZippers
                  renderExpr false <$> kidZippers
          renderPath zipper2 $
            HH.div
              [ classNames $ ["node"] <> clsNames
              , HP.id elemId
              , HE.onMouseDown \event -> do
                  H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                  setCursor (Just zipper2)
              , HE.onMouseOver \event -> do
                  H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                  setHighlight (Just zipper2.path)
                  pure unit
              ] $
              Array.concat
              [ [ HH.slot bufferSlot elemId bufferComponent 
                  { zipper: zipper2
                  , edits: input.getEdits zipper2
                  } 
                  handleBufferOutput
                ]
              , kidElems
              ]

  HK.pure $
    -- Debug.trace 
    --   ("[editorComponent.render]" <> Array.foldMap ("\n" <> _)
    --     (map ("  - " <> _)
    --       [ "zipper = " <> prettyZipper zipper
    --       ]))
    --   \_ ->
      HH.div [classNames ["editor"]]
      [ HH.div 
        [ classNames ["program"]
        , HE.onMouseLeave \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setHighlight Nothing
        ]
        [ renderPath zipper $ renderExpr true zipper
        ]
      ]

bufferSlot = Proxy :: Proxy "buffer"

type BufferInput l =
  { zipper :: Zipper l
  , edits :: Array (Edit l)
  }

bufferComponent :: forall l. GramLabel l => H.Component (Query l) (BufferInput l) (Output l) Aff
bufferComponent = HK.component \tokens input -> HK.do
  isEnabled /\ isEnabled_id <- HK.useState false
  bufferString /\ bufferString_id <- HK.useState ""
  bufferFocus /\ bufferFocus_id <- HK.useState 0

  let bufferInputRefLabelString = "buffer-input"

  edits <- HK.captures {zipper: input.zipper, bufferString} $ flip HK.useMemo \_ ->
    input.edits #
      -- memo fuzzy distances
      map (\edit -> Fuzzy.matchStr false bufferString edit.label /\ edit) >>>
      -- filter out edits that are below a certain fuzzy distance from the edit label
      Array.filter (\(FuzzyStr fs /\ _) -> Rational.fromInt 0 < fs.ratio) >>>
      -- sort the remaining edits by the fuzzy distance
      Array.sortBy (\(fuzzyStr1 /\ _) (fuzzyStr2 /\ _) -> compare fuzzyStr1 fuzzyStr2) >>>
      -- forget fuzzy distances
      map snd

  let normalBufferFocus = bufferFocus `mod` Array.length edits

  HK.useQuery tokens.queryToken case _ of
    SetBufferEnabledQuery isEnabled' k -> do
      HK.put isEnabled_id isEnabled' -- update isEnabled
      HK.put bufferFocus_id 0 -- reset bufferFocus
      when isEnabled' do
          -- focus buffer input tag
          HK.getHTMLElementRef (H.RefLabel bufferInputRefLabelString) >>= case _ of 
            Nothing -> unsafeCrashWith $ "[bufferComponent.useQuery] could not find element with ref label: " <> bufferInputRefLabelString
            Just elem -> liftEffect $ HTMLElement.focus elem
          pure unit
      pure $ Just $ k 
        if isEnabled' then BufferMode
        else CursorMode
    MoveBufferQuery qm a -> do
      if isEnabled then do
        case qm of
          Up -> HK.modify_ bufferFocus_id (_ - 1)
          Down -> HK.modify_ bufferFocus_id (_ + 1)
        pure $ Just a
      else
        pure Nothing
    SubmitBufferQuery a -> do
      if isEnabled then do
        case Array.index edits normalBufferFocus of
          Nothing -> do
            liftEffect $ Console.log $ "[bufferComponent.SubmitBufferQuery] attempted to submit buffer, but bufferFocus is out of range: \n  - length edits = " <> show (Array.length edits) <> "\n  - bufferFocus = " <> show bufferFocus 
            pure Nothing
          Just edit -> do
            HK.put isEnabled_id false -- disable query
            HK.put bufferFocus_id 0 -- reset bufferFocus
            HK.raise tokens.outputToken $ ActionOutput edit.action -- output edit action
            pure $ Just a
      else
        pure Nothing
    -- _ -> unsafeCrashWith "TODO"

  HK.pure $
    -- Debug.trace 
    --   ("[bufferComponent.render]" <> Array.foldMap ("\n" <> _)
    --     (map ("  - " <> _)
    --       [ "zipper = " <> prettyZipper input.zipper
    --       , "isEnabled = " <> show isEnabled
    --       ]))
    --   \_ ->
      HH.div
        [ classNames ["subnode", "buffer"]
        ] $ 
        Array.concat
        [ if not isEnabled then [] else
          [ HH.div [classNames ["inner"]]
              [ HH.input 
                [ classNames ["buffer-input"]
                , HP.autofocus true 
                , HP.ref $ H.RefLabel "buffer-input"
                , HP.type_ HP.InputText
                , HE.onInput \event -> do
                    bufferString' <- liftEffect $ fromInputEventToTargetValue event
                    HK.put bufferString_id bufferString'
                    -- !TODO reset bufferFocus to 0
                    pure unit
                ]
              , HH.div
                [ classNames ["buffer-results"]
                ] $
                flip Array.mapWithIndex edits \i edit -> 
                  HH.div 
                    [classNames $ ["buffer-result"] <> if i == normalBufferFocus then ["buffer-focus"] else []]
                    [HH.text edit.preview]
              ]
          ]
        ]

--
-- utilities
--
