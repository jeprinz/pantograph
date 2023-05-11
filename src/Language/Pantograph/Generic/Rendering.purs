module Language.Pantograph.Generic.Rendering where

import Data.Tuple
import Data.Tuple.Nested
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Gram (class GramLabel, Expr, Gram(..), MetaExpr, Path(..), Path1, Zipper, Zipper', prettyPathUp, prettyZipper, unTooth, zipDowns, zipDownsTooth, zipUp)
import Data.Lazy (Lazy, defer, force)
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
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
  | UpdateFacadeOutput (State l -> HK.HookM Aff (State l))

data Query l a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean a
  | MoveBufferQuery Dir a
  | SubmitBufferQuery a

type Edit l =
  { label :: String
  , preview :: String
  , action :: Action l
  }

data Action l
  = SetZipperAction (Lazy (Maybe (Zipper l)))

data State l
  = BufferState (Buffer l)
  | CursorState (Cursor l)
  | SelectState (Select l)
  | TopState (Top l)

instance GramLabel l => Pretty (State l) where
  pretty = case _ of
    BufferState buffer -> Array.intercalate "\n"
      [ "buffer:"
      , "  - zipper = " <> prettyZipper buffer.zipper
      ]
    CursorState cursor -> Array.intercalate "\n"
      [ "cursor:"
      , "  - zipper = " <> prettyZipper cursor.zipper
      ]
    SelectState select -> Array.intercalate "\n"
      [ "select: !TODO"
      ]
    TopState top -> Array.intercalate "\n"
      [ "top:"
      ]

-- !TODO is it worth having a different type for frontend that has Buffer where State doesn't have Buffer?
-- data Facade l
--   = BufferFacade (Buffer l)
--   | CursorFacade (Cursor l)
--   | SelectFacade (Select l)
--   | TopFacade (Top l)

type Buffer l =
  { zipper :: Zipper l
  }

type Cursor l =
  { zipper :: Zipper l
  }

type Select l = 
  { zipper' :: Zipper' l
  }

type Top l =
  { expr :: Expr l
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
     -- TODO: factor out this type, and add: Grammar.Sorts, Grammar.Derivations, Grammar.Languaage, something for smallstep
    }
    Unit
    Aff
editorComponent = HK.component \tokens input -> HK.do

  let 
    initState = CursorState
      { zipper: input.zipper
      }

  currentState /\ state_id <- HK.useState $ initState
  _ /\ facade_ref <- HK.useRef $ initState

  let _ = Debug.trace ("[editorComponent] currentState: " <> pretty currentState) \_ -> unit

  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing
  _ /\ maybeCursorPath_ref <- HK.useRef (Just input.zipper.path)
  _ /\ pathElementIds_ref <- HK.useRef (Map.empty :: Map (Path Dir.Up l) String)

  let 
    getElementIdByPath :: Path Dir.Up l -> HK.HookM Aff String
    getElementIdByPath path = do
      pathElementIds <- liftEffect $ Ref.read pathElementIds_ref
      case Map.lookup path pathElementIds of
        Nothing -> bug $ "could not find element id for path: " <> prettyPathUp path "{}"
        Just elemId -> pure elemId

    getElementByPath :: Path Dir.Up l -> HK.HookM Aff DOM.Element
    getElementByPath path = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByPath path
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> bug $ "could not find element by id: " <> elemId
        Just elem -> pure elem

    setCursorElement :: Maybe (Path Dir.Up l) -> HK.HookM Aff Unit
    setCursorElement mb_path = do
      -- unset the current cursor
      liftEffect (Ref.read maybeCursorPath_ref) >>= case _ of
        Nothing -> do
          -- set the new cursor
          update
        Just path -> do
          -- ignore if the new cursor is the same as the old cursor
          if Just path == mb_path then do
            -- Debug.traceM $ "[setCursorElement] no cursor update"
            pure unit
          else do
            -- Debug.traceM $ "[setCursorElement] unset old cursor"
            -- Debug.traceM $ "[setCursorElement] old path is path = " <> prettyPathUp path "{{}}"
            elem <- getElementByPath path
            liftEffect $ setClassName elem cursorClassName false
            -- set the new cursor
            update
      where
      update = do
        liftEffect $ Ref.write mb_path maybeCursorPath_ref
        case mb_path of
          Nothing -> pure unit
          Just path -> do
            -- Debug.traceM "[setCursorElement] set new cursor"
            elem' <- getElementByPath path
            liftEffect $ setClassName elem' cursorClassName true

    setHighlightElement :: Maybe (Path Dir.Up l) -> HK.HookM Aff Unit
    setHighlightElement maybePath' = do
      -- unset the current highlight
      liftEffect (Ref.read maybeHighlightPath_ref) >>= case _ of
        Nothing -> 
          -- set the new highlight
          update
        Just path -> do
          -- ignore if new highlight is same as old highlight
          if Just path == maybePath' then do
            -- Debug.traceM "[setHighlightElement] no highlight update"
            pure unit
          else do
            -- Debug.traceM "[setHighlightElement] unset old highlight"
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
            -- Debug.traceM "[setHighlightElement] set new highlight"
            elem' <- getElementByPath path'
            liftEffect $ setClassName elem' highlightClassName true

    unsetFacadeElements :: HK.HookM Aff Unit
    unsetFacadeElements = getFacade >>= case _ of
      BufferState _st -> do
        setHighlightElement Nothing
        setCursorElement Nothing
      CursorState _st -> do
        setHighlightElement Nothing
        setCursorElement Nothing
      SelectState _st -> do
        setHighlightElement Nothing
        setCursorElement Nothing
      TopState _st -> do
        setHighlightElement Nothing

    setFacade :: State l -> HK.HookM Aff Unit
    setFacade st = do
      -- Debug.traceM $ "[setFacade] new state: " <> pretty st
      unsetFacadeElements
      setFacade' st

    -- doesn't `unsetFacadeElements` first
    setFacade' :: State l -> HK.HookM Aff Unit
    setFacade' st = do
      case st of
        CursorState cursor -> do
          setCursorElement (Just cursor.zipper.path)
        BufferState buffer -> do
          setCursorElement (Just buffer.zipper.path)
        SelectState select -> do
          unsafeCrashWith "!TODO setFacade SelectState"
        TopState top -> do
          setCursorElement Nothing
      liftEffect (Ref.write st facade_ref)

    getFacade :: HK.HookM Aff (State l)
    getFacade = liftEffect (Ref.read facade_ref)

    setState :: State l -> HK.HookM Aff Unit
    setState st = do
      unsetFacadeElements
      HK.put state_id st
      -- can use `setFacade'` since we already `unsetFacadeElements`
      setFacade' st

    handleAction = case _ of
      SetZipperAction lazy_zipper -> do
        -- compute new zipper
        case force lazy_zipper of
          Nothing -> pure unit
          Just zipper -> setState $ CursorState {zipper}

    moveCursor dir = getFacade >>= case _ of
      CursorState st -> do
        -- Debug.traceM $ "[moveCursor] st = " <> prettyZipper st.zipper
        case moveZipper dir st.zipper of
          Nothing -> pure unit
          Just zipper -> setFacade $ CursorState st {zipper = zipper}
      BufferState _st -> pure unit
      SelectState _st -> unsafeCrashWith "!TODO escape select then move cursor"
      TopState _st -> unsafeCrashWith "!TODO move to first top cursor position, then move"

    moveSelect dir = getFacade >>= case _ of
      CursorState cursor -> unsafeCrashWith "!TODO turn into selection with direction determined by dir"
      BufferState _ -> unsafeCrashWith "!TODO escape to cursor first"
      SelectState _ -> unsafeCrashWith "!TODO impl select movement"
      TopState _ -> unsafeCrashWith "!TODO move to first top cursor position, then move"

    handleKeyboardEvent :: KeyboardEvent.KeyboardEvent -> HK.HookM Aff Unit
    handleKeyboardEvent event = do
      -- Console.log $ "[event.key] " <> KeyboardEvent.key event
      let key = KeyboardEvent.key event
      getFacade >>= case _ of
        BufferState st -> do
          if key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath st.zipper.path
            HK.tell tokens.slotToken bufferSlot elemId SubmitBufferQuery
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            elemId <- getElementIdByPath st.zipper.path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery false
            -- set facade to cursor
            setFacade $ CursorState {zipper: st.zipper}
          else if key == "ArrowUp" then do
            -- Debug.traceM $ "[moveBufferQuery Up]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath st.zipper.path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery Up
          else if key == "ArrowDown" then do
            -- Debug.traceM $ "[moveBufferQuery Down]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath st.zipper.path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery Down
          else pure unit
        CursorState st -> do
          if key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            elemId <- getElementIdByPath st.zipper.path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true
          else if key == "ArrowUp" then moveCursor MoveUp
          else if key == "ArrowDown" then moveCursor MoveDown
          else if key == "ArrowLeft" then moveCursor MoveLeft
          else if key == "ArrowRight" then moveCursor MoveRight
          else pure unit
        SelectState _ -> unsafeCrashWith "!TODO handle keyboard event in SelectState"
        TopState _ -> unsafeCrashWith "!TODO handle keyboard event in TopState"

    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
      UpdateFacadeOutput f -> setFacade =<< (f =<< getFacade)
  
    renderExpr isCursor zipper = do
      let 
        elemId = unsafePerformEffect do
          -- generate new elemId and add it to the map
          elemId_ <- UUID.toString <$> UUID.genUUID
          Ref.modify_ (Map.insert zipper.path elemId_) pathElementIds_ref
          pure elemId_

        clsNames /\ kidElems = input.renderExprKids zipper.expr $ renderExpr false <$> zipDowns zipper
      HH.div
        [ classNames $ ["node"] <> clsNames <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setFacade $ CursorState {zipper: zipper}
        , HE.onMouseOver \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setHighlightElement (Just zipper.path)
            pure unit
        ] $
        Array.concat
        [ [ HH.slot bufferSlot elemId bufferComponent 
            { zipper: zipper
            , edits: input.getEdits zipper
            } 
            handleBufferOutput
          ]
        , kidElems
        ]

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
                  setFacade $ CursorState {zipper: zipper2}
              , HE.onMouseOver \event -> do
                  H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                  setHighlightElement (Just zipper2.path)
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
            setHighlightElement Nothing
        ]
        case currentState of
          CursorState st -> [renderPath st.zipper $ renderExpr true st.zipper]
          BufferState _st -> unsafeCrashWith "!TODO render BufferState"
          SelectState _st -> unsafeCrashWith "!TODO render SelectState"
          TopState _st -> unsafeCrashWith "!TODO render TopState"
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
    SetBufferEnabledQuery isEnabled' a -> do
      HK.put isEnabled_id isEnabled' -- update isEnabled
      HK.put bufferFocus_id 0 -- reset bufferFocus
      if isEnabled' then do
          -- focus buffer input tag
          HK.getHTMLElementRef (H.RefLabel bufferInputRefLabelString) >>= case _ of 
            Nothing -> bug $ "[bufferComponent.useQuery] could not find element with ref label: " <> bufferInputRefLabelString
            Just elem -> liftEffect $ HTMLElement.focus elem
          -- update facade to BufferState
          HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
            pure $ BufferState {zipper: input.zipper}
          pure unit
      else
        -- update facade to CursorState
        HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
          pure $ CursorState {zipper: input.zipper}
      pure (Just a)
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
                    HK.put bufferFocus_id 0 -- reset bufferFocus
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
