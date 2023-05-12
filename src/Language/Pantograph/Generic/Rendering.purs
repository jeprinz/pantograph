module Language.Pantograph.Generic.Rendering where

import Data.CodePoint.Unicode
import Data.Tuple
import Data.Tuple.Nested
import Prelude
import Type.Direction
import Bug (bug)
import Data.Array as Array
import Data.Expr (class IsExprExprLabel, Expr(..), Path, Zipper(..), Zipper', prettyPath, unTooth, zipDowns, zipDownsTooth, zipUp)
import Data.Foldable (foldMap)
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, defer, force)
import Data.List.Rev as RevList
import Data.List.Zip as ZipList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Rational as Rational
import Data.String as String
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
import Language.Pantograph.Generic.ZipperMovement (moveZipper)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Text.Pretty (class Pretty, pretty)
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
  | MoveBufferQuery MoveDir a
  | SubmitBufferQuery a

type Edit l =
  { ExprLabel :: String
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

instance IsExprExprLabel l => Pretty (State l) where
  pretty = case _ of
    BufferState buffer -> Array.intercalate "\n"
      [ "buffer:"
      , "  - zipper = " <> pretty buffer.zipper
      ]
    CursorState cursor -> Array.intercalate "\n"
      [ "cursor:"
      , "  - zipper = " <> pretty cursor.zipper
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
  Eq l => Ord l => IsExprExprLabel l =>
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
  _ /\ maybeCursorPath_ref <- HK.useRef (Just (unwrap input.zipper).path)
  _ /\ pathElementIds_ref <- HK.useRef (Map.empty :: Map (Path Dir.Up l) String)

  let 
    getElementIdByPath :: Path Dir.Up l -> HK.HookM Aff String
    getElementIdByPath path = do
      pathElementIds <- liftEffect $ Ref.read pathElementIds_ref
      case Map.lookup path pathElementIds of
        Nothing -> bug $ "could not find element id for path: " <> prettyPath path "{}"
        Just elemId -> pure elemId

    getElementByPath :: Path Dir.Up l -> HK.HookM Aff DOM.Element
    getElementByPath path = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByPath path
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> bug $ "could not find element by id: " <> elemId
        Just elem -> pure elem

    setNodeElementStyle :: Ref.Ref (Maybe (Path Dir.Up l)) -> String -> Maybe (Path Dir.Up l) -> HK.HookM Aff Unit
    setNodeElementStyle mb_path_ref className mb_path = do
      -- unset the current node element
      liftEffect (Ref.read mb_path_ref) >>= case _ of
        Nothing -> do
          -- set the new node element
          update
        Just path -> do
          -- ignore if the new cursor is the same as the old cursor
          if Just path == mb_path then do
            pure unit
          else do
            elem <- getElementByPath path
            liftEffect $ setClassName elem className false
            -- set the new node element
            update
      where
      update = do
        liftEffect $ Ref.write mb_path mb_path_ref
        case mb_path of
          Nothing -> pure unit
          Just path -> do
            elem' <- getElementByPath path
            liftEffect $ setClassName elem' className true

    setCursorElement = setNodeElementStyle maybeCursorPath_ref cursorClassName
    setHighlightElement = setNodeElementStyle maybeHighlightPath_ref highlightClassName

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
          setCursorElement (Just (unwrap cursor.zipper).path)
        BufferState buffer -> do
          setCursorElement (Just (unwrap buffer.zipper).path)
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
      CursorState cursor -> do
        -- Debug.traceM $ "[moveCursor] st = " <> pretty st.zipper
        case moveZipper dir cursor.zipper of
          Nothing -> pure unit
          Just zipper -> setFacade $ CursorState cursor {zipper = zipper}
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
      let shiftKey = KeyboardEvent.shiftKey event
      getFacade >>= case _ of
        BufferState buffer -> do
          if key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath (unwrap buffer.zipper).path
            HK.tell tokens.slotToken bufferSlot elemId SubmitBufferQuery
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            elemId <- getElementIdByPath (unwrap buffer.zipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery false
            -- -- set facade to cursor
            -- setFacade $ CursorState {zipper: buffer.zipper}
          else if key == "ArrowUp" then do
            -- Debug.traceM $ "[moveBufferQuery Up]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath (unwrap buffer.zipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery upDir
          else if key == "ArrowDown" then do
            -- Debug.traceM $ "[moveBufferQuery Down]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByPath (unwrap buffer.zipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery downDir
          else pure unit
        CursorState cursor -> do
          if  key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            elemId <- getElementIdByPath (unwrap cursor.zipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true
          else if key == "ArrowUp" then (if shiftKey then moveSelect else moveCursor) upDir
          else if key == "ArrowDown" then (if shiftKey then moveSelect else moveCursor) downDir
          else if key == "ArrowLeft" then (if shiftKey then moveSelect else moveCursor) leftDir
          else if key == "ArrowRight" then (if shiftKey then moveSelect else moveCursor) rightDir
          else pure unit
        SelectState select -> do
          if key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- !TODO select --> cursor
            -- !TODO activate buffer
            pure unit
          else if key == "ArrowUp" then (if shiftKey then moveSelect else moveCursor) upDir
          else if key == "ArrowDown" then (if shiftKey then moveSelect else moveCursor) downDir
          else if key == "ArrowLeft" then (if shiftKey then moveSelect else moveCursor) leftDir
          else if key == "ArrowRight" then (if shiftKey then moveSelect else moveCursor) rightDir
          else pure unit
        TopState _ -> unsafeCrashWith "!TODO handle keyboard event in TopState"

    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
      UpdateFacadeOutput f -> setFacade =<< (f =<< getFacade)
  
    renderExpr isCursor zipper = do
      let 
        elemId = unsafePerformEffect do
          -- generate new elemId and add it to the map
          elemId_ <- UUID.toString <$> UUID.genUUID
          Ref.modify_ (Map.insert (unwrap zipper).path elemId_) pathElementIds_ref
          pure elemId_

        clsNames /\ kidElems = input.renderExprKids (unwrap zipper).expr $ renderExpr false <$> zipDowns zipper
      HH.div
        [ classNames $ ["node"] <> clsNames <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setFacade $ CursorState {zipper: zipper}
        , HE.onMouseOver \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setHighlightElement (Just (unwrap zipper).path)
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
                Ref.modify_ (Map.insert (unwrap zipper2).path elemId_) pathElementIds_ref
                pure elemId_
          let clsNames /\ kidElems = 
                input.renderExprKids (unTooth th (unwrap zipper1).expr) $
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
                  setHighlightElement (Just (unwrap zipper2).path)
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
    --       [ "zipper = " <> pretty zipper
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

bufferComponent :: forall l. IsExprExprLabel l => H.Component (Query l) (BufferInput l) (Output l) Aff
bufferComponent = HK.component \tokens input -> HK.do
  isEnabled /\ isEnabled_id <- HK.useState false
  bufferString /\ bufferString_id <- HK.useState ""
  bufferFocus /\ bufferFocus_id <- HK.useState 0

  let bufferInputRefExprLabelString = "buffer-input"

  edits <- HK.captures {zipper: input.zipper, bufferString} $ flip HK.useMemo \_ ->
    input.edits #
      -- memo fuzzy distances
      map (\edit -> Fuzzy.matchStr false bufferString edit.ExprLabel /\ edit) >>>
      -- filter out edits that are below a certain fuzzy distance from the edit ExprLabel
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
          HK.getHTMLElementRef (H.RefExprLabel bufferInputRefExprLabelString) >>= case _ of 
            Nothing -> bug $ "[bufferComponent.useQuery] could not find element with ref ExprLabel: " <> bufferInputRefExprLabelString
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
        -- case qm of
        --   Up -> HK.modify_ bufferFocus_id (_ - 1)
        --   Down -> HK.modify_ bufferFocus_id (_ + 1)
        let _ = unsafeCrashWith "TODO"
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
    --       [ "zipper = " <> pretty input.zipper
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
                , HP.ref $ H.RefExprLabel "buffer-input"
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
