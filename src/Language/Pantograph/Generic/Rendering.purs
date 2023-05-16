module Language.Pantograph.Generic.Rendering where

import Data.CodePoint.Unicode
import Data.Either.Nested
import Data.Tuple
import Data.Tuple.Nested
import Language.Pantograph.Generic.Grammar
import Prelude
import Type.Direction

import Bug (bug)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Expr as Expr
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
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.UUID as UUID
import Data.Variant (case_, on)
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
import Language.Pantograph.Generic.ZipperMovement (moveZipper, moveZipperp)
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

cursorClassName :: String
cursorClassName = "cursor"
highlightClassName :: String
highlightClassName = "highlight"
selectTopClassName :: String
selectTopClassName = "select-top"
selectBottomClassName :: String
selectBottomClassName = "select-bottom"

isBufferKey = (_ `Array.elem` [" ", "Enter"])

data Output l r
  = ActionOutput (Action l r)
  | UpdateFacadeOutput (State l r -> HK.HookM Aff (State l r))

data Query l r a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean a
  | MoveBufferQuery VerticalDir a
  | SubmitBufferQuery a

type Edit l r =
  { label :: String
  , preview :: String
  , action :: Action l r
  }

data Action l r
  = SetDerivZipperAction (Lazy (Maybe (DerivZipper l r)))

data State l r
  = BufferState (Buffer l r)
  | CursorState (Cursor l r)
  | SelectState (Select l r)
  | TopState (Top l r)

derive instance Generic (State l r) _
instance (Show l, Show r) => Show (State l r) where show x = genericShow x

instance (Expr.IsExprLabel l, IsRuleLabel l r) => Pretty (State l r) where
  pretty = case _ of
    BufferState buffer -> Array.intercalate "\n"
      [ "buffer:"
      , "  - dzipper = " <> pretty buffer.dzipper
      ]
    CursorState cursor -> Array.intercalate "\n"
      [ "cursor:"
      , "  - dzipper = " <> pretty cursor.dzipper
      ]
    SelectState select -> Array.intercalate "\n"
      [ "select: !TODO"
      ]
    TopState top -> Array.intercalate "\n"
      [ "top:"
      ]

-- !TODO is it worth having a different type for frontend that has Buffer where State doesn't have Buffer?
-- data Facade l
--   = BufferFacade (Buffer l r)
--   | CursorFacade (Cursor l r)
--   | SelectFacade (Select l r)
--   | TopFacade (Top l r)

type Buffer l r =
  { dzipper :: DerivZipper l r
  }

type Cursor l r =
  { dzipper :: DerivZipper l r
  }

type Select l r =
  { dzipperp :: DerivZipperp l r
  }

type Top l r =
  { expr :: DerivExpr l r
  }

type EditorSpec l r =
  { dzipper :: DerivZipper l r
  , getEdits :: DerivZipper l r -> Array (Edit l r)
  , renderDerivExprKids ::
      DerivExpr l r -> 
      Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query l r) (Output l r) String) Aff) -> 
      Array String /\ Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query l r) (Output l r) String) Aff)
    -- TODO: factor out this type, and add: Grammar.Sorts, Grammar.Derivations, Grammar.Languaage, something for smallstep
  }

editorComponent :: forall q l r.
  IsRuleLabel l r =>
  H.Component 
    q
    (EditorSpec l r)
    Unit
    Aff
editorComponent = HK.component \tokens input -> HK.do

  let
    initState = CursorState
      { dzipper: input.dzipper
      }

  -- state
  currentState /\ state_id <- HK.useState $ initState
  let _ = Debug.trace ("[editorComponent] currentState: " <> pretty currentState) \_ -> unit

  -- facade state
  _ /\ facade_ref <- HK.useRef $ initState

  -- clipboard
  _ /\ clipboard_ref <- HK.useRef (Nothing :: Maybe (DerivPath Dir.Up l r \/ DerivExpr l r))

  -- !TODO OLD
  -- -- mapping: dpath ==> html elem id
  -- _ /\ pathElementIds_ref <- HK.useRef (Map.empty :: Map (DerivPath Dir.Up l r) String)

  -- highlight path
  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing

  let 
    getElementIdByDerivPath :: DerivPath Dir.Up l r -> HK.HookM Aff String
    -- getElementIdByDerivPath path = do
    --   pathElementIds <- liftEffect $ Ref.read pathElementIds_ref
    --   case Map.lookup path pathElementIds of
    --     Nothing -> bug $ "could not find element id for path: " <> pretty path
    --     Just elemId -> pure elemId
    getElementIdByDerivPath = pure <<< fromPathToElementId

    getElementByPath :: DerivPath Dir.Up l r -> HK.HookM Aff DOM.Element
    getElementByPath derivPath = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByDerivPath derivPath
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> bug $ "could not find element by id: " <> elemId
        Just elem -> pure elem

  let
    setNodeElementStyle :: String -> Maybe (DerivPath Dir.Up l r) -> Maybe (DerivPath Dir.Up l r) -> HK.HookM Aff Unit
    setNodeElementStyle className mb_dpath_old mb_dpath_new = do
      case mb_dpath_old of
        Nothing -> do
          -- set the new node element
          update
        Just dpath ->
          -- ignore if the new path is the same as the old path
          if Just dpath == mb_dpath_new then
            pure unit
          else do
            -- unset the old node element
            elem <- getElementByPath dpath
            liftEffect $ setClassName elem className false
            -- set the new node element
            update
      where
      update = do
        case mb_dpath_new of
          Nothing -> pure unit
          Just dpath -> do
            elem <- getElementByPath dpath
            liftEffect $ setClassName elem className true

    setCursorElement = setNodeElementStyle cursorClassName
    setSelectTopElement = setNodeElementStyle selectTopClassName
    setSelectBottomElement = setNodeElementStyle selectBottomClassName
    setHighlightElement mb_dpath_new = do
      mb_dpath_old <- liftEffect $ Ref.read maybeHighlightPath_ref
      setNodeElementStyle highlightClassName mb_dpath_old mb_dpath_new
      liftEffect $ Ref.write mb_dpath_new maybeHighlightPath_ref

    unsetFacadeElements :: HK.HookM Aff Unit
    unsetFacadeElements = getFacade >>= case _ of
      BufferState buffer -> do
        setHighlightElement Nothing
        setCursorElement (Just (unwrap (buffer.dzipper)).path) Nothing
      CursorState cursor -> do
        setHighlightElement Nothing
        setCursorElement (Just (unwrap (cursor.dzipper)).path) Nothing
      SelectState select -> do
        setHighlightElement Nothing
        setSelectTopElement (Just (Expr.zipperpTopPath select.dzipperp)) Nothing
        setSelectBottomElement (Just (Expr.zipperpBottomPath select.dzipperp)) Nothing
      TopState _top -> do
        setHighlightElement Nothing

    -- | Sets the facade state, which updates all the corresponding UI elements.
    setFacade :: State l r -> HK.HookM Aff Unit
    setFacade st = do
      -- Debug.traceM $ "[setFacade] new state: " <> pretty st
      unsetFacadeElements
      setFacade' st

    -- | Sets the facade state, which updates all the corresponding UI elements.
    -- | Doesn't `unsetFacadeElements` first
    setFacade' :: State l r -> HK.HookM Aff Unit
    setFacade' st = do
      case st of
        CursorState cursor -> do
          setCursorElement Nothing (Just (unwrap cursor.dzipper).path)
        BufferState buffer -> do
          setCursorElement Nothing (Just (unwrap buffer.dzipper).path)
        SelectState select -> do
          Debug.traceM $ "[setFacade'] Expr.zipperpTopPath select.dzipperp = " <> show (Expr.zipperpTopPath select.dzipperp)
          setSelectTopElement Nothing (Just (Expr.zipperpTopPath select.dzipperp))

          -- Debug.traceM $ case selection of
          --   Left _ -> "left"
          --   Right _ -> "right"
          Debug.traceM $ show (unwrap select.dzipperp).selection

          let botPath = Expr.zipperpBottomPath select.dzipperp
          Debug.traceM $ "[setFacade'] Expr.zipperpBottomPath select.dzipperp = " <> show botPath
          Debug.traceM $ "[setFacade'] Expr.zipperpBottomPath select.dzipperp = " <> pretty botPath
          Debug.traceM $ "[setFacade'] path dir = " <> show (Expr.reflectPathDir botPath)
          setSelectBottomElement Nothing (Just botPath)
        TopState _top -> do
          pure unit
      liftEffect (Ref.write st facade_ref)

    getFacade :: HK.HookM Aff (State l r)
    getFacade = liftEffect (Ref.read facade_ref)

    setState :: State l r -> HK.HookM Aff Unit
    setState st = do
      unsetFacadeElements
      HK.put state_id st
      -- can use `setFacade'` since we already `unsetFacadeElements`
      setFacade' st

    handleAction = case _ of
      SetDerivZipperAction lazy_dzipper -> do
        -- compute new dzipper
        case force lazy_dzipper of
          Nothing -> pure unit
          Just dzipper -> setState $ CursorState {dzipper}

    moveCursor dir = getFacade >>= case _ of
      BufferState _buffer -> pure unit
      CursorState cursor -> do
        -- Debug.traceM $ "[moveCursor] st = " <> pretty st.dzipper
        case moveZipper dir cursor.dzipper of
          Nothing -> pure unit
          Just dzipper -> setFacade $ CursorState cursor {dzipper = dzipper}
      SelectState select -> do
        let cursor = {dzipper: Expr.unzipperp select.dzipperp}
        case moveZipper dir cursor.dzipper of
          Nothing -> pure unit
          Just dzipper -> setFacade $ CursorState cursor {dzipper = dzipper}
      TopState top -> do
        let cursor = {dzipper: Expr.Zipper {path: mempty, expr: top.expr}}
        case moveZipper dir cursor.dzipper of
          Nothing -> pure unit
          Just dzipper -> setFacade $ CursorState cursor {dzipper = dzipper}

    moveSelect dir = getFacade >>= case _ of
      BufferState _buffer -> unsafeCrashWith "!TODO escape to cursor first"
      CursorState cursor -> do
        Debug.traceM "[moveSelect] attempting CursorState --> SelectState"
        let select = (_ $ dir) $ case_
              # on _up (\_ -> {dzipperp: Expr.Zipperp {path: (unwrap cursor.dzipper).path, selection: Left mempty, expr: (unwrap cursor.dzipper).expr}})
              # on _down (\_ -> {dzipperp: Expr.Zipperp {path: (unwrap cursor.dzipper).path, selection: Right mempty, expr: (unwrap cursor.dzipper).expr}})
              # on _left (\_ -> unsafeCrashWith "!TODO moveSelect left when CursorState")
              # on _right (\_ -> unsafeCrashWith "!TODO moveSelect right when CursorState")
              # on _prev (\_ -> unsafeCrashWith "!TODO moveSelect prev when CursorState")
              # on _next (\_ -> unsafeCrashWith "!TODO moveSelect next when CursorState")
        Debug.traceM $ "[moveSelect] select = " <> show select
        case moveZipperp dir select.dzipperp of
          Nothing -> do
            Debug.traceM "[moveSelect] failed to enter SelectState"
            pure unit
          Just (Left dzipper) -> setFacade $ CursorState {dzipper: dzipper}
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      SelectState select -> do
        Debug.traceM "[moveSelect] moving selection"
        case moveZipperp dir select.dzipperp of
          Nothing -> do
            Debug.traceM "[moveSelect] failed to move selection"
          Just (Left dzipper) -> setFacade $ CursorState {dzipper: dzipper}
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      TopState top -> do
        let select = (_ $ dir) $ case_
              # on _up (\_ -> unsafeCrashWith "!TODO moveSelect up when TopState")
              # on _down (\_ -> {dzipperp: Expr.Zipperp {path: mempty, selection: Left mempty, expr: top.expr}})
              # on _left (\_ -> unsafeCrashWith "!TODO moveSelect left when TopState")
              # on _right (\_ -> unsafeCrashWith "!TODO moveSelect right when TopState")
              # on _prev (\_ -> unsafeCrashWith "!TODO moveSelect prev when TopState")
              # on _next (\_ -> unsafeCrashWith "!TODO moveSelect next when TopState")
        case moveZipperp dir select.dzipperp of
          Nothing -> pure unit
          Just (Left dzipper) -> setFacade $ CursorState {dzipper: dzipper}
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}

    handleKeyboardEvent :: KeyboardEvent.KeyboardEvent -> HK.HookM Aff Unit
    handleKeyboardEvent event = do
      -- Console.log $ "[event.key] " <> KeyboardEvent.key event
      let 
        key = KeyboardEvent.key event
        shiftKey = KeyboardEvent.shiftKey event
        ctrlKey = KeyboardEvent.ctrlKey event
        metaKey = KeyboardEvent.metaKey event
        altKey = KeyboardEvent.altKey event
        cmdKey = ctrlKey || metaKey

      getFacade >>= case _ of
        ------------------------------------------------------------------------
        -- BufferState
        ------------------------------------------------------------------------
        BufferState buffer -> do
          if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByDerivPath (unwrap buffer.dzipper).path
            HK.tell tokens.slotToken bufferSlot elemId SubmitBufferQuery
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            elemId <- getElementIdByDerivPath (unwrap buffer.dzipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery false
          else if key == "ArrowUp" then do
            -- Debug.traceM $ "[moveBufferQuery Up]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByDerivPath (unwrap buffer.dzipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery upDir
          else if key == "ArrowDown" then do
            -- Debug.traceM $ "[moveBufferQuery Down]"
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByDerivPath (unwrap buffer.dzipper).path
            HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery downDir
          else pure unit
        ------------------------------------------------------------------------
        -- CursorState
        ------------------------------------------------------------------------
        CursorState cursor -> do
          let Expr.Zipper zp = cursor.dzipper
          -- copy
          if cmdKey && key == "c" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Right zp.expr)) clipboard_ref
          -- cut
          else if cmdKey && key == "x" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Right zp.expr)) clipboard_ref
            -- replace cursor with hole
            unsafeCrashWith "!TODO requires holes in generic grammar"
          else if cmdKey && key == "v" then do
            liftEffect (Ref.read clipboard_ref) >>= case _ of
              Nothing -> pure unit -- nothing in clipboard
              Just (Left path) -> do
                -- paste a path
                setState $ CursorState cursor {dzipper = Expr.Zipper zp {path = path <> zp.path}}
              Just (Right expr) -> do
                -- paste an expr
                setState $ CursorState cursor {dzipper = Expr.Zipper zp {expr = expr}}
          else if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            elemId <- getElementIdByDerivPath zp.path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true
          else if key == "ArrowUp" then (if shiftKey then moveSelect else moveCursor) upDir
          else if key == "ArrowDown" then (if shiftKey then moveSelect else moveCursor) downDir
          else if key == "ArrowLeft" then (if shiftKey then moveSelect else moveCursor) leftDir
          else if key == "ArrowRight" then (if shiftKey then moveSelect else moveCursor) rightDir
          else pure unit
        ------------------------------------------------------------------------
        -- SelectState
        ------------------------------------------------------------------------
        SelectState select -> do
          let Expr.Zipperp {path, selection, expr} = select.dzipperp
          -- copy
          if cmdKey && key == "c" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Left (either Expr.reversePath identity selection))) clipboard_ref
          else if cmdKey && key == "x" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Left (either Expr.reversePath identity selection))) clipboard_ref
            -- escape to cursor mode, but without selection (updates state)
            setState $ CursorState {dzipper: Expr.Zipper {path, expr}}
          else if key == "Escape" then do
            -- SelectState --> CursorState
            setFacade $ CursorState {dzipper: Expr.unzipperp select.dzipperp}
          else if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = {dzipper: Expr.unzipperp select.dzipperp}
            setFacade $ CursorState cursor
            -- activate buffer
            elemId <- getElementIdByDerivPath path
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true
          else if key == "ArrowUp" then (if shiftKey then moveSelect else moveCursor) upDir
          else if key == "ArrowDown" then (if shiftKey then moveSelect else moveCursor) downDir
          else if key == "ArrowLeft" then (if shiftKey then moveSelect else moveCursor) leftDir
          else if key == "ArrowRight" then (if shiftKey then moveSelect else moveCursor) rightDir
          else pure unit
        ------------------------------------------------------------------------
        -- TopState
        ------------------------------------------------------------------------
        TopState _ -> do
          if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- !TODO top --> cursor
            -- !TODO activate buffer
            pure unit
          else if key == "ArrowUp" then (if shiftKey then moveSelect else moveCursor) upDir
          else if key == "ArrowDown" then (if shiftKey then moveSelect else moveCursor) downDir
          else if key == "ArrowLeft" then (if shiftKey then moveSelect else moveCursor) leftDir
          else if key == "ArrowRight" then (if shiftKey then moveSelect else moveCursor) rightDir
          else pure unit

    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
      UpdateFacadeOutput f -> setFacade =<< (f =<< getFacade)
  
    renderExpr isCursor dzipper = do
      let
        elemId = fromPathToElementId (unwrap dzipper).path
        clsNames /\ kidElems = input.renderDerivExprKids (unwrap dzipper).expr $ renderExpr false <<< snd <$> Expr.zipDowns dzipper

      HH.div
        [ classNames $ ["node"] <> clsNames <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setFacade $ CursorState {dzipper: dzipper}
        , HE.onMouseOver \event -> do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setHighlightElement (Just (unwrap dzipper).path)
            pure unit
        ] $
        Array.concat
        [ [ HH.slot bufferSlot elemId bufferComponent 
            { dzipper: dzipper
            , edits: input.getEdits dzipper
            } 
            handleBufferOutput
          ]
        , kidElems
        ]

    renderPath dzipper1 interior  = do
      case Expr.zipUp dzipper1 of
        Nothing -> interior
        Just (th /\ dzipper2) -> do
          let
            elemId = fromPathToElementId (unwrap dzipper2).path
            clsNames /\ kidElems = 
              input.renderDerivExprKids (Expr.unTooth th (unwrap dzipper1).expr) $
              Array.fromFoldable $
              ZipList.unpathAround interior $ do
                let kidZippers = Expr.zipDownsTooth dzipper2 th
                let ZipList.Path p = kidZippers
                renderExpr false <$> kidZippers
          renderPath dzipper2 $
            HH.div
              [ classNames $ ["node"] <> clsNames
              , HP.id elemId
              , HE.onMouseDown \event -> do
                  H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                  setFacade $ CursorState {dzipper: dzipper2}
              , HE.onMouseOver \event -> do
                  H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                  setHighlightElement (Just (unwrap dzipper2).path)
                  pure unit
              ] $
              Array.concat
              [ [ HH.slot bufferSlot elemId bufferComponent 
                  { dzipper: dzipper2
                  , edits: input.getEdits dzipper2
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
    --       [ "dzipper = " <> pretty dzipper
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
          CursorState st -> [renderPath st.dzipper $ renderExpr true st.dzipper]
          BufferState _st -> unsafeCrashWith "!TODO render BufferState"
          SelectState _st -> unsafeCrashWith "!TODO render SelectState"
          TopState _st -> unsafeCrashWith "!TODO render TopState"
      ]

bufferSlot = Proxy :: Proxy "buffer"

type BufferInput l r =
  { dzipper :: DerivZipper l r
  , edits :: Array (Edit l r)
  }

bufferComponent :: forall l r. IsRuleLabel l r => H.Component (Query l r) (BufferInput l r) (Output l r) Aff
bufferComponent = HK.component \tokens input -> HK.do
  isEnabled /\ isEnabled_id <- HK.useState false
  bufferString /\ bufferString_id <- HK.useState ""
  bufferFocus /\ bufferFocus_id <- HK.useState 0
  -- !TODO bufferFocus is actually 2D, since eventually I'll implement cycling
  -- between different edits that have the same label

  let bufferInputRefLabelString = "buffer-input"

  edits <- HK.captures {dzipper: input.dzipper, bufferString} $ flip HK.useMemo \_ ->
    input.edits #
      -- memo fuzzy distances
      map (\edit -> Fuzzy.matchStr false bufferString edit.label /\ edit) >>>
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
          HK.getHTMLElementRef (H.RefLabel bufferInputRefLabelString) >>= case _ of 
            Nothing -> bug $ "[bufferComponent.useQuery] could not find element with ref ExprLabel: " <> bufferInputRefLabelString
            Just elem -> liftEffect $ HTMLElement.focus elem
          -- update facade to BufferState
          HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
            pure $ BufferState {dzipper: input.dzipper}
          pure unit
      else
        -- update facade to CursorState
        HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
          pure $ CursorState {dzipper: input.dzipper}
      pure (Just a)
    MoveBufferQuery qm a -> do
      if isEnabled then do
        (qm # _) $ case_
          # on _up (\_ -> HK.modify_ bufferFocus_id (_ - 1))
          # on _down (\_ -> HK.modify_ bufferFocus_id (_ + 1))
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
    --       [ "dzipper = " <> pretty input.dzipper
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

_verbose_path_element_ids :: Boolean
_verbose_path_element_ids = true

fromPathToElementId :: forall l. Expr.IsExprLabel l => Expr.Path Up l -> String
fromPathToElementId 
  | _verbose_path_element_ids = Expr.foldMapPath "PathEnd" \(Expr.Tooth l kidsZip) str -> pretty l <> "@" <> show (ZipList.leftLength kidsZip) <> "-" <> str
  | otherwise = Expr.foldMapPath "PathEnd" \(Expr.Tooth _ kidsZip) str -> show (ZipList.leftLength kidsZip) <> "-" <> str

{-
unsafePerformEffect do
    -- generate new elemId and add it to the map
    elemId_ <- UUID.toString <$> UUID.genUUID
    Ref.modify_ (Map.insert (unwrap dzipper).path elemId_) pathElementIds_ref
    pure elemId_
-}