module Language.Pantograph.Generic.Rendering.Editor where

import Language.Pantograph.Generic.Rendering.Base (CursorMode(..), EditorLocals, EditorSpec, HoleyDerivPath(..), HoleyDerivZipper(..), Output(..), Query(..), State(..), bufferSlot, cursorClassName, cursorFromHoleyDerivZipper, fromHoleyDerivPathToElementId, fromPathToElementId, hdzipperDerivPath, hdzipperDerivTerm, hdzipperHoleyDerivPath, hdzipperZipper, highlightClassName, isBufferKey, selectBottomClassName, selectTopClassName)
import Language.Pantograph.Generic.Rendering.Elements (colonElem, interrogativeElem, placeholderCursorNodeElem)
import Prelude
import Bug (bug)
import Bug.Assertion (assert, just)
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Expr (wellformedExpr, (%))
import Data.Expr as Expr
import Data.Int.Bits as Bits
import Data.Lazy (defer, force)
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, default, inj, on)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML (div, slot, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, setClassName)
import Hole (hole)
import Language.Pantograph.Generic.Edit (Action(..), EditPreview(..))
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, DerivLabel(..), DerivPath, DerivTerm, defaultDerivTerm, derivTermSort, derivToothInteriorSort, isHoleDerivTerm, isHoleRule)
import Language.Pantograph.Generic.Rendering.Buffer (bufferComponent)
import Language.Pantograph.Generic.ZipperMovement (moveZipper, moveZipperp)
import Log (logM)
import Text.Pretty (pretty)
import Text.Pretty as P
import Type.Direction (Up, _down, _left, _next, _prev, _right, _up, readMoveDir, readVerticalDir)
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.MouseEvent as MouseEvent


editorComponent :: forall q l r.
  IsRuleLabel l r =>
  H.Component q (EditorSpec l r) Unit Aff
editorComponent = HK.component \tokens input -> HK.do
  let
    initState = CursorState
      (cursorFromHoleyDerivZipper input.hdzipper)

  -- state
  currentState /\ state_id <- HK.useState $ initState

  -- facade state
  _ /\ facade_ref <- HK.useRef $ initState

  -- clipboard
  _ /\ clipboard_ref <- HK.useRef (Nothing :: Maybe (DerivPath Up l r \/ DerivTerm l r))

  -- highlight path
  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing

  let 
    locals :: EditorLocals l r
    locals = 
        { input
        , initState
        , currentState
        , state_id
        , facade_ref
        , clipboard_ref
        , maybeHighlightPath_ref
        }

  let
    getElementIdByHoleyDerivPath :: HoleyDerivPath Up l r -> HK.HookM Aff String
    getElementIdByHoleyDerivPath = pure <<< fromHoleyDerivPathToElementId

    getElementByHoleyDerivPath :: HoleyDerivPath Up l r -> HK.HookM Aff DOM.Element
    getElementByHoleyDerivPath hdzipper = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByHoleyDerivPath hdzipper
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> do
          st <- getState
          bug $ "could not find element" <> P.indent ("\n- elemId = " <> elemId <> "\n- hdzipper = " <> pretty hdzipper <> "\n- st = " <> pretty st)
        Just elem -> pure elem

    -- setNodeElementStyle :: String -> Maybe (DerivPath Up l r) -> Maybe (DerivPath Up l r) -> HK.HookM Aff Unit
    setNodeElementStyle :: String -> Maybe (HoleyDerivPath Up l r) -> Maybe (HoleyDerivPath Up l r) -> HK.HookM Aff Unit
    setNodeElementStyle className mb_hdpath_old mb_hdpath_new = do
      case mb_hdpath_old of
        Nothing -> do
          -- set the new node element
          update
        Just hdpath ->
          -- ignore if the new path is the same as the old path
          if Just hdpath == mb_hdpath_new then
            pure unit
          else do
            -- unset the old node element
            elem <- getElementByHoleyDerivPath hdpath
            liftEffect $ setClassName elem className false
            -- set the new node element
            update
      where
      update = do
        case mb_hdpath_new of
          Nothing -> pure unit
          Just hdpath -> do
            elem <- getElementByHoleyDerivPath hdpath
            liftEffect $ setClassName elem className true

    setCursorElement = setNodeElementStyle cursorClassName
    setSelectTopElement = setNodeElementStyle selectTopClassName
    setSelectBottomElement = setNodeElementStyle selectBottomClassName
    setHighlightElement mb_hdpath_new = do
      mb_hdpath_old <- liftEffect $ Ref.read maybeHighlightPath_ref
      setNodeElementStyle highlightClassName mb_hdpath_old mb_hdpath_new
      liftEffect $ Ref.write mb_hdpath_new maybeHighlightPath_ref

    unsetFacadeElements :: HK.HookM Aff Unit
    unsetFacadeElements = do
      currentFacade <- getFacade
      -- Debug.traceM $ "[unsetFacadeElements] currentFacade = " <> pretty currentFacade
      case currentFacade of
        CursorState cursor -> do
          setHighlightElement Nothing
          setCursorElement (Just (hdzipperHoleyDerivPath cursor.hdzipper)) Nothing
        SelectState select -> do
          setHighlightElement Nothing
          setSelectTopElement (Just (InjectHoleyDerivPath (Expr.zipperpTopPath select.dzipperp))) Nothing
          setSelectBottomElement (Just (InjectHoleyDerivPath (Expr.zipperpBottomPath select.dzipperp))) Nothing
        TopState _top -> do
          setHighlightElement Nothing

    -- | Sets the facade state, which updates all the corresponding UI elements.
    setFacade :: State l r -> HK.HookM Aff Unit
    setFacade st = do
      -- Debug.traceM $ "[setFacade] st = " <> pretty st
      unsetFacadeElements
      setFacade' st

    -- | Sets the facade state, which updates all the corresponding UI elements.
    -- | Doesn't `unsetFacadeElements` first
    setFacade' :: State l r -> HK.HookM Aff Unit
    setFacade' st = do
      -- Debug.traceM $ "[setFacade'] st = " <> pretty st
      case st of
        CursorState cursor -> do
          setCursorElement Nothing (Just (hdzipperHoleyDerivPath cursor.hdzipper))
        SelectState select -> do
          setSelectTopElement Nothing (Just (InjectHoleyDerivPath (Expr.zipperpTopPath select.dzipperp)))
          setSelectBottomElement Nothing (Just (InjectHoleyDerivPath (Expr.zipperpBottomPath select.dzipperp)))
        TopState _top -> do
          pure unit
      liftEffect (Ref.write st facade_ref)

    getFacade :: HK.HookM Aff (State l r)
    getFacade = liftEffect (Ref.read facade_ref)

    setState :: State l r -> HK.HookM Aff Unit
    setState st = do
      -- Debug.traceM $ "[setState] st = " <> pretty st
      unsetFacadeElements
      -- Debug.traceM $ "[setState] HK.put state_id st"
      HK.put state_id st
      -- can use `setFacade'` since we already `unsetFacadeElements`
      setFacade' st

    getState :: HK.HookM Aff (State l r)
    getState = HK.get state_id

    handleAction = case _ of
      SetCursorAction lazy_dzipper -> do
        -- compute new dzipper
        let dzipper = force lazy_dzipper
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper))
      SetSSTermAction lazy_ssterm -> 
        hole "handleAction SetSSTermAction"

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState cursor@{mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          -- Debug.traceM $ "[moveCursor] " <> pretty (CursorState cursor)
          case cursor.hdzipper of
            -- if at hole, moving down goes to hole interior
            InjectHoleyDerivZipper dzipper 
              | dterm <- Expr.zipperExpr dzipper
              , isHoleDerivTerm dterm
              , dir == inj _down Proxy -> do
                  -- Debug.traceM $ "[moveCursor] at hole; going down to hole interior"
                  setFacade $ CursorState (cursorFromHoleyDerivZipper (HoleInteriorHoleyDerivZipper (Expr.zipperPath dzipper) (derivTermSort dterm)))
            InjectHoleyDerivZipper dzipper -> do
              -- Debug.traceM $ "[moveCursor] at hole; NOT going down to hole interior"
              case moveZipper dir dzipper of
                Nothing -> pure unit
                Just dzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper'))
            HoleInteriorHoleyDerivZipper dpath sort -> default (pure unit)
              -- if at hole interior, moving up goes to hole
              # on _down (\_ -> pure unit)
              # on _up (\_ -> assert (just "moveCursor.HoleInteriorHoleyDerivZipper" (defaultDerivTerm sort)) \dterm -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper dpath dterm))))
              # on _left (\_ -> assert (just "moveCursor.HoleInteriorHoleyDerivZipper" (defaultDerivTerm sort)) \dterm -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper dpath dterm))))
              # on _right (\_ -> assert (just "moveCursor.HoleInteriorHoleyDerivZipper" (defaultDerivTerm sort)) \dterm -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper dpath dterm))))
              # on _prev (\_ -> assert (just "moveCursor.HoleInteriorHoleyDerivZipper" (defaultDerivTerm sort)) \dterm -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper dpath dterm))))
              # on _next (\_ -> assert (just "moveCursor.HoleInteriorHoleyDerivZipper" (defaultDerivTerm sort)) \dterm -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper dpath dterm))))
              $ dir
        SelectState select -> do
          let dzipper = Expr.unzipperp select.dzipperp
          case moveZipper dir dzipper of
            Nothing -> pure unit
            Just dzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper'))
        TopState top -> do
          let dzipper = Expr.Zipper mempty top.dterm
          case moveZipper dir dzipper of
            Nothing -> pure unit
            Just dzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper'))

    moveSelect dir = getFacade >>= case _ of
      CursorState cursor -> do
        let path = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        let select = (_ $ dir) $ case_
              # on _up (\_ -> {dzipperp: Expr.Zipperp path (Left mempty) dterm})
              # on _down (\_ -> {dzipperp: Expr.Zipperp path (Right mempty) dterm})
              # on _left (\_ -> hole "moveSelect left when CursorState")
              # on _right (\_ -> hole "moveSelect right when CursorState")
              # on _prev (\_ -> hole "moveSelect prev when CursorState")
              # on _next (\_ -> hole "moveSelect next when CursorState")
        case moveZipperp dir select.dzipperp of
          Nothing -> do
            logM "moveSelect" "failed to enter SelectState"
            pure unit
          Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper))
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      SelectState select -> do
        case moveZipperp dir select.dzipperp of
          Nothing -> do
            logM "moveSelect" "failed to move selection"
          Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper))
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      TopState top -> do
        let mb_select = (_ $ dir) $ default Nothing -- (pure unit)
              # on _down (\_ -> Just {dzipperp: Expr.Zipperp mempty (Left mempty) top.dterm})
              # on _next (\_ -> Just (hole "moveSelect next when TopState"))
        case mb_select of
          Nothing -> pure unit
          Just select -> case moveZipperp dir select.dzipperp of
            Nothing -> pure unit
            Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper))
            Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}

    handleKeyboardEvent :: KeyboardEvent.KeyboardEvent -> HK.HookM Aff Unit
    handleKeyboardEvent event = do
      -- Console.log $ "[event.key] " <> KeyboardEvent.key event
      let 
        key = KeyboardEvent.key event
        keyCodePoint = case String.toCodePointArray key of
          [c] -> Just c
          _ -> Nothing
        shiftKey = KeyboardEvent.shiftKey event
        ctrlKey = KeyboardEvent.ctrlKey event
        metaKey = KeyboardEvent.metaKey event
        altKey = KeyboardEvent.altKey event
        cmdKey = ctrlKey || metaKey

      getFacade >>= case _ of
        ------------------------------------------------------------------------
        -- CursorState where mode = BufferCursorMode
        ------------------------------------------------------------------------
        CursorState cursor@{mode: BufferCursorMode} -> do
          if isBufferKey key then do
            -- exit BufferCursorMode
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            HK.tell tokens.slotToken bufferSlot elemId SubmitBufferQuery
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery false Nothing
          else if isJust (readVerticalDir key) then
            assert (just "handleKeyboardEvent" $ readVerticalDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
              HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery dir
          else pure unit
        -- !TODO no, actually this is all handled in buffer mode
        -- ------------------------------------------------------------------------
        -- -- CursorState where mode = StringCursorMode
        -- ------------------------------------------------------------------------
        -- CursorState cursor@{mode: StringCursorMode str} -> do
        --   if isBufferKey key then do
        --     -- exit StringCursorMode
        --     Debug.traceM "exiting StringCursorMode"
        --     liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
        --     setState $ CursorState cursor 
        --       { mode = NavigationCursorMode
        --       , hdzipper = InjectHoleyDerivZipper (Expr.Zipper (hdzipperDerivPath cursor.hdzipper) (DerivString str % []))
        --       }
        --   -- !TODO dont do this here; actually do this in 
        --   -- else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
        --   --   setFacade $ CursorState cursor {mode = StringCursorMode (str <> key)}
        --   --   pure unit
        --   else
        --     pure unit
        ------------------------------------------------------------------------
        -- CursorState where mode = NavigationCursorMode
        ------------------------------------------------------------------------
        CursorState cursor -> do
          let path = hdzipperDerivPath cursor.hdzipper
          let dterm = hdzipperDerivTerm cursor.hdzipper
          -- copy
          if cmdKey && key == "c" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Right dterm)) clipboard_ref
          -- cut
          else if cmdKey && key == "x" then do
            case defaultDerivTerm (derivTermSort dterm) of
              Nothing -> pure unit
              Just dterm' -> do
                -- update clipboard
                liftEffect $ Ref.write (Just (Right dterm)) clipboard_ref
                -- replace cursor with default deriv
                setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm')))
          else if cmdKey && key == "v" then do
            liftEffect (Ref.read clipboard_ref) >>= case _ of
              Nothing -> pure unit -- nothing in clipboard
              Just (Left path') -> do
                -- paste a path
                setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper (path' <> path) dterm)))
              Just (Right dterm') -> do
                -- paste an dterm
                setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm')))
          else if isBufferKey key then do
            -- enter BufferCursorMode or StringCursorMode depending on the dterm
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            case hdzipperDerivTerm cursor.hdzipper of
              DerivString str % _ -> do
                elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
                HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true (Just str)
              _ -> do
                -- activate buffer
                elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
                HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true Nothing
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true (Just key)
          else if key == "Backspace" then do
            -- replace dterm with default deriv
            case defaultDerivTerm (derivTermSort dterm) of
              Nothing -> pure unit
              Just dterm' ->  setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm')))
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
          else pure unit
        ------------------------------------------------------------------------
        -- SelectState
        ------------------------------------------------------------------------
        SelectState select -> do
          let Expr.Zipperp path selection dterm = select.dzipperp
          -- copy
          if cmdKey && key == "c" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Left (either Expr.reversePath identity selection))) clipboard_ref
          -- cut
          else if cmdKey && key == "x" then do
            -- update clipboard
            liftEffect $ Ref.write (Just (Left (either Expr.reversePath identity selection))) clipboard_ref
            -- escape to cursor state, but without selection (updates state)
            setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm)))
          else if key == "Escape" then do
            -- SelectState --> CursorState
            setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp)))
          else if key == "Backspace" then do
            -- escape to cursor state, but without selection (updates state)
            setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm)))
          else if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
            setFacade $ CursorState cursor
            -- activate buffer
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true Nothing
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
            setFacade $ CursorState cursor
            -- activate buffer
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery true (Just key)
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
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
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
          else pure unit

    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
      UpdateFacadeOutput f -> setFacade =<< (f =<< getFacade)
  
    renderDerivTermKids (Expr.Zipper dpath dterm) kidElems = assert (wellformedExpr "renderDerivTermKids" dterm) \_ -> case dterm of
      DerivLabel r sort % [] | isHoleRule r -> ["hole"] /\
        [ HH.div [classNames ["subnode", "inner"]]
            [ HH.div [classNames ["subnode", "hole-interior"]] [renderHoleInterior false dpath sort]
            , colonElem
            , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
            ]
        ]
      DerivLabel r sort % kids -> input.renderDerivTermKids' (r /\ sort /\ kids) kidElems
      DerivString str % [] -> ["string"] /\ 
        [ if String.null str 
            then HH.div [classNames ["subnode", "inner", "empty-string"]] [HH.text "String"]
            else HH.div [classNames ["subnode", "inner"]] [HH.text str]
        ]

    {-
    mouse cursoring and selecting:
    
    - onMouseDown a node: 
        - set cursor to this node
    - onMouseOver a node:
        - if mouse is down
          - if this node is not equal to cursor node, and selection is possible
            from cursor node to this node:
              - update all nodes to have a cache of their
              - set selection to be from cursor node to this node
        - else if mouse is up:
          - set highlight to this node
    -}

    onMouseDown hdzipper event = do
      H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
      setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper)
    
    -- !TODO when making a selection, should i check that sorts match? Or should
    -- I delay that check to when you try to do an action e.g. delete/cut
    onMouseOver hdzipper event = do
      H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
      let dzipper = (hdzipperZipper hdzipper)
      if MouseEvent.buttons event Bits..&. 1 /= 0 then do
        getFacade >>= case _ of
          CursorState cursor -> do
            case Expr.zipperpFromTo (hdzipperZipper cursor.hdzipper) dzipper of
              Nothing -> pure unit
              Just dzipperp -> setFacade (SelectState {dzipperp})
          SelectState select -> do
            case Expr.zipperpFromTo (Expr.unzipperp select.dzipperp) dzipper of
              Nothing -> pure unit
              Just dzipperp -> setFacade (SelectState {dzipperp})
          TopState _top -> pure unit
        setHighlightElement Nothing
      else do
        setHighlightElement (Just (hdzipperHoleyDerivPath hdzipper))

    renderExpr isCursor dzipper = do
      let
        elemId = fromPathToElementId (Expr.zipperPath dzipper)
        clsNames /\ kidElems = renderDerivTermKids dzipper $ renderExpr false <<< snd <$> Expr.zipDowns dzipper
      HH.div
        [ classNames $ ["node"] <> clsNames <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper))
        ] $
        Array.concat
        [ [ HH.slot bufferSlot elemId bufferComponent 
            { hdzipper: InjectHoleyDerivZipper dzipper
            , edits: input.editsAtHoleyDerivZipper input.topSort (InjectHoleyDerivZipper dzipper) <#>
                \edit -> renderEditPreview edit.preview /\ edit
            } 
            handleBufferOutput
          ]
        , kidElems
        ]

    renderPreviewDerivZipper dzipper = do
      let
        clsNames /\ kidElems = renderDerivTermKids dzipper $ renderPreviewDerivZipper <<< snd <$> Expr.zipDowns dzipper
      HH.div
        [classNames $ ["node"] <> clsNames]
        kidElems

    renderHoleInterior isCursor dpath sort = do
      (\kidElem -> if isCursor then assert (just "renderHoleInterior" (defaultDerivTerm sort)) \dterm -> do
        let
          dzipper = Expr.Zipper dpath dterm
          elemId = fromPathToElementId dpath
          clsNames = ["hole"]
        HH.div
          [ classNames $ ["node"] <> clsNames
          , HP.id elemId
          , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
          , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper))
          ] $
          Array.concat
          [ [ HH.slot bufferSlot elemId bufferComponent 
              { hdzipper: InjectHoleyDerivZipper dzipper
              , edits: input.editsAtHoleyDerivZipper input.topSort (InjectHoleyDerivZipper dzipper) <#>
                \edit -> renderEditPreview edit.preview /\ edit
              } 
              handleBufferOutput
            ]
          , [ HH.div [classNames ["subnode", "inner"]]
                [ HH.div [classNames ["subnode", "hole-interior"]] [kidElem]
                , colonElem
                , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
                ]
            ]
          ]
      else
        kidElem
      ) do
        let elemId = fromHoleyDerivPathToElementId (HoleInteriorHoleyDerivPath dpath)
        HH.div
          [ classNames $ ["node", "holeInterior"] <> if isCursor then [cursorClassName] else []
          , HP.id elemId
          , HE.onMouseDown (onMouseDown (HoleInteriorHoleyDerivZipper dpath sort))
          , HE.onMouseOver (onMouseOver (HoleInteriorHoleyDerivZipper dpath sort))
          ] $
          Array.concat
          [ [ let hdzipper = HoleInteriorHoleyDerivZipper dpath sort in
              HH.slot bufferSlot elemId bufferComponent
                { hdzipper
                , edits: input.editsAtHoleyDerivZipper input.topSort hdzipper <#>
                    \edit -> renderEditPreview edit.preview /\ edit
                }
                handleBufferOutput
            ]
          , [ HH.div [classNames ["subnode", "inner"]]
              [interrogativeElem]
            ]
          ]

    renderPath dzipper interior  = do
      case Expr.zipUp dzipper of
        Nothing -> interior
        Just (th /\ dzipper2) -> do
          let
            elemId = fromPathToElementId (Expr.zipperPath dzipper2)
            -- _ = Debug.trace ("[renderPath] Expr.zipDownsTooth dzipper2 th = " <> show (pretty <$> Expr.zipDownsTooth dzipper2 th)) \_ -> unit
            clsNames /\ kidElems = 
              -- renderDerivTermKids (Expr.unTooth th (Expr.zipperExpr dzipper)) $
              renderDerivTermKids (Expr.Zipper (Expr.zipperPath dzipper2) (Expr.unTooth th (Expr.zipperExpr dzipper))) $
              Array.fromFoldable $
              ZipList.unpathAround interior $ do
                let kidZippers = Expr.zipDownsTooth dzipper2 th
                renderExpr false <$> kidZippers
          renderPath dzipper2 $
            HH.div
              [ classNames $ ["node"] <> clsNames
              , HP.id elemId
              , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper2))
              , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper2))
              ] $
              Array.concat
              [ [ HH.slot bufferSlot elemId bufferComponent 
                  { hdzipper: InjectHoleyDerivZipper dzipper2
                  , edits: input.editsAtHoleyDerivZipper input.topSort (InjectHoleyDerivZipper dzipper2) <#>
                    \edit -> renderEditPreview edit.preview /\ edit
                  } 
                  handleBufferOutput
                ]
              , kidElems
              ]

    renderPreviewDerivTooth dtooth = assert (just "renderPreviewDerivTooth" (defaultDerivTerm (derivToothInteriorSort dtooth))) \dterm -> do
      let
        dzipper = Expr.Zipper mempty (Expr.unTooth dtooth dterm)

        clsNames /\ kidElems =
          renderDerivTermKids dzipper $
          Array.fromFoldable $
          ZipList.unpathAround placeholderCursorNodeElem $ do
            let kidDZippers = Expr.zipDownsTooth dzipper dtooth
            renderPreviewDerivZipper <$> kidDZippers
      HH.div
        [classNames $ ["node"] <> clsNames]
        kidElems

    renderEditPreview preview = defer \_ -> case preview of
      DerivTermEditPreview dterm -> renderPreviewDerivZipper (Expr.Zipper mempty dterm)
      DerivToothEditPreview dtooth -> renderPreviewDerivTooth dtooth

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
    Debug.trace 
      ("[editorComponent.render]" <> P.bullets
        [ "currentState = " <> pretty currentState
        ]) \_ ->
          HH.div [classNames ["editor"]]
          [ HH.div 
            [ classNames ["program"]
            , HE.onMouseLeave \event -> do
                H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
                setHighlightElement Nothing
            ]
            case currentState of
              CursorState cursor -> do
                let dzipper = hdzipperZipper cursor.hdzipper
                case cursor.hdzipper of
                  InjectHoleyDerivZipper _ -> [renderPath dzipper $ renderExpr true dzipper]
                  HoleInteriorHoleyDerivZipper dpath sort -> [renderPath dzipper $ renderHoleInterior true dpath sort]
              SelectState _st -> hole "render SelectState"
              TopState _st -> hole "render TopState"
          ]