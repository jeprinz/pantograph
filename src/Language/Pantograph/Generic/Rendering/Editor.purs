module Language.Pantograph.Generic.Rendering.Editor where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Rendering.Base
import Language.Pantograph.Generic.Rendering.Buffer
import Language.Pantograph.Generic.Rendering.Elements
import Prelude

import Bug (bug)
import Bug.Assertion (assert, just)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Expr (wellformedExpr, (%))
import Data.Expr as Expr
import Data.Int.Bits as Bits
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (case_, default, inj, on)
import Data.Zippable as Zippable
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML (div, slot, slot_, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, setClassName)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Preview (previewComponent)
import Language.Pantograph.Generic.Smallstep (setupSSTermFromReplaceAction, setupSSTermFromWrapAction)
import Language.Pantograph.Generic.ZipperMovement (moveZipperp)
import Log (logM)
import Text.Pretty (pretty)
import Text.Pretty as P
import Type.Direction (Up, _down, _left, _next, _prev, _right, _up, leftDir, readMoveDir, readVerticalDir, rightDir)
import Type.Proxy (Proxy(..))
import Util (fromJust')
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
editorComponent = HK.component \tokens spec -> HK.do

  ------------------------------------------------------------------------------
  -- initialize state and refs
  ------------------------------------------------------------------------------

  let
    initState = CursorState
      (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper mempty spec.dterm)))

  -- state
  currentState /\ state_id <- HK.useState $ initState

  -- facade state
  _ /\ facade_ref <- HK.useRef $ initState

  -- clipboard
  _ /\ clipboard_ref <- HK.useRef (Nothing :: Maybe (DerivPath Up l r \/ DerivTerm l r))

  -- highlight path
  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing

  let

    ------------------------------------------------------------------------------
    -- manipulate dom
    ------------------------------------------------------------------------------

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

    ------------------------------------------------------------------------------
    -- manipulate state
    ------------------------------------------------------------------------------

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
        SmallStepState _ss -> do
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
        SmallStepState _ss -> do
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

    getCursorState :: String -> HK.HookM Aff (Cursor l r)
    getCursorState source = do
      st <- getState
      assert (cursorState source st) pure

    handleAction = case _ of
      -- !TODO use topChange, botChange
      WrapAction {topChange, dpath, botChange} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper

        let ssterm = setupSSTermFromWrapAction 
              up
              topChange 
              dpath 
              botChange 
              dterm

        -- !TODO
        -- use `step` to get the new term
        -- repeatedly apply `step`
        -- yields `Nothing` when done
        -- to finally turn it back into a DerivTerm, use `termToZipper`

        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper (dpath <> up) dterm)))
      -- !TODO use sub
      FillAction {sub, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper

        let dzipper0 = Expr.Zipper up dterm
        let dzipper1 = subDerivLabel sub <$> dzipper0

        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper1))
      -- !TODO use topChange
      ReplaceAction {topChange, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper

        -- !TODO us this, same way as in WrapAction
        let ssterm = setupSSTermFromReplaceAction 
              up
              topChange
              dterm

        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper up dterm)))

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          case moveHoleyDerivZipper dir cursor.hdzipper of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        SelectState select -> do
          let dzipper = Expr.unzipperp select.dzipperp
          case moveHoleyDerivZipper dir (InjectHoleyDerivZipper dzipper) of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        TopState top -> do
          let dzipper = Expr.Zipper mempty top.dterm
          case moveHoleyDerivZipper dir (InjectHoleyDerivZipper dzipper) of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        SmallStepState _ -> pure unit

    moveSelect dir = getFacade >>= case _ of
      CursorState cursor -> do
        let path = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        let select = {dzipperp: Expr.Zipperp path (Left mempty) dterm}
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
      SmallStepState _ -> pure unit

    setBufferEnabled :: Boolean -> Maybe String -> HK.HookM Aff Unit
    setBufferEnabled isEnabled mb_str = do
      cursor <- do
        st <- getFacade
        assert (cursorState "setBufferEnabled" st) pure
      setState $ CursorState cursor {mode = if isEnabled then BufferCursorMode else NavigationCursorMode}
      HK.tell tokens.slotToken bufferSlot unit $ SetBufferEnabledQuery isEnabled mb_str

    ------------------------------------------------------------------------------
    -- handle keyboard event
    ------------------------------------------------------------------------------

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
        -- altKey = KeyboardEvent.altKey event
        cmdKey = ctrlKey || metaKey

      getFacade >>= case _ of
        ------------------------------------------------------------------------
        -- CursorState where mode = BufferCursorMode
        ------------------------------------------------------------------------
        CursorState {mode: BufferCursorMode} -> do
          if isBufferKey key then do
            -- exit BufferCursorMode
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            HK.tell tokens.slotToken bufferSlot unit SubmitBufferQuery
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate
            setBufferEnabled false Nothing
          else if isJust (readVerticalDir key) then
            assert (just "handleKeyboardEvent" $ readVerticalDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              HK.tell tokens.slotToken bufferSlot unit $ MoveBufferQuery dir
          else pure unit
        ------------------------------------------------------------------------
        -- CursorState where mode = NavigationCursorMode
        ------------------------------------------------------------------------
        CursorState cursor@{mode: NavigationCursorMode} -> do
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
            mb_str <- case hdzipperDerivTerm cursor.hdzipper of
              DerivString str % _ -> pure (Just str)
              _ -> pure Nothing
            -- activate buffer
            setBufferEnabled true mb_str
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            setBufferEnabled true (Just key)
          else if key == "Backspace" then do
            -- replace dterm with default deriv
            case defaultDerivTerm (derivTermSort dterm) of
              Nothing -> pure unit
              Just dterm' ->  setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm')))
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- CursorState --> TopState
            setFacade $ TopState {dterm: Expr.unzipper (hdzipperZipper cursor.hdzipper)}
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
            setBufferEnabled true Nothing
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
            setFacade $ CursorState cursor
            -- activate buffer
            setBufferEnabled true (Just key)
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
          else pure unit
        ------------------------------------------------------------------------
        -- TopState
        ------------------------------------------------------------------------
        TopState top -> do
          if isBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper mempty top.dterm)))
          else if isJust (readMoveDir key) then
            setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper mempty top.dterm)))
          else pure unit
        ------------------------------------------------------------------------
        -- SmallStepState
        ------------------------------------------------------------------------
        SmallStepState _ -> do
          if key == "Space" then do
            hole "TODO: small step forwards"
          else
            pure unit

    ------------------------------------------------------------------------------
    -- handle buffer output
    ------------------------------------------------------------------------------

    handleBufferOutput = case _ of
      ActionOutput act -> handleAction act
      SetPreviewOutput mb_preview -> do
        HK.tell tokens.slotToken previewSlot leftDir $ SetPreviewQuery mb_preview
        HK.tell tokens.slotToken previewSlot rightDir $ SetPreviewQuery mb_preview
  
    ------------------------------------------------------------------------------
    -- mouse stuff
    ------------------------------------------------------------------------------

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
          SmallStepState _ -> pure unit
        setHighlightElement Nothing
      else do
        setHighlightElement (Just (hdzipperHoleyDerivPath hdzipper))

    ------------------------------------------------------------------------------
    -- arrange
    ------------------------------------------------------------------------------

    arrangeDerivTermSubs ::
      DerivZipper l r ->
      Array (RenderingContext -> EditorHTML l r) ->
      RenderingContext ->
      Array (EditorHTML l r)
    arrangeDerivTermSubs (Expr.Zipper dpath dterm) kidCtxElems renCtx = assert (wellformedExpr "arrangeDerivTermSubs" dterm) \_ -> case dterm of
      DerivLabel r sort % [] | isHoleRule r ->
        arrangeHoleExterior sort (const (renderHoleInterior false dpath sort)) renCtx
      DerivLabel rule sort % kids -> do
        let subCtxSymElems = spec.arrangeDerivTermSubs {renCtx, rule, sort, kids}
        Array.concat $ subCtxSymElems <#> case _ of
          Left (renCtx' /\ kidIx) -> assert (just "arrangeDerivTermSubs" (Array.index kidCtxElems kidIx)) \kidElem -> [kidElem renCtx']
          Right elems -> elems
      DerivString str % [] -> 
        [ if String.null str 
            then HH.div [classNames ["subnode", "string-inner", "empty-string"]] [HH.text "String"]
            else HH.div [classNames ["subnode", "string-inner"]] [HH.text str] ]

    arrangeNodeSubs :: Boolean -> HoleyDerivZipper l r -> Array (EditorHTML l r) -> Array (EditorHTML l r)
    arrangeNodeSubs isCursor hdzipper subElems = Array.concat
      [ if not isCursor then [] else 
        [ HH.slot bufferSlot unit bufferComponent 
            { hdzipper
            , edits: editsAtHoleyDerivZipper spec hdzipper <#>
                \edit -> 
                  { lazy_preview: renderEditPreview hdzipper edit
                  , edit }
            }
            handleBufferOutput
        , HH.slot_ previewSlot leftDir previewComponent leftDir
        ]
      , subElems
      , if not isCursor then [] else
        [ HH.slot_ previewSlot rightDir previewComponent rightDir ]
      ]

    arrangeHoleExterior :: Sort l -> (RenderingContext -> EditorHTML l r) -> RenderingContext -> Array (EditorHTML l r)
    arrangeHoleExterior sort holeInteriorElem renCtx =
      [ HH.div [classNames ["subnode", "holeExterior-inner"]]
        [ HH.div [classNames ["subnode", "hole-interior"]] [holeInteriorElem renCtx]
        , colonElem
        , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
        ]
      ]


    ------------------------------------------------------------------------------
    -- render term
    ------------------------------------------------------------------------------

    renderDerivTerm :: Boolean -> DerivZipper l r -> RenderingContext -> EditorHTML l r
    renderDerivTerm isCursor dzipper renCtx = do
      let
        subElems = arrangeDerivTermSubs dzipper (Zippable.zipDowns dzipper <#> renderDerivTerm false) renCtx
      HH.div
        (Array.concat
          [ [classNames $ ["node"] <> if isCursor then [cursorClassName] else []]
          , if not renCtx.isInteractive then [] else do
            let elemId = fromPathToElementId (Expr.zipperPath dzipper)
            [ HP.id elemId
            , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
            , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper)) 
            ]
          ]) $
        arrangeNodeSubs isCursor (InjectHoleyDerivZipper dzipper) subElems

    ------------------------------------------------------------------------------
    -- render hole exterior and interior
    ------------------------------------------------------------------------------

    -- !TODO I think this should actually use arrangeDerivTermSubs somehow
    renderHoleExterior :: DerivPath Up l r -> Sort l -> (RenderingContext -> EditorHTML l r) -> RenderingContext -> EditorHTML l r
    renderHoleExterior dpath sort holeInteriorElem renCtx = assert (just "renderHoleInterior" (defaultDerivTerm sort)) \dterm -> do
      let
        dzipper = Expr.Zipper dpath dterm
        elemId = fromPathToElementId dpath
        subElems = arrangeHoleExterior sort holeInteriorElem renCtx
      HH.div
        [ classNames ["node"]
        , HP.id elemId
        , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper))
        ] $
        subElems

    renderHoleInterior :: Boolean -> DerivPath Up l r -> Sort l -> EditorHTML l r
    renderHoleInterior isCursor dpath sort = do
      let hdzipper = HoleInteriorHoleyDerivZipper dpath sort
      let elemId = fromHoleyDerivPathToElementId (HoleInteriorHoleyDerivPath dpath)
      HH.div
        [ classNames $ ["node", "holeInterior"] <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown (onMouseDown hdzipper)
        , HE.onMouseOver (onMouseOver hdzipper)
        ] $
        arrangeNodeSubs isCursor hdzipper
          [ HH.div [classNames ["subnode", "holeInterior-inner"]]
            [interrogativeElem]
          ]

    ------------------------------------------------------------------------------
    -- render path
    ------------------------------------------------------------------------------

    renderPath :: DerivZipper l r -> (RenderingContext -> EditorHTML l r) -> RenderingContext -> EditorHTML l r
    renderPath dzipper interior =
      case Expr.zipUp dzipper of
        Nothing -> interior
        Just (th /\ dzipper2) -> do
          let elemId = fromPathToElementId (Expr.zipperPath dzipper2)
          renderPath dzipper2
            (\renCtx -> 
              HH.div
                [ classNames ["node"]
                , HP.id elemId
                , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper2))
                , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper2))
                ] do
                let
                  subElems =
                    arrangeDerivTermSubs 
                      (Expr.Zipper (Expr.zipperPath dzipper2) (Expr.unTooth th (Expr.zipperExpr dzipper)))
                      ( Array.fromFoldable $
                        ZipList.unpathAround interior do
                          let kidZippers = Expr.zipDownsTooth dzipper2 th
                          kidZippers <#> renderDerivTerm false )
                      renCtx
                arrangeNodeSubs false (InjectHoleyDerivZipper dzipper2) subElems
            )

    ------------------------------------------------------------------------------
    -- render preview
    ------------------------------------------------------------------------------

    -- !TODO maybe someday have this use rendering context
    renderPreviewDerivTooth :: DerivPath Up l r -> DerivTooth l r -> DerivTerm l r -> {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
    renderPreviewDerivTooth up dtooth@(Expr.Tooth dl kidsPath) dterm = do
      let rule /\ sort = case dl of
            DerivLabel rule sort -> rule /\ sort
            DerivString _ -> bug "in `renderPreviewDerivTooth`: should not have a tooth with a non-DerivLabel DerivLabel"

      let dzipper = Expr.Zipper up (Expr.unTooth dtooth dterm)
      let kidDZippers = Zippable.zipDowns dzipper
      -- deferred since we know that the kid inside the tooth will not actually
      -- get forced to render
      let kidElems = kidDZippers <#> \dzipper' -> defer \_ -> renderPreviewDerivTerm dzipper'

      let renderSubElem = case _ of
            -- don't use renCtx' here because rendering previews doesn't use
            -- rendering context
            Left (_renCtx' /\ i) -> [force $ fromJust' "renderPreviewDerivTooth" $ kidElems Array.!! i]
            Right elems -> elems

      let subCtxSymElems = spec.arrangeDerivTermSubs {renCtx: previewRenderingContext, rule, sort, kids: Array.fromFoldable $ ZipList.unpathAround dterm kidsPath}
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

    renderPreviewDerivPath :: DerivPath Up l r -> DerivPath Up l r -> DerivTerm l r -> {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
    renderPreviewDerivPath _up (Expr.Path Nil) _dterm = {before: [], after: []}
    renderPreviewDerivPath up (Expr.Path (th : ths)) dterm = do
      let next = renderPreviewDerivPath up (Expr.Path ths) (Expr.unTooth th dterm)
      let {before, after} = renderPreviewDerivTooth up th dterm
      {before: next.before <> before, after: after <> next.after}
    
    renderEditPreview :: HoleyDerivZipper l r -> Edit l r -> Lazy (EditPreviewHTML l r)
    renderEditPreview hdzipper edit = edit.action <#> case _ of
      FillAction {dterm} -> FillEditPreview $ renderPreviewDerivTerm (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
      ReplaceAction {dterm} -> ReplaceEditPreview $ renderPreviewDerivTerm (Expr.Zipper (hdzipperDerivPath hdzipper) dterm) 
      WrapAction {dpath} -> WrapEditPreview $ renderPreviewDerivPath (hdzipperDerivPath hdzipper) dpath (hdzipperDerivTerm hdzipper)

    renderPreviewDerivTerm :: DerivZipper l r -> EditorHTML l r
    renderPreviewDerivTerm dzipper = do
      let subElems = arrangeDerivTermSubs 
            dzipper 
            (Zippable.zipDowns dzipper <#> \kidDZipper _ -> renderPreviewDerivTerm kidDZipper) 
            previewRenderingContext
      HH.div
        [classNames ["node"]]
        subElems

  ------------------------------------------------------------------------------
  -- lifecycle
  ------------------------------------------------------------------------------

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

  ------------------------------------------------------------------------------
  -- render
  ------------------------------------------------------------------------------

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
                  InjectHoleyDerivZipper _ -> 
                    [ renderPath dzipper 
                          (renderDerivTerm true dzipper)
                        defaultRenderingContext
                    ]
                  HoleInteriorHoleyDerivZipper dpath sort -> 
                    [ renderPath dzipper 
                        (renderHoleExterior dpath sort
                            (const (renderHoleInterior true dpath sort)))
                        defaultRenderingContext
                    ]
              SelectState _select -> hole "render SelectState"
              TopState _top -> hole "render TopState"
              SmallStepState _ss -> hole "render SmallStepState"
          ]