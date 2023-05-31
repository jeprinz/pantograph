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

  -- let 
  --   locals :: EditorLocals l r
  --   locals = 
  --       { input
  --       , initState
  --       , currentState
  --       , state_id
  --       , facade_ref
  --       , clipboard_ref
  --       , maybeHighlightPath_ref
  --       }

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

    getCursorState :: String -> HK.HookM Aff (Cursor l r)
    getCursorState source = do
      st <- getState
      assert (cursorState source st) pure

    handleAction = case _ of
      -- !TODO use topChange, botChange
      WrapAction {topChange, dpath, botChange} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper (dpath <> up) dterm)))
      -- !TODO use sub
      FillAction {sub, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper up dterm)))
      -- !TODO use topChange
      ReplaceAction {topChange, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper up dterm)))

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          -- Debug.traceM $ "[moveCursor] at hole; NOT going down to hole interior"
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

    setBufferEnabled :: String -> Boolean -> Maybe String -> HK.HookM Aff Unit
    setBufferEnabled elemId isEnabled mb_str = do
      HK.tell tokens.slotToken bufferSlot elemId $ SetBufferEnabledQuery isEnabled mb_str

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
            setBufferEnabled elemId false Nothing
          else if isJust (readVerticalDir key) then
            assert (just "handleKeyboardEvent" $ readVerticalDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
              HK.tell tokens.slotToken bufferSlot elemId $ MoveBufferQuery dir
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
            case hdzipperDerivTerm cursor.hdzipper of
              DerivString str % _ -> do
                elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
                setBufferEnabled elemId true (Just str)
              _ -> do
                -- activate buffer
                elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
                setBufferEnabled elemId true Nothing
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            setBufferEnabled elemId true (Just key)
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
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            setBufferEnabled elemId true Nothing
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
            setFacade $ CursorState cursor
            -- activate buffer
            elemId <- getElementIdByHoleyDerivPath (hdzipperHoleyDerivPath cursor.hdzipper)
            setBufferEnabled elemId true (Just key)
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

    handleBufferOutput elemId = case _ of
      ActionOutput act -> handleAction act
      UpdateFacadeOutput f -> setFacade =<< (f =<< getFacade)
      SetPreviewOutput {before, after} -> do
        HK.tell tokens.slotToken previewSlot (elemId /\ leftDir) $ SetPreviewQuery before
        HK.tell tokens.slotToken previewSlot (elemId /\ rightDir) $ SetPreviewQuery after
  
    prerenderDerivZipper :: 
      DerivZipper l r ->
      Array (EditorHTML l r) ->
      { classNames :: Array String
      , subElems :: Array (EditorHTML l r) }
    prerenderDerivZipper (Expr.Zipper dpath dterm) kidElems = assert (wellformedExpr "prerenderDerivZipper" dterm) \_ -> case dterm of
      DerivLabel r sort % [] | isHoleRule r ->
        { classNames: ["hole"]
        , subElems: [ HH.div [classNames ["subnode", "inner"]]
              [ HH.div [classNames ["subnode", "hole-interior"]] [renderHoleInterior false dpath sort]
              , colonElem
              , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
              ]
          ]
        }
      DerivLabel rule sort % kids -> do
        let {classNames, subSymElems} = input.prerenderDerivTerm {rule, sort, kids}
        { classNames
        , subElems: Array.concat $ subSymElems <#> case _ of
            Left kidIx -> assert (just "prerenderDerivZipper" (Array.index kidElems kidIx)) \kidElem -> [kidElem]
            Right elems -> elems
        }
      DerivString str % [] -> 
        { classNames: ["string"]
        , subElems: [ if String.null str 
              then HH.div [classNames ["subnode", "inner", "empty-string"]] [HH.text "String"]
              else HH.div [classNames ["subnode", "inner"]] [HH.text str]
          ]
        }

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

    renderNodeSubElems :: String -> DerivZipper l r -> Array (EditorHTML l r) -> Array (EditorHTML l r)
    renderNodeSubElems elemId dzipper subElems = Array.concat
      [ [ HH.slot bufferSlot elemId bufferComponent 
            { hdzipper: InjectHoleyDerivZipper dzipper
            , edits: input.editsAtHoleyDerivZipper input.topSort (InjectHoleyDerivZipper dzipper) <#>
                \edit -> 
                  { lazy_preview: renderEditPreview dzipper edit
                  , edit }
            } 
            (handleBufferOutput elemId)
        , HH.slot_ previewSlot (elemId /\ leftDir) previewComponent unit
        ]
      , subElems
      , [HH.slot_ previewSlot (elemId /\ rightDir) previewComponent unit]
      ]

    renderExpr :: Boolean -> DerivZipper l r -> EditorHTML l r
    renderExpr isCursor dzipper = do
      let
        elemId = fromPathToElementId (Expr.zipperPath dzipper)
        {classNames: cns, subElems} = prerenderDerivZipper dzipper $ renderExpr false <<< snd <$> Expr.zipDowns dzipper
      HH.div
        [ classNames $ ["node"] <> cns <> if isCursor then [cursorClassName] else []
        , HP.id elemId
        , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
        , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper))
        ] $
        renderNodeSubElems elemId dzipper subElems

    renderPhantomDerivTerm dzipper = do
      let {classNames: cns, subElems} = prerenderDerivZipper dzipper $ renderPhantomDerivTerm <<< snd <$> Expr.zipDowns dzipper
      HH.div
        [classNames $ ["node"] <> cns]
        subElems

    renderHoleInterior isCursor dpath sort = assert (just "renderHoleInterior" (defaultDerivTerm sort)) \dterm -> do
      let dzipper = Expr.Zipper dpath dterm
      -- let hdzipper = HoleInteriorHoleyDerivZipper dpath sort
      (\kidElem -> 
        if isCursor then do
          let
            elemId = fromPathToElementId dpath
            clsNames = ["hole"]
          HH.div
            [ classNames $ ["node"] <> clsNames
            , HP.id elemId
            , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper))
            , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper))
            ] $
            -- !TODO there used to be a buffer here, but pretty sure i dont need
            -- a buffer on the outside of the hole if there's already one
            -- rendered inside the hole
            [ HH.div [classNames ["subnode", "inner"]]
              [ HH.div [classNames ["subnode", "hole-interior"]] [kidElem]
              , colonElem
              , HH.div [classNames ["subnode", "hole-sort"]] [HH.text (pretty sort)] 
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
          renderNodeSubElems elemId dzipper
            [ HH.div [classNames ["subnode", "inner"]]
              [interrogativeElem]
            ]

    renderPath dzipper interior  = do
      case Expr.zipUp dzipper of
        Nothing -> interior
        Just (th /\ dzipper2) -> do
          let
            elemId = fromPathToElementId (Expr.zipperPath dzipper2)
            -- _ = Debug.trace ("[renderPath] Expr.zipDownsTooth dzipper2 th = " <> show (pretty <$> Expr.zipDownsTooth dzipper2 th)) \_ -> unit
            {classNames: cns, subElems} =
              -- prerenderDerivZipper (Expr.unTooth th (Expr.zipperExpr dzipper)) $
              prerenderDerivZipper (Expr.Zipper (Expr.zipperPath dzipper2) (Expr.unTooth th (Expr.zipperExpr dzipper))) $
              Array.fromFoldable $
              ZipList.unpathAround interior $ do
                let kidZippers = Expr.zipDownsTooth dzipper2 th
                renderExpr false <$> kidZippers
          renderPath dzipper2 $
            HH.div
              [ classNames $ ["node"] <> cns
              , HP.id elemId
              , HE.onMouseDown (onMouseDown (InjectHoleyDerivZipper dzipper2))
              , HE.onMouseOver (onMouseOver (InjectHoleyDerivZipper dzipper2))
              ] $
              renderNodeSubElems elemId dzipper2 subElems

    renderPreviewDerivTooth :: DerivPath Up l r -> DerivTooth l r -> DerivTerm l r -> {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
    renderPreviewDerivTooth up dtooth@(Expr.Tooth dl kidsPath) dterm = do
      let rule /\ sort = case dl of
            DerivLabel rule sort -> rule /\ sort
            DerivString s -> bug "in `renderPreviewDerivTooth`: should not have a tooth with a non-DerivLabel DerivLabel"

      let dzipper = Expr.Zipper up (Expr.unTooth dtooth dterm)
      let kidDZippers = Zippable.zipDowns dzipper
      -- deferred since we know that the kid inside the tooth will not actually
      -- get forced to render
      let kidElems = kidDZippers <#> \dzipper -> defer \_ -> renderPhantomDerivTerm dzipper

      let renderSubElem = case _ of
            Left i -> [force $ fromJust' "renderPreviewDerivTooth" $ kidElems Array.!! i]
            Right elems -> elems

      let {classNames, subSymElems} = input.prerenderDerivTerm {rule, sort, kids: Array.fromFoldable $ ZipList.unpathAround dterm kidsPath}
      let toothInteriorKidIx = ZipList.leftLength kidsPath
      let isToothInterior = case _ of
            Left i -> i == toothInteriorKidIx
            _ -> false
      let toothInteriorSymElemIx = fromJust' "renderPreviewDerivTooth" $ Array.findIndex isToothInterior subSymElems
      let before = Array.take toothInteriorSymElemIx subSymElems
      let after = Array.drop (toothInteriorSymElemIx + 1) subSymElems
            
      { before: Array.concat $ before <#> renderSubElem
      , after: Array.concat $ after <#> renderSubElem
      }

    renderPreviewDerivPath :: DerivPath Up l r -> DerivPath Up l r -> DerivTerm l r -> {before :: Array (EditorHTML l r), after :: Array (EditorHTML l r)}
    renderPreviewDerivPath _up (Expr.Path Nil) _dterm = {before: [], after: []}
    renderPreviewDerivPath up (Expr.Path (th : ths)) dterm = do
      let next = renderPreviewDerivPath up (Expr.Path ths) (Expr.unTooth th dterm)
      let {before, after} = renderPreviewDerivTooth up th dterm
      {before: next.before <> before, after: after <> next.after}
    
    renderEditPreview :: DerivZipper l r -> Edit l r -> Lazy (EditPreviewHTML l r)
    renderEditPreview dzipper edit = edit.action <#> case _ of
      FillAction {dterm} -> FillEditPreview $ renderPhantomDerivTerm (Expr.Zipper (Expr.zipperPath dzipper) dterm) 
      ReplaceAction {dterm} -> ReplaceEditPreview $ renderPhantomDerivTerm (Expr.Zipper (Expr.zipperPath dzipper) dterm) 
      WrapAction {dpath} -> WrapEditPreview $ renderPreviewDerivPath (Expr.zipperPath dzipper) dpath (Expr.zipperExpr dzipper)

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