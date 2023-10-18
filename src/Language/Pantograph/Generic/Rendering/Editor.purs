module Language.Pantograph.Generic.Rendering.Editor where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Rendering.Base
import Prelude

import Bug (bug)
import Bug.Assertion (assert, just)
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Tuple as Tuple
import Data.Variant (default, on)
import Debug as Debug
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, setClassName)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Console (_consoleSlot, consoleComponent)
import Language.Pantograph.Generic.Rendering.Rendering (renderDerivTerm, renderHoleExterior, renderHoleInterior, renderPath, renderSSTerm)
import Language.Pantograph.Generic.Smallstep (setupSSTermFromReplaceAction, setupSSTermFromWrapAction)
import Language.Pantograph.Generic.Smallstep as SmallStep
import Language.Pantograph.Generic.ZipperMovement (moveZipperp)
import Log (log, logM)
import Text.Pretty (bullets, pretty)
import Text.Pretty as P
import Type.Direction (Up, _down, _next, leftDir, readMoveDir, readVerticalDir, rightDir)
import Web.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.MouseEvent as MouseEvent
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Util as Util
import Language.Pantograph.Generic.Unification as Unification
import Debug (trace, traceM)

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
   -- JACOB: A useful constraint to have here is that the path should be non-empty
   -- It is useless to have an empty path in the clipboard, and it also makes some computations more annoying to have to deal with that case.
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

    doSmallstep :: SmallStep.SSTerm l r -> HK.HookM Aff Unit
    doSmallstep ssterm = do
        -- NOTE: uncomment this line and comment the following two lines to make it step one-by-one through smallstep
--        setState $ SmallStepState {ssterm}
        let final = SmallStep.stepRepeatedly ssterm spec.stepRules
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (SmallStep.termToZipper final)))

    handleAction = case _ of
      WrapAction {topChange, dpath, botChange} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        let ssterm = setupSSTermFromWrapAction
              up
              topChange 
              dpath 
              botChange 
              dterm
        doSmallstep ssterm

      FillAction {sub, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dzipper0 = Expr.Zipper up dterm
        let dzipper1 = subDerivZipper sub dzipper0
--        traceM "resetting cursor to top because the rendering code is buggy" -- TODO: delete this and the next line after rendering is fixed
--        let dzipper1' = Expr.zipAllTheWayUp dzipper1
--        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper1'))
        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper dzipper1))

      -- !TODO use topChange
      ReplaceAction {topChange, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper

--         TODO: this is old, doesn't even do smallstep
--        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper up dterm)))

        -- !TODO us this, same way as in WrapAction
        let ssterm = setupSSTermFromReplaceAction
              up
              topChange
              dterm

        doSmallstep ssterm

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          case moveHDZUntil dir (isValidCursor spec) cursor.hdzipper of
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

    -- Takes a term to be added to the clipboard, and generalizes it before adding it to the clipboard
    genAndCopyClipTerm :: DerivTerm l r -> HK.HookM Aff Unit
    genAndCopyClipTerm dterm = do
        let generalizingChange = spec.generalizeDerivation (derivTermSort dterm)
--        traceM ("genCh is : " <> pretty generalizingChange <> " and unifiedDTerm is " <> pretty unifiedDTerm)
        let generalizedDTerm = SmallStep.assertJustExpr
                (SmallStep.stepRepeatedly (SmallStep.wrapBoundary SmallStep.Down generalizingChange
                    (SmallStep.termToSSTerm dterm)) spec.stepRules)
        let forgottenDTerm = map (forgetDerivLabelSorts spec.forgetSorts) generalizedDTerm
        let unifyingSub' = Util.fromJust' "shouldn't fail if term typechecks" $ infer forgottenDTerm
        let expectedClipSort = spec.clipboardSort (derivTermSort forgottenDTerm)
        let forgottenTopSort = Expr.subMetaExprPartially unifyingSub' (derivTermSort forgottenDTerm)
        let unifyingSub = Unification.composeSub unifyingSub'
                (Tuple.snd $ Util.fromJust' "gacct shouldn't fail" $ (Unification.unify expectedClipSort forgottenTopSort))
        let unifiedDTerm = subDerivTerm unifyingSub forgottenDTerm
        liftEffect $ Ref.write (Just (Right unifiedDTerm)) clipboard_ref

    -- Takes a path to be added to the clipboard, and generalizes it before adding it to the clipboard
    -- The path to be copied should be nonempty
    genAndCopyClipPath :: DerivPath Up l r -> HK.HookM Aff Unit
    genAndCopyClipPath dpath = do
        let generalizingChange = spec.generalizeDerivation (nonemptyUpPathTopSort dpath)
        let _upChange /\ generalizedDPath /\ _downChange = SmallStep.ssTermToChangedPath
                (SmallStep.stepRepeatedly (SmallStep.wrapBoundary SmallStep.Down generalizingChange
                    (SmallStep.wrapPath dpath (SmallStep.Marker 0 % []))) spec.stepRules)
        let forgottenDPath = map (forgetDerivLabelSorts spec.forgetSorts) generalizedDPath
        let unifyingSub' = Util.fromJust' "shouldn't fail if term typechecks" $ inferPath (nonemptyPathInnerSort forgottenDPath) forgottenDPath
        let expectedClipSort = spec.clipboardSort (nonemptyUpPathTopSort forgottenDPath)
        let forgottenTopSort = Expr.subMetaExprPartially unifyingSub' (nonemptyUpPathTopSort forgottenDPath)
        let unifyingSub = Unification.composeSub unifyingSub'
                (Tuple.snd $ Util.fromJust' "gacct shouldn't fail" $ (Unification.unify expectedClipSort forgottenTopSort))
        let unifiedDPath = subDerivPath unifyingSub forgottenDPath
        traceM ("going into clipboard is path: " <> pretty unifiedDPath <> " with top sort " <> pretty (nonemptyUpPathTopSort unifiedDPath) <> " and bottom sort " <> pretty (nonemptyPathInnerSort unifiedDPath))
        liftEffect $ Ref.write (Just (Left unifiedDPath)) clipboard_ref

    -- Deletes the term at the cursor and enters smallstep with a change going up
    deleteTermAtCursor :: DerivPath Up l r -> DerivTerm l r -> HK.HookM Aff Unit
    deleteTermAtCursor restOfProg dterm = do
        let upChange = spec.onDelete (derivTermSort dterm)
        case defaultDerivTerm (ChangeAlgebra.rEndpoint upChange) of
          Nothing -> pure unit
          Just dterm' -> do
--                setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm')))
            let ssterm = setupSSTermFromReplaceAction
                  restOfProg
                  upChange
                  dterm'
            doSmallstep ssterm

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
            -- TODO: Question for Henry: why doesn't this have preventDefault?
            -- update clipboard
            genAndCopyClipTerm dterm
          -- cut
          else if cmdKey && key == "x" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- update clipboard
            genAndCopyClipTerm dterm
            -- replace cursor with default deriv
            deleteTermAtCursor path dterm
          else if cmdKey && key == "v" then do
            liftEffect (Ref.read clipboard_ref) >>= case _ of
              Nothing -> pure unit -- nothing in clipboard
              Just (Left clipDPath) -> do
                liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
                -- paste a path
                -- First, specialize the path using the specializingChange from EditorSpec, which generally is used for putting the path into the cursor's context and thus updating variables that appear in the path
                let specializingChange = spec.specializeDerivation (nonemptyUpPathTopSort clipDPath) (derivTermSort dterm)
                let _upChange /\ specializedClipDPath /\ _downChange = SmallStep.ssTermToChangedPath
                        (SmallStep.stepRepeatedly (SmallStep.wrapBoundary SmallStep.Down specializingChange
                            (SmallStep.wrapPath clipDPath (SmallStep.Marker 0 % []))) spec.stepRules)
                -- call splitChange to get the expected sort that the path should unify with, and the changes that will be sent up and down
                let {downChange, upChange, cursorSort} =
                      spec.splitChange
                        (SmallStep.getPathChange spec.languageChanges specializedClipDPath (nonemptyPathInnerSort specializedClipDPath))
                -- Then, unify to make sure the types line up
                case Unification.unify cursorSort (derivTermSort dterm) of
                    Just (_newSort /\ unifyingSub) -> do
                        -- update everything with the substitution
                        let unifiedClipDPath = subDerivPath unifyingSub specializedClipDPath
                        let unifiedProgPath = subDerivPath unifyingSub path
                        let unifiedProgDTerm = subDerivTerm unifyingSub dterm
                        let unifiedDownChange = ChangeAlgebra.subSomeMetaChange unifyingSub downChange
                        let unifiedUpChange = ChangeAlgebra.subSomeMetaChange unifyingSub upChange
                        -- Now, we set up smallstep for changing the program on path paste
                        let ssterm = setupSSTermFromWrapAction
                              unifiedProgPath
                              unifiedUpChange
                              unifiedClipDPath
                              (ChangeAlgebra.invert unifiedDownChange)
                              unifiedProgDTerm
                        doSmallstep ssterm
                    Nothing -> do -- Didn't unify; can't paste
                        pure unit
              Just (Right clipDTerm) -> do
                liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
                -- paste a dterm:
                -- First, specialize the term
                let specializingChange = spec.specializeDerivation (derivTermSort clipDTerm) (derivTermSort dterm)
                let specializedDTerm = SmallStep.assertJustExpr
                        (SmallStep.stepRepeatedly (SmallStep.wrapBoundary SmallStep.Down specializingChange
                            (SmallStep.termToSSTerm clipDTerm)) spec.stepRules)
                -- Then, unify to make sure the types line up
                case Unification.unify (derivTermSort specializedDTerm) (derivTermSort dterm) of
                    Just (_newSort /\ unifyingSub) -> do
                        let unifiedDTerm = subDerivTerm unifyingSub specializedDTerm
                        let unifiedPath = subDerivPath unifyingSub path
                        setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper unifiedPath unifiedDTerm)))
                    Nothing -> do -- Didn't unify; can't paste
                        pure unit
          else if cmdKey && key == "s" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            Debug.traceM $ pretty $ 
              "[print sort]" <> bullets
                [ "path = " <> pretty path
                , "dterm = " <> pretty dterm
                , "sort = " <> pretty (derivTermSort dterm)
                ]
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
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- replace dterm with default deriv, and propagate change up from onDelete
            deleteTermAtCursor path dterm
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- CursorState --> TopState
            setFacade $ TopState {dterm: Expr.unzipper (hdzipperDerivZipper cursor.hdzipper)}
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
          else pure unit
        ------------------------------------------------------------------------
        -- SelectState
        ------------------------------------------------------------------------
        SelectState select -> do
          let deleteSelection = do
                let Expr.Zipperp path selection dterm = select.dzipperp
                let sort = derivTermSort dterm
                let selection'' = case selection of
                      Left p -> Expr.reversePath p -- Expr.toUpPath p
                      Right p -> p -- Expr.toUpPath p
                let {downChange, upChange, cursorSort: _} =
                      spec.splitChange
                        (SmallStep.getPathChange spec.languageChanges selection'' sort)
                let ssterm = setupSSTermFromWrapAction
                      path
                      (ChangeAlgebra.invert upChange)
                      (Expr.Path mempty)
                      downChange
                      dterm
                doSmallstep ssterm

          let Expr.Zipperp path selection dterm = select.dzipperp
          -- copy
          if cmdKey && key == "c" then do
            -- update clipboard
            genAndCopyClipPath (either Expr.reversePath identity selection)
          -- cut
          else if cmdKey && key == "x" then do
            -- update clipboard
            genAndCopyClipPath (either Expr.reversePath identity selection)
            -- then delete the selection
            deleteSelection
          else if key == "Escape" then do
            -- SelectState --> CursorState
            setFacade $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.unzipperp select.dzipperp)))
          else if key == "Backspace" then deleteSelection
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
        SmallStepState ss -> do
          if key == " " then do
            case SmallStep.step ss.ssterm spec.stepRules of
              Nothing -> setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (SmallStep.termToZipper ss.ssterm)))
              Just ssterm -> setState $ SmallStepState ss {ssterm = ssterm}
          else if key == "Enter" then do
            let final = SmallStep.stepRepeatedly ss.ssterm spec.stepRules
            setState $ CursorState (cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (SmallStep.termToZipper final)))
          else
            pure unit

    ------------------------------------------------------------------------------
    -- handle buffer output
    ------------------------------------------------------------------------------

    handleBufferOutput :: Output l r -> HK.HookM Aff Unit
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
      let dzipper = (hdzipperDerivZipper hdzipper)
      if MouseEvent.buttons event Bits..&. 1 /= 0 then do
        getFacade >>= case _ of
          CursorState cursor -> do
            case Expr.zipperpFromTo (hdzipperDerivZipper cursor.hdzipper) dzipper of
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
  -- locals
  ------------------------------------------------------------------------------

  let 
    locs :: EditorLocals l r
    locs = 
      { spec
      , handleBufferOutput
      , onMouseDown
      , onMouseOver
      }

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
    log "editorComponent.render" (P.bullets 
      [ "currentState = " <> pretty currentState ]
    ) \_ ->
    HH.div [classNames ["editor"]]
    [ HH.div
      [ classNames ["status"] ]
      [ HH.table_
        [ HH.tr_ [HH.td_ [HH.text "mode"], HH.td_ [HH.text case currentState of
            CursorState _ -> "cursor"
            SelectState _ -> "cursor"
            TopState _ -> "cursor"
            SmallStepState _ -> "smallstep"] ]
        ]
      ]
    , HH.div 
      [ classNames ["program"]
      , HE.onMouseLeave \event -> do
          H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
          setHighlightElement Nothing
      ]
      case currentState of
        CursorState cursor -> do
          let dzipper = hdzipperDerivZipper cursor.hdzipper
          case cursor.hdzipper of
            InjectHoleyDerivZipper _ -> 
              [ renderPath locs dzipper 
                    (renderDerivTerm locs true dzipper)
                  defaultRenderingContext
              ]
            HoleInteriorHoleyDerivZipper dpath label ->
              [ renderPath locs dzipper 
                  (renderHoleExterior locs dpath label
                      (renderHoleInterior locs true dpath label))
                  defaultRenderingContext
              ]
        SelectState _select -> hole "render SelectState"
        TopState _top -> hole "render TopState"
        SmallStepState ss -> 
          [HH.div
            [ classNames ["smallstep-program"] ]
            [ renderSSTerm locs ss.ssterm 
                defaultRenderingContext
            ]]
    , HH.slot_ _consoleSlot unit consoleComponent unit
    ]

