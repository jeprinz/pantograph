module Language.Pantograph.Generic.Rendering.Editor where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Rendering.Base
import Prelude

import Bug (bug)
import Bug.Assertion (assert, just)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (encodeJson)
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Int as Int
import Data.Int.Bits as Bits
import Data.Lazy as Lazy
import Data.List (List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.Variant (case_)
import Data.Variant (case_, on, default)
import Debug (trace, traceM)
import Debug as Debug
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, encode_uri_string, get_url_search_param, setClassName)
import Hole (hole)
import Language.Pantograph.Generic.ChangeAlgebra as ChangeAlgebra
import Language.Pantograph.Generic.Grammar as Grammar
import Language.Pantograph.Generic.Rendering.Console (_consoleSlot, consoleComponent)
import Language.Pantograph.Generic.Rendering.Rendering (renderDerivTerm, renderHoleInterior, renderPath, renderSSTerm)
import Language.Pantograph.Generic.Smallstep (setupSSTermFromReplaceAction, setupSSTermFromWrapAction)
import Language.Pantograph.Generic.Smallstep as SmallStep
import Language.Pantograph.Generic.Unification as Unification
import Language.Pantograph.Generic.ZipperMovement (moveZipperpUntil)
import Language.Pantograph.Generic.ZipperMovement (normalizeZipperp)
import Language.Pantograph.UserStudy.Programs as UserStudyPrograms
import Log (log, logM)
import Text.Pretty (bullets, pretty)
import Text.Pretty as P
import Type.Direction (Up, _down, _next, leftDir, readMoveDir, readVerticalDir, rightDir, nextDir, prevDir, MoveDir)
import Type.Direction (_prev)
import Util as Util
import Web.DOM as DOM
import Web.DOM.Element as Element
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as EventTypes
import Web.UIEvent.MouseEvent as MouseEvent

editorPrefix :: Util.Stateful Int
editorPrefix = Util.stateful 0

data EditorQuery l r a =
    SetProgram (DerivTerm l r) (Array (HoleyDerivPath l r)) a
    | GetProgram (DerivTerm l r -> a)

editorComponent :: forall q l r.
  IsRuleLabel l r =>
  Unit -> H.Component (EditorQuery l r) (EditorSpec l r) Unit Aff
editorComponent _unit =
    let editorIdPrefixNum = editorPrefix.get unit in -- Because we are using HTML ids to identify elements of the editor in the DOM, we need a unique prefix for each editor component so the ids don't clash with one another. Yes, its stupid.
    let _ = editorPrefix.set (editorIdPrefixNum + 1) in
    let pathIdPrefix = "Editor" <> show editorIdPrefixNum <> "-" in
    -- let pathIdPrefix = "TEST" in
    HK.component \tokens spec -> HK.do

  ------------------------------------------------------------------------------
  -- initialize state and refs
  ------------------------------------------------------------------------------

  let
    initState = unsafePerformEffect do
      -- program_string <- get_url_search_param "program"
      -- if String.null program_string then
      --   pure $ CursorState $ cursorFromHoleyDerivZipper $ injectHoleyDerivZipper $ Expr.Zipper mempty spec.dterm
      -- else do
      --   let dterm = Grammar.decodeSerializedZipper2 spec.clipboardSort program_string
      --   pure $ CursorState $ cursorFromHoleyDerivZipper $ injectHoleyDerivZipper $ Expr.Zipper mempty dterm
      let default = pure $ CursorState $ cursorFromHoleyDerivZipper $ injectHoleyDerivZipper $ Expr.Zipper mempty spec.dterm
      param <- get_url_search_param "UserStudyProgramIndex"
      if String.null param then
        default
      else do
        case Int.fromString param of 
          Nothing -> default
          Just i -> do
            case UserStudyPrograms.program_strings Array.!! i of 
              Nothing -> default 
              Just program_string -> do
                let dterm = Grammar.decodeSerializedZipper2 spec.clipboardSort program_string
                pure $ CursorState $ cursorFromHoleyDerivZipper $ injectHoleyDerivZipper $ Expr.Zipper mempty dterm

  -- state
  currentState /\ state_id <- HK.useState $ initState
  _ /\ stateIsStale_ref <- HK.useRef false

  -- history
  _ /\ history_ref <- HK.useRef $ (pure initState :: List (State l r))

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

    getElementIdByHoleyDerivPath :: HoleyDerivPath l r -> HK.HookM Aff String
    getElementIdByHoleyDerivPath = pure <<< fromHoleyDerivPathToElementId pathIdPrefix

    getElementByHoleyDerivPath :: HoleyDerivPath l r -> HK.HookM Aff DOM.Element
    getElementByHoleyDerivPath hdzipper = do
      doc <- liftEffect $ HTML.window >>= Window.document
      elemId <- getElementIdByHoleyDerivPath hdzipper
      liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
        Nothing -> do
          st <- getState
          bug $ "could not find element" <> P.indent ("\n- elemId = " <> elemId <> "\n- hdzipper = " <> pretty hdzipper <> "\n- st = " <> pretty st)
        Just elem -> pure elem

    -- setNodeElementStyle :: String -> Maybe (DerivPath Up l r) -> Maybe (DerivPath Up l r) -> HK.HookM Aff Unit
    setNodeElementStyle :: String -> Maybe (HoleyDerivPath l r) -> Maybe (HoleyDerivPath l r) -> HK.HookM Aff Unit
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

    -- Gets the coordinates of an element of the tree on the screen
    getBoundingClientRectFromPath :: HoleyDerivPath l r -> HK.HookM Aff Element.DOMRect
    getBoundingClientRectFromPath path = do
        elem <- getElementByHoleyDerivPath path
        rect <- H.liftEffect (Element.getBoundingClientRect elem)
        pure rect

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
          setSelectTopElement (Just (injectHoleyDerivPath (Expr.zipperpTopPath select.dzipperp))) Nothing
          setSelectBottomElement (Just (injectHoleyDerivPath (Expr.zipperpBottomPath select.dzipperp))) Nothing
        TopState _top -> do
          setHighlightElement Nothing
        SmallStepState _ss -> do
          setHighlightElement Nothing

    -- | Sets the facade state, which updates all the corresponding UI elements.
    setFacade :: State l r -> HK.HookM Aff Unit
    setFacade st = do
      -- Debug.traceM $ "[setFacade] st = " <> pretty st
      case st of -- If the buffer was enabled, we need to disable it when we move the facade.
        CursorState _ -> do
            state <- getState
            case state of
                CursorState _ ->
                    HK.tell tokens.slotToken bufferSlot unit $ SetBufferEnabledQuery false Nothing
                _ -> pure unit
        _ -> pure unit
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
          setSelectTopElement Nothing (Just (injectHoleyDerivPath (Expr.zipperpTopPath select.dzipperp)))
          setSelectBottomElement Nothing (Just (injectHoleyDerivPath (Expr.zipperpBottomPath select.dzipperp)))
        TopState _top -> do
          pure unit
        SmallStepState _ss -> do
          pure unit
      liftEffect (Ref.write st facade_ref)
      liftEffect $ Ref.write true stateIsStale_ref

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
      -- state is now fresh
      liftEffect $ Ref.write true stateIsStale_ref
      pushHistory st

    getState :: HK.HookM Aff (State l r)
    getState = HK.get state_id

    getCursorState :: String -> HK.HookM Aff (Cursor l r)
    getCursorState source = do
      st <- getState
      assert (cursorState source st) pure

    -- re-infers the sorts of the term, and sets back to cursor state
    finalizeSmallstep :: SmallStep.SSTerm l r -> HK.HookM Aff Unit
    finalizeSmallstep ssterm = do
            let preFinal = SmallStep.termToZipper $ SmallStep.stepRepeatedly Nothing ssterm spec.stepRules
            -- re-infer, which will 1) unlink metavars that don't need to be linked and 2) fill holes with defaults where relevant
            let forgottenFinal = map (forgetDerivLabelSorts spec.forgetSorts) preFinal
            let forgottenFinalSort = (derivZipperTopSort forgottenFinal)
            let expectedProgSort = spec.clipboardSort forgottenFinalSort
            let unifyingSub = Tuple.fst $ Util.fromJust' "fss" $ Unification.runUnifyMonad do
                    _ <- inferZipperFImpl forgottenFinal
                    _ <- Unification.unifyFImpl expectedProgSort forgottenFinalSort
                    pure unit
            let final = subDerivZipper unifyingSub forgottenFinal -- This will also call fillDefaults

            setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper final))


    doSmallstep :: SmallStep.SSTerm l r -> HK.HookM Aff Unit
    doSmallstep ssterm = do
        -- NOTE: set to true to make it step one-by-one through smallstep
        if false then
            setState $ SmallStepState {ssterm}
        else do
            finalizeSmallstep ssterm

    handleAction = case _ of
      WrapAction {topChange, dpath, botChange, sub, cursorGoesInside} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        let ssterm = setupSSTermFromWrapAction cursorGoesInside
--              (subDerivPath sub up)
              (map (subDerivLabel sub) up) -- NOTE: I am intentionally not calling subDerivPath, so that fillDefaults is not called
              topChange
              dpath
              botChange
--              (subDerivTerm sub dterm)
              (map (subDerivLabel sub) dterm)
        doSmallstep ssterm

      FillAction {sub, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper
        let dzipper0 = Expr.Zipper up dterm
        let dzipper1 = subDerivZipper sub dzipper0
        setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper1))

      -- !TODO use topChange
      ReplaceAction {topChange, dterm} -> getCursorState "handleAction" >>= \cursor -> do
        let up = hdzipperDerivPath cursor.hdzipper

--         TODO: this is old, doesn't even do smallstep
--        setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper up dterm)))

        -- !TODO us this, same way as in WrapAction
        let ssterm = setupSSTermFromReplaceAction
              up
              topChange
              dterm

        doSmallstep ssterm

    moveToNextHole = do
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          case moveHDZUntil nextDir (\hdz -> isValidCursor spec.isValidCursorSort hdz && hdzIsHolePosition hdz) cursor.hdzipper of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        _ -> pure unit
    moveToPrevHole = do
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          case moveHDZUntil prevDir (\hdz -> isValidCursor spec.isValidCursorSort hdz && hdzIsHolePosition hdz) cursor.hdzipper of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        _ -> pure unit

    moveCursorVertically verticalOrHorizontal = do -- true is up, false is down
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
            let getPos hdzipper = do
                    getBoundingClientRectFromPath (hdzipperHoleyDerivPath hdzipper)
--                    pure $ (pos.left + pos.right) / 2.0
            startRect <- getPos cursor.hdzipper
            let goUntil dir hdzipper check = do
                    newPos <- getPos hdzipper
                    if check newPos
                        then pure $ hdzipper
                        else case computeCursorMovement dir hdzipper of
                                Nothing -> pure $ hdzipper
                                Just newZipper -> goUntil dir newZipper check
            finalZipper <- if verticalOrHorizontal
                then do
                    pos1 <- goUntil prevDir cursor.hdzipper (\newpos -> newpos.top + 4.0 < startRect.top)
                    goUntil prevDir pos1 (\newpos -> newpos.left <= startRect.left + 4.0)
                else do
                    pos1 <- goUntil nextDir cursor.hdzipper (\newpos -> newpos.top > startRect.top + 4.0)
                    rect1 <- getPos pos1
                    pos2 <- goUntil nextDir pos1 (\newpos -> newpos.left + 4.0 >= startRect.left || newpos.top > rect1.top + 4.0)
                    rect2 <- getPos pos2
                    if rect2.top > rect1.top + 4.0
                        then case computeCursorMovement prevDir pos2 of
                            Nothing -> pure pos2
                            Just x -> pure x
                        else pure pos2
            setFacade $ CursorState (cursorFromHoleyDerivZipper finalZipper)
        _ -> pure unit

    computeCursorMovement :: MoveDir -> HoleyDerivZipper l r -> Maybe (HoleyDerivZipper l r)
    computeCursorMovement dir hdzipper = moveHDZUntil dir (isValidCursor spec.isValidCursorSort) hdzipper

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState {mode: BufferCursorMode} -> pure unit
        CursorState cursor -> do
          case computeCursorMovement dir cursor.hdzipper of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        -- TODO: if cursor is moved in select state, it should set the cursor to the right or left of the selection rather than what it does now.
        -- as it is now, it ignores isValidCursor, and therefore could even cause a crash.
        SelectState select -> do
--          case moveHoleyDerivZipper dir (injectHoleyDerivZipper dzipper) of
--            Nothing -> pure unit
--            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
--            (case_
--              # on _up ?h
--              # on default (\_ -> Zippable.zipRight)) dir
          let atTop = (default false #on _prev (\_ -> true)) dir
          setFacade $ CursorState (cursorFromHoleyDerivZipper
            (injectHoleyDerivZipper (Expr.zipperpToZipper atTop select.dzipperp)))
--            (injectHoleyDerivZipper dzipper))
        TopState top -> do
          let dzipper = Expr.Zipper mempty top.dterm
          case moveHoleyDerivZipper dir (injectHoleyDerivZipper dzipper) of
            Nothing -> pure unit
            Just hdzipper' -> setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper')
        SmallStepState _ -> pure unit

    normalizeZipperpToState zipperp = case zipperp of
          (Left dzipper) -> CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper))
          (Right dzipperp) -> SelectState {dzipperp}

    moveSelect dir = getFacade >>= case _ of
      CursorState cursor -> do
        let path = hdzipperDerivPath cursor.hdzipper
        let dterm = hdzipperDerivTerm cursor.hdzipper
        let select = {dzipperp: Expr.Zipperp path (Left mempty) dterm}
        case moveZipperpUntil dir (isValidSelect spec) select.dzipperp of
          Nothing -> do
            logM "moveSelect" "failed to enter SelectState"
            pure unit
          Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper))
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      SelectState select -> do
        case moveZipperpUntil dir (isValidSelect spec) select.dzipperp of
          Nothing -> do
            logM "moveSelect" "failed to move selection"
          Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper))
          Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      TopState top -> do
        let mb_select = (_ $ dir) $ default Nothing -- (pure unit)
              # on _down (\_ -> Just {dzipperp: Expr.Zipperp mempty (Left mempty) top.dterm})
              # on _next (\_ -> Just (hole "moveSelect next when TopState"))
        case mb_select of
          Nothing -> pure unit
          Just select -> case moveZipperpUntil dir (isValidSelect spec) select.dzipperp of
            Nothing -> pure unit
            Just (Left dzipper) -> setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper))
            Just (Right dzipperp) -> setFacade $ SelectState select {dzipperp = dzipperp}
      SmallStepState _ -> pure unit

    setBufferEnabled :: Boolean -> Maybe String -> HK.HookM Aff Unit
    setBufferEnabled isEnabled mb_str = do
      cursor <- do
        st <- getFacade
        assert (cursorState "setBufferEnabled" st) pure
--      traceM ("in setBufferEnabled, cursor is: " <> pretty cursor.hdzipper)
      setState $ CursorState cursor {mode = if isEnabled then BufferCursorMode else NavigationCursorMode}
      HK.tell tokens.slotToken bufferSlot unit $ SetBufferEnabledQuery isEnabled mb_str

    -- Takes a term to be added to the clipboard, and generalizes it before adding it to the clipboard
    genAndCopyClipTerm :: DerivTerm l r -> HK.HookM Aff Unit
    genAndCopyClipTerm dterm = do
        let generalizingChange = spec.generalizeDerivation (derivTermSort dterm)
--        traceM ("genCh is : " <> pretty generalizingChange <> " and unifiedDTerm is " <> pretty unifiedDTerm)
        let generalizedDTerm = SmallStep.assertJustExpr
                (SmallStep.stepRepeatedly Nothing (SmallStep.wrapBoundary SmallStep.Down generalizingChange
                    (SmallStep.termToSSTerm dterm)) spec.stepRules)
        let forgottenDTerm = map (forgetDerivLabelSorts spec.forgetSorts) generalizedDTerm
        let unifyingSub' = Util.fromJust' "shouldn't fail if term typechecks" $ inferF forgottenDTerm
        let expectedClipSort = spec.clipboardSort (derivTermSort forgottenDTerm)
        let forgottenTopSort = Expr.subMetaExprPartially unifyingSub' (derivTermSort forgottenDTerm)
        let unifyingSub = Unification.composeSub unifyingSub'
                (Tuple.snd $ Util.fromJust' "gacct shouldn't fail" $ (Unification.unify expectedClipSort forgottenTopSort))
        let unifiedDTerm = subDerivTerm unifyingSub forgottenDTerm
        liftEffect $ Ref.write (Just (Right unifiedDTerm)) clipboard_ref

    goToInnerHoleIfPossible :: HK.HookM Aff Unit
    goToInnerHoleIfPossible = do
        st <- getFacade
        case st of
            CursorState cursor@{hdzipper} -> setFacade (CursorState cursor{hdzipper = hdzEnterInnerHoleIfPossible hdzipper})
            _ -> pure unit

    -- Takes a path to be added to the clipboard, and generalizes it before adding it to the clipboard
    -- The path to be copied should be nonempty
    genAndCopyClipPath :: DerivPath Up l r -> HK.HookM Aff Unit
    genAndCopyClipPath dpath = do
        let generalizingChange = spec.generalizeDerivation (nonemptyUpPathTopSort dpath)
        let _upChange /\ generalizedDPath /\ _downChange = SmallStep.ssTermToChangedPath
                (SmallStep.stepRepeatedly Nothing (SmallStep.wrapBoundary SmallStep.Down generalizingChange
                    (SmallStep.wrapPath dpath (SmallStep.Marker 0 % []))) spec.stepRules)
        let forgottenDPath = map (forgetDerivLabelSorts spec.forgetSorts) generalizedDPath
        let unifyingSub' = Util.fromJust' "shouldn't fail if term typechecks" $ inferPathF (nonemptyPathInnerSort forgottenDPath) forgottenDPath
        let expectedClipSort = spec.clipboardSort (nonemptyUpPathTopSort forgottenDPath)
        let forgottenTopSort = Expr.subMetaExprPartially unifyingSub' (nonemptyUpPathTopSort forgottenDPath)
        let unifyingSub = Unification.composeSub unifyingSub'
                (Tuple.snd $ Util.fromJust' "gacct shouldn't fail" $ (Unification.unify expectedClipSort forgottenTopSort))
        let unifiedDPath = subDerivPath unifyingSub forgottenDPath
--        traceM ("going into clipboard is path: " <> pretty unifiedDPath <> " with top sort " <> pretty (nonemptyUpPathTopSort unifiedDPath) <> " and bottom sort " <> pretty (nonemptyPathInnerSort unifiedDPath))
        liftEffect $ Ref.write (Just (Left unifiedDPath)) clipboard_ref

    -- Deletes the term at the cursor and enters smallstep with a change going up
    deleteTermAtCursor :: DerivPath Up l r -> DerivTerm l r -> HK.HookM Aff Unit
    deleteTermAtCursor restOfProg dterm = do
        let upChange = spec.onDelete (derivTermSort dterm)
        case defaultDerivTerm (ChangeAlgebra.rEndpoint upChange) of
          Nothing -> pure unit
          Just dterm' -> do
--                setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper path dterm')))
            let ssterm = setupSSTermFromReplaceAction
                  restOfProg
                  upChange
                  dterm'
            doSmallstep ssterm
            goToInnerHoleIfPossible
    
    popHistory :: HK.HookM Aff (Maybe (State l r))
    popHistory = do
      history <- liftEffect $ Ref.read history_ref
      case history of
        Nil -> do
          Console.log "[popHistory] empty history"
          pure Nothing
        Cons st history' -> do
          liftEffect $ Ref.write history' history_ref
        -- NOTE: if true, allows you to undo to the cursor position right before
        -- you made an edit, BUT this introduces a new undo point there so you
        -- have to undo twice sometimes
          let allowUndoToCursorPositionBeforeLastEdit = false
          case st of
            CursorState cursor@{mode: BufferCursorMode} | allowUndoToCursorPositionBeforeLastEdit -> 
              pure $ Just $ CursorState cursor {mode = NavigationCursorMode}
            CursorState {mode: BufferCursorMode} | not allowUndoToCursorPositionBeforeLastEdit -> 
              popHistory
            _ -> pure $ Just st
    
    pushHistory :: State l r -> HK.HookM Aff Unit
    pushHistory st = liftEffect $ Ref.modify_ (Cons st) history_ref

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
          if key == "Tab" || key == "Enter" || key == " " then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            HK.tell tokens.slotToken bufferSlot unit SubmitBufferQuery
            when (key == " ") do
              moveToNextHole
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- tell buffer to deactivate -- TODO: this causes an exception if the buffer wasn't active
            setBufferEnabled false Nothing
          else if isJust (readVerticalDir key) then
            assert (just "handleKeyboardEvent" $ readVerticalDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              HK.tell tokens.slotToken bufferSlot unit $ MoveBufferQuery dir
          else pure unit
        ------------------------------------------------------------------------
        -- CursorState where mode = NavigationCursorMode
        ------------------------------------------------------------------------
        facade@(CursorState cursor@{mode: NavigationCursorMode}) -> do
          let path = hdzipperDerivPath cursor.hdzipper
          let dterm = hdzipperDerivTerm cursor.hdzipper
          -- language specific key edit
          if isJust (spec.keyAction key (derivTermSort dterm)) then do
            setState facade
            let action = Util.fromJust $ spec.keyAction key (derivTermSort dterm)
--            HK.raise tokens.outputToken $ ActionOutput action
--            setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper path dterm)))
            handleAction action
          -- undo
          else if cmdKey && key == "z" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            popHistory >>= \_ -> popHistory >>= case _ of
              Nothing -> pure unit
              Just st' -> setState st'
          -- copy
          else if cmdKey && key == "c" then do
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
                        (SmallStep.stepRepeatedly Nothing (SmallStep.wrapBoundary SmallStep.Down specializingChange
                            (SmallStep.wrapPath clipDPath (SmallStep.Marker 0 % []))) spec.stepRules)
                -- call splitChange to get the expected sort that the path should unify with, and the changes that will be sent up and down
                let pathChange = (SmallStep.getPathChange2 specializedClipDPath spec.forgetSorts)
                let {downChange, upChange, cursorSort} = spec.splitChange pathChange
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
                        let ssterm = setupSSTermFromWrapAction false
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
                let _  /\ specializedDTerm = SmallStep.ssTermToChangedTerm
                        (SmallStep.stepRepeatedly Nothing (SmallStep.wrapBoundary SmallStep.Down specializingChange
                            (SmallStep.termToSSTerm clipDTerm)) spec.stepRules)
                -- Then, unify to make sure the types line up
                case Unification.unify (derivTermSort specializedDTerm) (derivTermSort dterm) of
                    Just (_newSort /\ unifyingSub) -> do
                        let unifiedDTerm = subDerivTerm unifyingSub specializedDTerm
                        let unifiedPath = subDerivPath unifyingSub path
                        setState $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper unifiedPath unifiedDTerm)))
                    Nothing -> do -- Didn't unify; can't paste
                        pure unit
                -- If not in a hole, swap the old term back into the clipboard:
                if hdzIsHolePosition cursor.hdzipper then pure unit else
                    genAndCopyClipTerm dterm

          else if cmdKey && key == "s" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            rect <- getBoundingClientRectFromPath (hdzipperHoleyDerivPath cursor.hdzipper)
            Console.log $ pretty $ 
              "[print sort]" <> bullets
                [ "path = " <> pretty path
                , "dterm = " <> pretty dterm
                , "sort = " <> pretty (derivTermSort dterm)
                , "dlabel = " <> pretty (Expr.exprLabel dterm)
                , "rect = " <> show rect
                , "serialized-path = " <> serializePath path
                ]
          else if cmdKey && key == "p" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            if shiftKey then
              Console.log $ encode_uri_string $ Argonaut.stringify $ encodeJson $ hdzipperDerivZipper cursor.hdzipper
            else
              Console.log $ Argonaut.stringify $ encodeJson $ hdzipperDerivZipper cursor.hdzipper
            pure unit
          else if isOpenBufferKey key then do
            -- enter BufferCursorMode or StringCursorMode depending on the dterm
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            mb_str <- case hdzipperDerivTerm cursor.hdzipper of
              DerivLiteral (DataString str) % _ -> pure (Just str)
              DerivLiteral (DataInt n) % _ -> pure (Just (show n))
              _ -> pure Nothing
            -- activate buffer
            setBufferEnabled true mb_str
          else if not cmdKey && (isQueryKey <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- activate buffer
            setBufferEnabled true (Just key)
          else if key == "Backspace" || key == "Delete" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- replace dterm with default deriv, and propagate change up from onDelete
            deleteTermAtCursor path dterm
          else if key == "Escape" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- CursorState --> TopState
            setFacade $ TopState {dterm: Expr.unzipper (hdzipperDerivZipper cursor.hdzipper)}
          else if key == "ArrowUp" then
            moveCursorVertically true
          else if key == "ArrowDown" then
            moveCursorVertically false
          else if isJust (readMoveDir key) then
            assert (just "handleKeyboardEvent" $ readMoveDir key) \dir -> do
              liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
              (if shiftKey then moveSelect else moveCursor) dir
          else if shiftKey && key == "Tab" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            moveToPrevHole
          else if key == " " || key == "Tab" then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            moveToNextHole
          else pure unit
        ------------------------------------------------------------------------
        -- SelectState
        ------------------------------------------------------------------------
        SelectState select -> do
          let deleteSelection unit = do
                let Expr.Zipperp path selection dterm = select.dzipperp
                let sort = derivTermSort dterm
                let selection'' = case selection of
                      Left p -> Expr.reversePath p -- Expr.toUpPath p
                      Right p -> p -- Expr.toUpPath p
                let {downChange, upChange, cursorSort: _} =
                      spec.splitChange
                        (SmallStep.getPathChange2 selection'' spec.forgetSorts)
                let ssterm = setupSSTermFromWrapAction false
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
            deleteSelection unit
          else if key == "Escape" then do
            -- SelectState --> CursorState
--            setBufferEnabled false Nothing
            setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.unzipperp select.dzipperp)))
          else if key == "Backspace" then deleteSelection unit
          else if isOpenBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
            setFacade $ CursorState cursor
            -- activate buffer
            setBufferEnabled true Nothing
          else if (isQueryKey <$> keyCodePoint) == Just true then do
            -- assert: key is a single alpha char
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            -- SelectState --> CursorState
            let cursor = cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.unzipperp select.dzipperp))
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
          if isOpenBufferKey key then do
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper mempty top.dterm)))
          else if isJust (readMoveDir key) then
            setFacade $ CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper (Expr.Zipper mempty top.dterm)))
          else pure unit
        ------------------------------------------------------------------------
        -- SmallStepState
        ------------------------------------------------------------------------
        SmallStepState ss -> do
          if key == " " then do
            case SmallStep.step Nothing ss.ssterm spec.stepRules of
              Nothing -> finalizeSmallstep ss.ssterm
              Just ssterm -> setState $ SmallStepState ss {ssterm = ssterm}
          else if key == "Enter" then do
            let final = SmallStep.stepRepeatedly Nothing ss.ssterm spec.stepRules
            finalizeSmallstep final
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
      if isValidCursor spec.isValidCursorSort hdzipper
          then do
            H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
            setFacade $ CursorState (cursorFromHoleyDerivZipper hdzipper)
          else pure unit
    
    -- !TODO when making a selection, should i check that sorts match? Or should
    -- I delay that check to when you try to do an action e.g. delete/cut
    onMouseOver hdzipper event = do
      let stopprop = H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
      let checkValidity dzipperp =
            let topSort /\ botSort = derivZipperpSorts dzipperp in
            if isValidSelect spec dzipperp then do
                stopprop
                setFacade (SelectState {dzipperp})
                else pure unit
--             was here, need to use and only set state and propagate if valid
      let dzipper = (hdzipperDerivZipper hdzipper)
      if MouseEvent.buttons event Bits..&. 1 /= 0 then do
        getFacade >>= case _ of
          CursorState cursor -> do
            case Expr.zipperpFromTo (hdzipperDerivZipper cursor.hdzipper) dzipper of
              Nothing -> pure unit
              Just dzipperp -> checkValidity dzipperp -- setFacade (SelectState {dzipperp})
          SelectState select -> do
            let from = (Expr.unzipperp select.dzipperp)
--            traceM ("in mouse selection stuff. from is: " <> pretty from <> "i and to is: " <> pretty dzipper)
--            traceM ("and the output from zipperpFormTo is: " <> pretty (Expr.zipperpFromTo from dzipper))
            case Expr.zipperpFromTo from dzipper of
                -- selected back to original node
--              Nothing -> trace ("select Nothing out") \_ -> do
--                    stopprop
--                    setFacade (CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper dzipper)))
              Nothing -> pure unit
              Just dzipperp ->
                case (normalizeZipperp dzipperp) of
                    Right dzipperp' -> checkValidity dzipperp'
                    Left zipperp -> do
                        stopprop
                        setFacade (CursorState (cursorFromHoleyDerivZipper (injectHoleyDerivZipper zipperp)))
          TopState _top -> pure unit
          SmallStepState _ -> pure unit
        setHighlightElement Nothing
      else do
        if isValidCursor spec.isValidCursorSort hdzipper then do
            stopprop
            setHighlightElement (Just (hdzipperHoleyDerivPath hdzipper))
            else pure unit

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
  -- Query
  ------------------------------------------------------------------------------

  HK.useQuery tokens.queryToken case _ of
    SetProgram newProg toBeMarked return -> do
        setState $ TopState {dterm: newProg}
        _ <- sequence (toBeMarked <#> \path -> setNodeElementStyle "marked" Nothing (Just path))
        pure (Just return)
    GetProgram return -> do
        st <- getState
        pure (Just (return (stateToDerivTerm st)))

  ------------------------------------------------------------------------------
  -- render
  ------------------------------------------------------------------------------

  HK.pure $
--    log "editorComponent.render" (P.bullets
--      [ "currentState = " <> pretty currentState ]
--    ) \_ ->
    HH.div [classNames ["editor"]]
    [ -- HH.div
--      [ classNames ["status"] ]
--      [ HH.table_
--        [ HH.tr_ [HH.td_ [HH.text "mode"], HH.td_ [HH.text case currentState of
--            CursorState _ -> "cursor"
--            SelectState _ -> "cursor"
--            TopState _ -> "cursor"
--            SmallStepState _ -> "smallstep"] ]
--        ]
--      ],
    HH.div
      [ classNames ["program"]
      , HE.onMouseLeave \event -> do
          H.liftEffect $ Event.stopPropagation $ MouseEvent.toEvent event
          setHighlightElement Nothing
      ]
      case currentState of
        CursorState cursor -> do
          let dzipper = hdzipperDerivZipper cursor.hdzipper
          -- TODO: now that I've refactored HoleyDerivZipper, this code can probably be way simpler.
          case cursor.hdzipper of
            HoleyDerivZipper _ false ->
              [ renderPath locs dzipper 
                    (renderDerivTerm locs true false dzipper)
                  (defaultRenderingContext pathIdPrefix)
              ]
            HoleyDerivZipper _ true ->
              [ renderPath locs dzipper
                    (renderDerivTerm locs false true dzipper)
                  (defaultRenderingContext pathIdPrefix)
              ]
        SelectState _select -> hole "render SelectState"
        TopState top -> [ renderDerivTerm locs false false (Expr.Zipper (Expr.Path Nil) top.dterm) (defaultRenderingContext pathIdPrefix) ]
        SmallStepState ss -> 
          [HH.div
            [ classNames ["smallstep-program"] ]
            [ renderSSTerm locs ss.ssterm 
                (defaultRenderingContext pathIdPrefix)
            ]]
     -- I've commented this out because I have no use for the console currently
--    , HH.slot_ _consoleSlot unit consoleComponent unit
    ]

