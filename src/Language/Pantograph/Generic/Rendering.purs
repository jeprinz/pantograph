-- !TODO there are some more places I need to tell the current buffer to disable e.g. when setting the facade

module Language.Pantograph.Generic.Rendering where

import Hole
import Language.Pantograph.Generic.Grammar
import Prelude
import Type.Direction

import Bug (bug)
import Bug.Assertion (Assertion(..), assert, assertM, just)
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Expr (class ReflectPathDir, wellformedExpr, (%))
import Data.Expr as Expr
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Generic.Rep (class Generic)
import Data.Int.Bits as Bits
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List.Zip as ZipList
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Rational as Rational
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (case_, default, inj, on)
import Debug as Debug
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML (ComponentHTML, HTML(..), div, input, slot, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Query.Event as HQ
import Halogen.Utilities (classNames, fromInputEventToTargetValue, setClassName)
import Language.Pantograph.Generic.Edit (Action(..), Edit, EditPreview(..), defaultEditsAtDerivZipper, defaultEditsAtHoleInterior)
import Language.Pantograph.Generic.ZipperMovement (moveZipper, moveZipperp)
import Log (logM)
import Log as Log
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as InputElement
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

isBufferKey :: String -> Boolean
isBufferKey = (_ `Array.elem` [" ", "Enter"])

data Output l r
  = ActionOutput (Action l r)
  | UpdateFacadeOutput (State l r -> HK.HookM Aff (State l r))

data Query a
  -- = KeyboardEvent KeyboardEvent.KeyboardEvent a
  = SetBufferEnabledQuery Boolean (Maybe String) a
  | MoveBufferQuery VerticalDir a
  | SubmitBufferQuery a

data State l r
  = CursorState (Cursor l r)
  | SelectState (Select l r)
  | TopState (Top l r)

derive instance Generic (State l r) _
instance (Show l, Show r) => Show (State l r) where show x = genericShow x

instance (Expr.IsExprLabel l, IsRuleLabel l r) => Pretty (State l r) where
  pretty = case _ of
    CursorState cursor -> Array.intercalate "\n"
      [ "cursor:"
      , P.indent $ P.newlines
          [ "- hdzipper = " <> pretty cursor.hdzipper
          , "- mode = " <> show cursor.mode
          ]
      ]
    SelectState select -> Array.intercalate "\n"
      [ "select:"
      , P.indent $ P.newlines
          [ "- dzipperp = " <> pretty select.dzipperp ]
      ]
    TopState _top -> Array.intercalate "\n"
      [ "top:"
      ]

-- !TODO is it worth having a different type for frontend that has Buffer where State doesn't have Buffer?
-- data Facade l
--   = BufferFacade (Buffer l r)
--   | CursorFacade (Cursor l r)
--   | SelectFacade (Select l r)
--   | TopFacade (Top l r)

-- type Buffer l r =
--   { dzipper :: DerivZipper l r
--   }

type Cursor l r =
  { hdzipper :: HoleyDerivZipper l r
  , mode :: CursorMode
  }

data CursorMode 
  = NavigationCursorMode
  | StringCursorMode String
  | BufferCursorMode

derive instance Generic CursorMode _
instance Show CursorMode where show x = genericShow x

cursorFromHoleyDerivZipper :: forall l r. HoleyDerivZipper l r -> Cursor l r
cursorFromHoleyDerivZipper hdzipper = 
  { hdzipper
  , mode: NavigationCursorMode
  }

type Select l r =
  { dzipperp :: DerivZipperp l r
  }

type Top l r =
  { dterm :: DerivTerm l r
  }

data HoleyDerivZipper l r
  = InjectHoleyDerivZipper (DerivZipper l r)
  | HoleInteriorHoleyDerivZipper 
      (DerivPath Up l r) -- the path to the Hole
      (Sort l) -- the sort of the Hole

derive instance Generic (HoleyDerivZipper l r) _
derive instance (Eq l, Eq r) => Eq (HoleyDerivZipper l r)
derive instance (Ord l, Ord r) => Ord (HoleyDerivZipper l r)
instance (Show l, Show r) => Show (HoleyDerivZipper l r) where show x = genericShow x

instance IsRuleLabel l r => Pretty (HoleyDerivZipper l r) where
  pretty (InjectHoleyDerivZipper dzipper) = pretty dzipper
  pretty (HoleInteriorHoleyDerivZipper dpath sort) = Expr.prettyPath dpath $ "(⌶{?} : " <> pretty sort <> ")"

data HoleyDerivPath dir l r
  = InjectHoleyDerivPath (DerivPath dir l r)
  | HoleInteriorHoleyDerivPath (DerivPath Up l r) -- the path to the Hole

derive instance Generic (HoleyDerivPath dir l r) _
derive instance (Eq l, Eq r) => Eq (HoleyDerivPath dir l r)
derive instance (Ord l, Ord r) => Ord (HoleyDerivPath dir l r)
instance (Show l, Show r) => Show (HoleyDerivPath dir l r) where show x = genericShow x

instance (IsRuleLabel l r, ReflectPathDir dir) => Pretty (HoleyDerivPath dir l r) where
  pretty (InjectHoleyDerivPath dpath) = pretty dpath
  pretty (HoleInteriorHoleyDerivPath dpath) = Expr.prettyPath dpath $ "(⌶{?} : _)"

hdzipperDerivPath :: forall l r. HoleyDerivZipper l r -> DerivPath Up l r
hdzipperDerivPath (InjectHoleyDerivZipper dzipper) = Expr.zipperPath dzipper
hdzipperDerivPath (HoleInteriorHoleyDerivZipper dpath _) = dpath

hdzipperHoleyDerivPath :: forall l r. HoleyDerivZipper l r -> HoleyDerivPath Up l r
hdzipperHoleyDerivPath (InjectHoleyDerivZipper dzipper) = InjectHoleyDerivPath (Expr.zipperPath dzipper)
hdzipperHoleyDerivPath (HoleInteriorHoleyDerivZipper dpath _) = HoleInteriorHoleyDerivPath dpath

hdzipperDerivTerm :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivTerm l r
hdzipperDerivTerm (InjectHoleyDerivZipper dzipper) = Expr.zipperExpr dzipper
hdzipperDerivTerm (HoleInteriorHoleyDerivZipper dpath sort) = assert (just "hdzipperDerivTerm" (defaultDerivTerm sort)) \dterm ->
  Expr.unzipper $ Expr.Zipper dpath dterm

hdzipperZipper :: forall l r. IsRuleLabel l r => HoleyDerivZipper l r -> DerivZipper l r
hdzipperZipper hdzipper = do
  let path = hdzipperDerivPath hdzipper
  let dterm = hdzipperDerivTerm hdzipper
  Expr.Zipper path dterm

escapeHoleInterior :: forall l r. IsRuleLabel l r => Cursor l r -> Cursor l r
escapeHoleInterior cursor = do
  let path = hdzipperDerivPath cursor.hdzipper
  let dterm = hdzipperDerivTerm cursor.hdzipper
  cursorFromHoleyDerivZipper (InjectHoleyDerivZipper (Expr.Zipper path dterm))

-- cursorState source msg st = Assertion
--   { name: "cursorState"
--   , source
--   , result: case st of
--       CursorState cursor -> pure cursor
--       st -> throwError $ 
--         "Expected state to be `CursorState` because " <> msg <> "\n" <> 
--           pretty st
--   }

defaultEditsAtHoleyDerivZipper :: forall l r. IsRuleLabel l r => Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
defaultEditsAtHoleyDerivZipper topSort = case _ of
  InjectHoleyDerivZipper dz -> defaultEditsAtDerivZipper topSort dz
  HoleInteriorHoleyDerivZipper p sort -> defaultEditsAtHoleInterior p sort

type EditorSpec l r =
  { hdzipper :: HoleyDerivZipper l r
  , topSort :: Sort l
  , editsAtHoleyDerivZipper :: Sort l -> HoleyDerivZipper l r -> Array (Edit l r)
  , renderDerivTermKids' ::
      (r /\ Sort l /\ Array (DerivTerm l r)) ->
      Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query) (Output l r) String) Aff) -> 
      Array String /\ Array (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query) (Output l r) String) Aff)
    
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
      -- { hdzipper: InjectHoleyDerivZipper input.dzipper
      (cursorFromHoleyDerivZipper input.hdzipper)

  -- state
  currentState /\ state_id <- HK.useState $ initState
  -- let _ = Debug.trace ("[editorComponent] currentState: " <> pretty currentState) \_ -> unit

  -- facade state
  _ /\ facade_ref <- HK.useRef $ initState

  -- clipboard
  _ /\ clipboard_ref <- HK.useRef (Nothing :: Maybe (DerivPath Up l r \/ DerivTerm l r))

  -- !TODO OLD
  -- -- mapping: dpath ==> html elem id
  -- _ /\ pathElementIds_ref <- HK.useRef (Map.empty :: Map (DerivPath Up l r) String)

  -- highlight path
  _ /\ maybeHighlightPath_ref <- HK.useRef Nothing

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

    -- getElementIdByDerivPath :: DerivPath Up l r -> HK.HookM Aff String
    -- getElementIdByDerivPath = pure <<< fromPathToElementId

    -- getElementByPath :: DerivPath Up l r -> HK.HookM Aff DOM.Element
    -- getElementByPath derivPath = do
    --   doc <- liftEffect $ HTML.window >>= Window.document
    --   elemId <- getElementIdByDerivPath derivPath
    --   liftEffect (NonElementParentNode.getElementById elemId (HTMLDocument.toNonElementParentNode doc)) >>= case _ of
    --     Nothing -> bug $ "could not find element by id: " <> elemId
    --     Just elem -> pure elem

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

    moveCursor dir = do
      -- Debug.traceM $ "[moveCursor] dir = " <> show dir
      getFacade >>= case _ of
        CursorState cursor@{mode: BufferCursorMode}  -> pure unit
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
        ------------------------------------------------------------------------
        -- CursorState where mode = StringCursorMode
        ------------------------------------------------------------------------
        CursorState cursor@{mode: StringCursorMode str} -> do
          if isBufferKey key then do
            -- exit StringCursorMode
            liftEffect $ Event.preventDefault $ KeyboardEvent.toEvent event
            setState $ CursorState cursor {mode = NavigationCursorMode}
          else if (Unicode.isAlpha <$> keyCodePoint) == Just true then do
            -- !TODO modify string
            pure unit
          else
            pure unit
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
                Debug.traceM "entering StringCursorMode"
                setFacade $ CursorState (cursor {mode = StringCursorMode str})
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
  
    renderDerivTermKids (Expr.Zipper dpath dterm) kidElems = Debug.trace ("[renderDerivTermKids] dterm = " <> pretty dterm) \_ -> assert (wellformedExpr "renderDerivTermKids" dterm) \_ -> case dterm of
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

    renderPath dzipper1 interior  = do
      case Expr.zipUp dzipper1 of
        Nothing -> interior
        Just (th /\ dzipper2) -> do
          let
            elemId = fromPathToElementId (Expr.zipperPath dzipper2)
            -- _ = Debug.trace ("[renderPath] Expr.zipDownsTooth dzipper2 th = " <> show (pretty <$> Expr.zipDownsTooth dzipper2 th)) \_ -> unit
            clsNames /\ kidElems = 
              -- renderDerivTermKids (Expr.unTooth th (Expr.zipperExpr dzipper1)) $
              renderDerivTermKids (Expr.Zipper (Expr.zipperPath dzipper2) (Expr.unTooth th (Expr.zipperExpr dzipper1))) $
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

    renderEditableString dzipper str = do
      let elemId = fromPathToElementId (Expr.zipperPath dzipper)
      let clsNames /\ _ = renderDerivTermKids dzipper $ renderPreviewDerivZipper <<< snd <$> Expr.zipDowns dzipper
      HH.div 
        [ classNames ["node", "editable-string"]
        , HP.id elemId
        ] $
        [ HH.input
          [ HP.autofocus true
          , HP.type_ HP.InputText
          , HE.onInput \_event -> do
              Debug.traceM "[string.onInput]"
          , HP.value str
          ]
        ]
          -- [ [ HH.slot bufferSlot elemId bufferComponent 
          --     { hdzipper: InjectHoleyDerivZipper dzipper
          --     , edits: input.editsAtHoleyDerivZipper input.topSort (InjectHoleyDerivZipper dzipper) <#>
          --       \edit -> renderEditPreview edit.preview /\ edit
          --     } 
          --     handleBufferOutput
          --   ]
          -- , [ HH.div [classNames ["subnode", "inner"]]
          --       [ HH.div [classNames ["subnode", "editable-string"]] [HH.text (pretty (Expr.zipperExpr dzipper))] 
          --       ]
          --   ]
          -- ]

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
              CursorState cursor@{mode: StringCursorMode str} -> do
                let dzipper = hdzipperZipper cursor.hdzipper
                [renderPath dzipper $ renderEditableString dzipper str]
              CursorState cursor -> do
                let dzipper = hdzipperZipper cursor.hdzipper
                case cursor.hdzipper of
                  InjectHoleyDerivZipper _ -> [renderPath dzipper $ renderExpr true dzipper]
                  HoleInteriorHoleyDerivZipper dpath sort -> [renderPath dzipper $ renderHoleInterior true dpath sort]
              SelectState _st -> hole "render SelectState"
              TopState _st -> hole "render TopState"
          ]

bufferSlot = Proxy :: Proxy "buffer"

type BufferInput l r =
  { hdzipper :: HoleyDerivZipper l r
  , edits :: Array (Lazy (HH.ComponentHTML (HK.HookM Aff Unit) (buffer :: H.Slot (Query) (Output l r) String) Aff) /\ Edit l r)
  }

bufferComponent :: forall l r. IsRuleLabel l r => H.Component (Query) (BufferInput l r) (Output l r) Aff
bufferComponent = HK.component \tokens input -> HK.do
  isEnabled /\ isEnabled_id <- HK.useState false
  bufferString /\ bufferString_id <- HK.useState ""
  bufferFocus /\ bufferFocus_id <- HK.useState 0
  -- !TODO bufferFocus is actually 2D, since eventually I'll implement cycling
  -- between different edits that have the same label

  let bufferInputRefLabelString = "buffer-input"

  edits <- HK.captures {hdzipper: input.hdzipper, bufferString} $ flip HK.useMemo \_ ->
    input.edits #
      -- memo fuzzy distances
      map (map (\edit -> Fuzzy.matchStr false bufferString edit.label /\ edit)) >>>
      -- filter out edits that are below a certain fuzzy distance from the edit ExprLabel
      Array.filter (\(_ /\ (FuzzyStr fs /\ _)) -> Rational.fromInt 0 < fs.ratio) >>>
      -- sort the remaining edits by the fuzzy distance
      Array.sortBy (\(_ /\ (fuzzyStr1 /\ _)) (_ /\ (fuzzyStr2 /\ _)) -> compare fuzzyStr1 fuzzyStr2) >>>
      -- forget fuzzy distances
      map (map snd)

  let normalBufferFocus = bufferFocus `mod` Array.length edits

  let submitBuffer _ = do
        if isEnabled then do
          case Array.index edits normalBufferFocus of
            Nothing -> bug $ "[bufferComponent.SubmitBufferQuery] attempted to submit buffer, but bufferFocus is out of range: \n  - length edits = " <> show (Array.length edits) <> "\n  - bufferFocus = " <> show bufferFocus 
            Just (_ /\ edit) -> do
              HK.put isEnabled_id false -- disable query
              HK.put bufferFocus_id 0 -- reset bufferFocus
              HK.raise tokens.outputToken $ ActionOutput edit.action -- output edit action
              pure true
        else
          pure false

  HK.useQuery tokens.queryToken case _ of
    SetBufferEnabledQuery isEnabled' mb_str a -> do
      HK.put isEnabled_id isEnabled' -- update isEnabled
      HK.put bufferFocus_id 0 -- reset bufferFocus
      if isEnabled' then do
          -- focus buffer input tag
          HK.getHTMLElementRef (H.RefLabel bufferInputRefLabelString) >>= case _ of 
            Nothing -> bug $ "[bufferComponent.useQuery] could not find element with ref ExprLabel: " <> bufferInputRefLabelString
            Just elem -> do
              liftEffect $ HTMLElement.focus elem
              case mb_str of
                Nothing -> pure unit
                Just str -> do
                  -- initialize string in buffer
                  case InputElement.fromElement (HTMLElement.toElement elem) of
                    Nothing -> bug "The element referenced by `bufferInputRefLabelString` wasn't an HTML input element."
                    Just inputElem -> liftEffect $ InputElement.setValue str inputElem
          -- update facade to BufferCursorMode
          HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
            pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper) {mode = BufferCursorMode}
          pure unit
      else
        -- update facade to CursorState
        HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
          pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper)
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
      submitBuffer unit >>= case _ of
        false -> pure Nothing
        true -> pure $ Just a

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
                flip Array.mapWithIndex edits \i (lazy_editHtml /\ edit) -> 
                  HH.div 
                    [ classNames $ ["buffer-result"] <> if i == normalBufferFocus then ["buffer-focus"] else []
                    , HE.onMouseOver \event -> do
                        liftEffect $ Event.preventDefault $ MouseEvent.toEvent event
                        HK.put bufferFocus_id i
                    , HE.onMouseDown \event -> do
                        liftEffect $ Event.preventDefault $ MouseEvent.toEvent event
                        -- HK.put bufferFocus_id i
                        void $ submitBuffer unit
                    ]
                    [force lazy_editHtml]
              ]
          ]
        ]

--
-- | Rendering utilities
--

_verbose_path_element_ids :: Boolean
_verbose_path_element_ids = true

fromHoleyDerivPathToElementId :: forall l r. IsRuleLabel l r => HoleyDerivPath Up l r -> String
fromHoleyDerivPathToElementId
  | _verbose_path_element_ids = case _ of
      InjectHoleyDerivPath dpath -> fromPathToElementId dpath
      HoleInteriorHoleyDerivPath dpath -> dpath # Expr.foldMapPath "holeInterior-PathNil" \(Expr.Tooth l kidsZip) str -> String.replaceAll (String.Pattern " ") (String.Replacement "_") (pretty l <> "@" <> show (ZipList.leftLength kidsZip)) <> "-" <> str
  | otherwise = case _ of
      InjectHoleyDerivPath dpath -> fromPathToElementId dpath
      HoleInteriorHoleyDerivPath dpath -> dpath # Expr.foldMapPath "holeInterior-PathNil" \(Expr.Tooth l kidsZip) str -> show (ZipList.leftLength kidsZip) <> "-" <> str

fromPathToElementId :: forall l. Expr.IsExprLabel l => Expr.Path Up l -> String
fromPathToElementId 
  | _verbose_path_element_ids = Expr.foldMapPath "PathNil" \(Expr.Tooth l kidsZip) str -> String.replaceAll (String.Pattern " ") (String.Replacement "_") (pretty l <> "@" <> show (ZipList.leftLength kidsZip)) <> "-" <> str
  | otherwise = Expr.foldMapPath "PathNil" \(Expr.Tooth _ kidsZip) str -> show (ZipList.leftLength kidsZip) <> "-" <> str

{-
unsafePerformEffect do
    -- generate new elemId and add it to the map
    elemId_ <- UUID.toString <$> UUID.genUUID
    Ref.modify_ (Map.insert (Expr.zipperPath dzipper) elemId_) pathElementIds_ref
    pure elemId_
-}

makePuncElem :: forall w i. String -> String -> HH.HTML w i
makePuncElem className symbol = HH.div [classNames ["subnode", "punctuation", className]] [HH.text symbol]

spaceElem = makePuncElem "space" " "
lparenElem = makePuncElem "lparen" "("
rparenElem = makePuncElem "rparen" ")"
colonElem = makePuncElem "colon" ":"
turnstileElem = makePuncElem "turnstile" "⊢"
interrogativeElem = makePuncElem "interrogative" "?"

ibeamElem = makePuncElem "ibeam" "⌶"

placeholderCursorNodeElem =
  HH.div [classNames ["node", "placeholder-cursor"]]
    [ HH.div [classNames ["subnode", "inner"]]
        -- [ibeamElem]
        [spaceElem]
    ]