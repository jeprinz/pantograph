module Language.Pantograph.Generic.Rendering.Buffer where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Grammar
import Language.Pantograph.Generic.Rendering.Base
import Prelude

import Bug (bug)
import Bug.Assertion (assert, just)
import Control.Alternative (guard)
import Data.Array as Array
import Data.Expr ((%), (%*))
import Data.Expr as Expr
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..))
import Data.Rational as Rational
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (case_, on)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML (div, input, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (classNames, fromInputEventToTargetValue)
import Hole (hole)
import Language.Pantograph.Generic.Rendering.Elements (placeholderCursorNodeElem)
import Log (log)
import Text.Pretty (bullets, pretty)
import Type.Direction (_down, _up)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.MouseEvent as MouseEvent

type BufferPreState l r =
  { isEnabled :: Boolean
  , bufferString :: String
  , bufferFocus :: Int
  }

type BufferState l r =
  { isEnabled :: Boolean
  , bufferString :: String
  , bufferFocus :: Int
  , normalBufferFocus :: Int
  , edits :: Array (EditAndPreview l r)
  , focussedEdit :: Maybe (EditAndPreview l r)
  , mb_oldString :: Maybe String
  }

computeEdits :: forall l r. IsRuleLabel l r => BufferInput l r -> _ -> Array (EditAndPreview l r)
computeEdits input {bufferString, mb_oldString} = 
  case mb_oldString of
    Just oldString -> do
      [ { edit: 
            { label: bufferString
            , action: defer \_ -> ReplaceAction
                -- !TODO compute actual change
                -- OLD: Expr.injectExprChange sort
                { topChange: 
                    Expr.injectChange (pure NameSortLabel) 
                      [Expr.replaceChange 
                        (StringSortLabel oldString %* [])
                        (StringSortLabel bufferString %* [])]
                , dterm: DerivString bufferString % []
                }
            }
        , lazy_preview: defer (\_ -> FillEditPreview (HH.text bufferString))
        } ]
    Nothing ->
      input.edits #
        -- memo fuzzy distances
        map (\item@{edit} -> Fuzzy.matchStr false bufferString edit.label /\ item) >>>
        -- filter out edits that are below a certain fuzzy distance from the edit ExprLabel
        Array.filter (\(FuzzyStr fs /\ _) -> Rational.fromInt 0 < fs.ratio) >>>
        -- sort the remaining edits by the fuzzy distance
        Array.sortBy (\(fuzzyStr1 /\ _) (fuzzyStr2 /\ _) -> compare fuzzyStr1 fuzzyStr2) >>>
        -- forget fuzzy distances
        map snd

computeNormalBufferFocus {bufferFocus, edits} = 
  bufferFocus `mod` Array.length edits

computeFocussedEdit {isEnabled, normalBufferFocus, edits} = do
  if isEnabled
    then Array.index edits normalBufferFocus
    else Nothing

computeBufferState :: forall l r. IsRuleLabel l r => BufferInput l r -> BufferPreState l r -> BufferState l r
computeBufferState input preSt@{isEnabled, bufferString, bufferFocus} = do
  let mb_oldString = case input.hdzipper of
        HoleyDerivZipper (Expr.Zipper _ (DerivString str % _)) false -> Just str
        _ -> Nothing
  let edits = computeEdits input {bufferString, mb_oldString}
  let normalBufferFocus = computeNormalBufferFocus {bufferFocus, edits}
  let focussedEdit = computeFocussedEdit {isEnabled, normalBufferFocus, edits}
  { isEnabled
  , bufferString
  , bufferFocus
  , normalBufferFocus
  , edits 
  , focussedEdit
  , mb_oldString
  }

extractBufferPreState :: forall l r. IsRuleLabel l r => BufferState l r -> BufferPreState l r
extractBufferPreState 
  { isEnabled
  , bufferString
  , bufferFocus
  , normalBufferFocus
  , edits 
  , focussedEdit
  , mb_oldString
  } = 
  { isEnabled
  , bufferString
  , bufferFocus
  }


bufferInputRefLabelString = "buffer-input" :: String
bufferInputRefLabel = H.RefLabel bufferInputRefLabelString :: H.RefLabel

bufferComponent :: forall l r. IsRuleLabel l r => H.Component Query (BufferInput l r) (Output l r) Aff
bufferComponent = HK.component \tokens input -> HK.do
  
-- !TODO bufferFocus is actually 2D, since eventually I'll implement cycling
-- between different edits that have the same label
  currentBufferState /\ bufferState_id <- HK.useState do
    let 
      isEnabled = false
      bufferString = ""
      bufferFocus = 0
    computeBufferState 
      input
      { isEnabled
      , bufferString
      , bufferFocus }

  let 
    -- setPreview Nothing = HK.raise tokens.outputToken $ SetPreviewOutput mempty
    -- setPreview (Just lazy_editPreviewHtml) = HK.raise tokens.outputToken $ SetPreviewOutput (force lazy_editPreviewHtml)
    setPreview mb_preview = HK.raise tokens.outputToken $ SetPreviewOutput mb_preview

  let 
    get = HK.get bufferState_id

    -- handles computing functionally-dependent parts of bufferState and updating preview
    put _st = do
      let st = computeBufferState input _st
      HK.put bufferState_id st
      -- update preview
      case st.focussedEdit of
        Nothing -> setPreview Nothing
        Just {lazy_preview} -> setPreview (Just (force lazy_preview))
      pure st

    modify :: (BufferPreState l r -> BufferPreState l r) -> HK.HookM Aff (BufferState l r)
    modify f = do
      st <- extractBufferPreState <$> get
      let st' = f st
      put st'

  -- | Yields whether or not submission was successful
  let submitBuffer _ = do
        st <- get
        case st.isEnabled /\ st.focussedEdit of
          true /\ Just {edit} -> do
            void $ modify _
              { isEnabled = false -- disable buffer
              , bufferFocus = 0 -- reset bufferFocus
              }
            setPreview Nothing
            -- output edit action
            HK.raise tokens.outputToken $ ActionOutput (force edit.action)
            pure true
          _ -> pure false

  let modifyBufferFocus f = do
        st <- modify \st -> st {bufferFocus = f st.bufferFocus}
        setPreview (force <$> (st.focussedEdit <#> _.lazy_preview))

  HK.useQuery tokens.queryToken case _ of
    SetBufferEnabledQuery isEnabled' mb_str a -> do
      -- update isEnabled
      -- !TODO this will recompute edits right away, which is inefficient since
      -- we are going to compute them again right away if isEnabled' is true
      void $ modify _ {isEnabled = isEnabled'}
      
      if isEnabled' then do
          -- focus buffer input tag
          HK.getHTMLElementRef bufferInputRefLabel >>= \mb_elem -> 
            assert (just ("could not find element with ref ExprLabel: " <> bufferInputRefLabelString) $ mb_elem) \elem -> do
              liftEffect $ HTMLElement.focus elem
              case mb_str of
                Nothing -> pure unit
                Just str ->
                  assert (just ("could not find element with ref ExprLabel: " <> bufferInputRefLabelString) $ 
                    InputElement.fromElement (HTMLElement.toElement elem)) \inputElem -> do
                      -- initialize string in buffer
                      liftEffect $ InputElement.setValue str inputElem
                      void $ modify _
                        { bufferString = str
                        , bufferFocus = 0 -- reset bufferFocus
                        }
          -- -- update facade to BufferCursorMode
          -- HK.raise tokens.outputToken $ UpdateStateOutput \_ ->
          --   pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper) {mode = BufferCursorMode}
      
      else do
        -- clear preview
        setPreview Nothing
        -- -- update facade to CursorState
        -- HK.raise tokens.outputToken $ UpdateStateOutput \_ ->
        --   pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper)
      
      pure (Just a)
    MoveBufferQuery qm a -> do
      st <- get
      if st.isEnabled then do
        (qm # _) $ case_
          # on _up (\_ -> modifyBufferFocus (_ - 1))
          # on _down (\_ -> modifyBufferFocus (_ + 1))
        pure $ Just a
      else
        pure Nothing
    SubmitBufferQuery a -> do
      submitBuffer unit >>= case _ of
        false -> pure Nothing
        true -> pure $ Just a

  HK.pure $
    log "bufferComponent.render"
      (bullets
          [ "input.hzdipper = " <> pretty input.hdzipper
          , "isEnabled = " <> show currentBufferState.isEnabled
          , "bufferString = " <> show currentBufferState.bufferString
          , "bufferFocus = " <> show currentBufferState.bufferFocus
          , "normalBufferFocus = " <> show currentBufferState.normalBufferFocus
          , "mb_oldString = " <> show currentBufferState.mb_oldString
          ])
      \_ ->
      HH.div
        [ classNames ["subnode", "buffer"]
        ] $ 
        Array.concat
        [ if not currentBufferState.isEnabled then [] else
          [ HH.div [classNames ["buffer-inner"]] $
              Array.concat
              [ [ HH.input 
                  [ classNames ["buffer-input"]
                  , HP.autofocus true 
                  , HP.ref $ bufferInputRefLabel
                  , HP.type_ HP.InputText
                  , HE.onInput \event -> do
                      bufferString' <- liftEffect $ fromInputEventToTargetValue event
                      void $ modify _
                        { bufferString = bufferString' -- update bufferString
                        , bufferFocus = 0 -- reset bufferFocus
                        }
                  ]
                ]
              , case currentBufferState.mb_oldString of
                  Just _ -> []
                  Nothing -> pure $
                    HH.div
                      [ classNames ["buffer-results"] ] $
                      flip Array.mapWithIndex currentBufferState.edits \i {lazy_preview} -> 
                        HH.div 
                          [ classNames $ ["buffer-result"] <> if i == currentBufferState.normalBufferFocus then ["buffer-focus"] else []
                          , HE.onMouseOver \event -> do
                              liftEffect $ Event.preventDefault $ MouseEvent.toEvent event
                              void $ modify _ {bufferFocus = i}
                          , HE.onMouseDown \event -> do
                              liftEffect $ Event.preventDefault $ MouseEvent.toEvent event
                              void $ modify _ {bufferFocus = i}
                              void $ submitBuffer unit
                          ]
                          (case force lazy_preview of
                            FillEditPreview html -> [html]
                            WrapEditPreview {before, after} -> before <> [placeholderCursorNodeElem] <> after
                            ReplaceEditPreview html -> [html]
                          )
                ]
          ]
        ]
