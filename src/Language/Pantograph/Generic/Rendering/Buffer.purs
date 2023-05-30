module Language.Pantograph.Generic.Rendering.Buffer where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Rendering.Base
import Prelude

import Bug (bug)
import Data.Array as Array
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Fuzzy (FuzzyStr(..))
import Data.Fuzzy as Fuzzy
import Data.Lazy (defer, force)
import Data.Maybe (Maybe(..))
import Data.Rational as Rational
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML (div, input, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (classNames, fromInputEventToTargetValue)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, DerivLabel(..), derivTermSort)
import Language.Pantograph.Generic.Rendering.Elements (placeholderCursorNodeElem)
import Type.Direction (_down, _up)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.MouseEvent as MouseEvent

bufferComponent :: forall l r. IsRuleLabel l r => H.Component Query (BufferInput l r) (Output l r) Aff
bufferComponent = HK.component \tokens input -> HK.do
  isEnabled /\ isEnabled_id <- HK.useState false
  bufferString /\ bufferString_id <- HK.useState ""
  bufferFocus /\ bufferFocus_id <- HK.useState 0
  -- !TODO bufferFocus is actually 2D, since eventually I'll implement cycling
  -- between different edits that have the same label

  let is_string = case input.hdzipper of
        InjectHoleyDerivZipper (Expr.Zipper _ (DerivString str % _)) -> true
        _ -> false

  let bufferInputRefLabelString = "buffer-input"

  edits <- HK.captures {hdzipper: input.hdzipper, bufferString} $ flip HK.useMemo \_ ->
    if is_string then do
      let dterm = DerivString bufferString % []
      let sort = derivTermSort dterm
      -- let preview = DerivTermEditPreview dterm
      [ defer (\_ -> FillEditPreview (HH.text bufferString)) /\
        { label: bufferString
        -- , action: SetCursorAction (defer \_ -> 
        --     Expr.Zipper (hdzipperDerivPath input.hdzipper) (DerivString bufferString % []))
        , action: defer \_ -> ReplaceAction
            -- !TODO compute actual change
            { topChange: Expr.injectExprChange sort
            , dterm
            }
        } ]
    else
      input.edits #
        -- memo fuzzy distances
        map (map (\edit -> Fuzzy.matchStr false bufferString edit.label /\ edit)) >>>
        -- filter out edits that are below a certain fuzzy distance from the edit ExprLabel
        Array.filter (\(_ /\ (FuzzyStr fs /\ _)) -> Rational.fromInt 0 < fs.ratio) >>>
        -- sort the remaining edits by the fuzzy distance
        Array.sortBy (\(_ /\ (fuzzyStr /\ _)) (_ /\ (fuzzyStr2 /\ _)) -> compare fuzzyStr fuzzyStr2) >>>
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
              HK.raise tokens.outputToken $ ActionOutput (force edit.action) -- output edit action
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
                    Just inputElem -> do
                      liftEffect $ InputElement.setValue str inputElem
                      HK.put bufferString_id str
          -- update facade to BufferCursorMode
          HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
            pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper) {mode = BufferCursorMode}
          pure unit
      else
        -- update facade to CursorState
        HK.raise tokens.outputToken $ UpdateFacadeOutput \_ ->
          pure $ CursorState (cursorFromHoleyDerivZipper input.hdzipper)
      pure (Just a)
    -- SetBufferStringQuery str a -> do
    --   Debug.traceM $ "SetBufferString str = " <> str
    --   HK.put bufferString_id str
    --   pure $ Just a
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
          [ HH.div [classNames ["inner"]] $
              Array.concat
              [ [ HH.input 
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
                ]
              , if is_string then [] else pure $
                HH.div
                  [ classNames ["buffer-results"] ] $
                  flip Array.mapWithIndex edits \i (lazy_editPreviewHtml /\ _edit) -> 
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
                      (case force lazy_editPreviewHtml of
                        FillEditPreview html -> [html]
                        WrapEditPreview {before, after} -> before <> [placeholderCursorNodeElem] <> after
                        ReplaceEditPreview html -> [html]
                      )
                ]
          ]
        ]
