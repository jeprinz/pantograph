module Language.Pantograph.Generic.Rendering.Preview where

import Language.Pantograph.Generic.Edit
import Language.Pantograph.Generic.Rendering.Base
import Prelude

import Bug (bug)
import Control.Plus (empty)
import Data.Array as Array
import Data.Expr ((%))
import Data.Expr as Expr
import Data.Lazy (defer, force)
import Data.Maybe (Maybe(..))
import Data.Rational as Rational
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, on)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML (div, input, span, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities (classNames, fromInputEventToTargetValue)
import Language.Pantograph.Generic.Grammar (class IsRuleLabel, DerivLabel(..), derivTermSort)
import Language.Pantograph.Generic.Rendering.Elements (placeholderCursorNodeElem)
import Type.Direction (HorizontalDir, _down, _left, _right, _up)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.MouseEvent as MouseEvent
import Debug (traceM)

previewComponent :: forall l r out. H.Component (PreviewQuery l r) HorizontalDir out Aff
previewComponent = HK.component \tokens dir -> HK.do
  preview /\ preview_id <- HK.useState $ empty

  HK.useQuery tokens.queryToken case _ of
    SetPreviewQuery preview' a -> do
      HK.put preview_id preview'
      pure $ Just a

  HK.pure do
    case preview of
      Nothing -> HH.span [classNames ["preview", "empty"]] []
      Just (ReplaceEditPreview elem) -> case_
          # on _left (\_ -> HH.span [classNames ["preview", "replace"]] [elem])
          # on _right (\_ -> HH.span [classNames ["preview", "replace"]] [])
          $ dir
      Just (FillEditPreview elem) -> case_
          # on _left (\_ -> HH.span [classNames ["preview", "fill"]] [elem])
          # on _right (\_ -> HH.span [classNames ["preview", "empty"]] [])
          $ dir
      Just (WrapEditPreview {before, after}) -> case_
          # on _left (\_ -> HH.span [classNames ["preview", "wrap", "before"]] before)
          # on _right (\_ -> HH.span [classNames ["preview", "wrap", "after"]] after)
          $ dir
