module Language.Pantograph.Generic.Rendering.Preview where

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
import Halogen.HTML (div, input, span, text) as HH
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

previewComponent :: forall l r out. H.Component (PreviewQuery l r) Unit out Aff
previewComponent = HK.component \tokens _ -> HK.do
  elems /\ elems_id <- HK.useState []
  let isEnabled = not $ Array.null elems

  HK.useQuery tokens.queryToken case _ of
    SetPreviewQuery elems' a -> do
      HK.put elems_id elems'
      pure $ Just a

  HK.pure do
    HH.span [classNames $ ["preview"] <> if isEnabled then ["enabled"] else []] do
      elems
