module Pantograph.Generic.Rendering.Buffer where

import Data.Either.Nested
import Data.Tree
import Data.Tuple.Nested
import Pantograph.Generic.Language
import Pantograph.Generic.Rendering.Common
import Pantograph.Generic.Rendering.Language
import Prelude
import Util

import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.State (StateT)
import Data.Foldable (foldM, foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.Utilities as HU
import Hole (hole)
import Pantograph.Generic.Language (Sort)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as R
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent

syncExprGyro :: forall el ed sn. ExprGyro el ed sn -> ExprGyro el (SyncExprData ed) sn
syncExprGyro = hole "TODO: syncExprGyro"

hydrateExprGyro :: forall el ed sn. ExprGyro el (SyncExprData ed) sn -> HK.HookM Aff Unit
hydrateExprGyro = hole "TODO: hydrateExprGyro"

renderExprGyro :: forall ctx env el ed sn. ExprGyro el (SyncExprData ed) sn -> RenderM ctx env el ed sn (BufferHtml el ed sn)
renderExprGyro = hole "TODO: renderExprGyro"

-- component
bufferComponent :: forall el ed sn ctx env. Eq el => Eq (Record ed) => Eq (Record (SyncExprData ed)) => Eq sn =>
  H.Component (BufferQuery el ed sn) (BufferInput ctx env el ed sn) (BufferOutput el ed sn) Aff
bufferComponent = HK.component \{queryToken, outputToken} (BufferInput input) -> HK.do
  let Renderer renderer = input.renderer

  renderCtx /\ renderCtxStateId <- HK.useState $
    R.union
      { depth: 0 }
      renderer.topCtx 
  renderEnv /\ renderEnvStateId <- HK.useState $
    R.union
      { holeCount: 0 }
      renderer.topEnv

  exprGyro /\ exprGyroStateId <- HK.useState $ RootGyro input.expr

  -- runs before each render
  let syncedExprGyro = syncExprGyro exprGyro
  let gyroHtml /\ _ = unwrap $ runM renderCtx renderEnv $ renderExprGyro syncedExprGyro

  -- runs after each render
  HK.captures {} HK.useTickEffect do
    hydrateExprGyro syncedExprGyro
    pure Nothing

  -- render
  HK.pure $ do
    HH.div 
      [HP.classes [HH.ClassName "Panel Buffer"]]
      [ HH.div
          [HP.classes [HH.ClassName "PanelHeader"]]
          [ HH.div
              [ HP.classes [HH.ClassName "button"]
              , HE.onClick \_ -> Console.log "TODO: close buffer" ]
              [HH.text "X"]
          , HH.text "Buffer" ]
      , HH.div
          [HP.classes [HH.ClassName "PanelContent"]]
          [gyroHtml] ]

