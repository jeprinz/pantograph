module Pantograph.Generic.App where

import Pantograph.Generic.Dynamics
import Pantograph.Generic.Rendering
import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver as VDomDriver
import Type.Proxy (Proxy)
import Pantograph.Generic.App.Editor (editorComponent)

class Dynamics sn el ctx env <= App sn el ctx env | sn -> el ctx env where
  editorInput :: EditorInput sn el ctx env

runEditor :: forall sn el ctx env.
  App sn el ctx env =>
  Proxy sn ->
  EditorOptions -> 
  Aff (H.HalogenIO (Const Void) Void Aff)
runEditor _ (EditorOptions {}) = VDomDriver.runUI cmp editorInput =<< HA.awaitBody
  where
  cmp = editorComponent :: EditorComponent sn el ctx env

