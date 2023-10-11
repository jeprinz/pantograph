module Test.InlineCss where

import Prelude
import CSS as Css
import Data.Tuple.Nested
import CSS as StyleSheet
import Effect (Effect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCss
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.VDom.Driver as VDomDriver

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   Console.log "[main]"
--   HA.awaitBody >>= VDomDriver.runUI component unit
--   where
--   component = HK.component \_ _ -> do

--     x /\ xStateId <- HK.useState 0

--     HK.pure $
--       HH.div_
--         [ HCss.stylesheet styles
--         , HH.button [] [HH.text "adjust"]
--         , HH.div [HP.class_ (HH.ClassName "target")] [HH.text "This is some example text."]
--         ]

--   styles = do
--     Css.fromString ".target" Css.? do
--       Css.color $ Css.xyz 1.0 0.0 0.0