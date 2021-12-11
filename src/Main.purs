module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RussianClock.App.VoiceSelect as VS

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI VS.component { language: Just "ru-RU" } body
