module Main where

import Prelude

import RussianClock.App.VoiceSelect as VS
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI VS.component unit body
