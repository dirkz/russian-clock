module Main where

import Prelude

import RussianClock.App.Time as Time
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Time.component unit body
