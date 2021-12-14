module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RussianClock.App.RandomTimePitch as RTP

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI RTP.component unit body
