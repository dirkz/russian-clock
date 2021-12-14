module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RussianClock.App.RandomTimeFull as RTF

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI RTF.component unit body
