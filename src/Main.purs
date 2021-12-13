module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RussianClock.App.RandomTime as RT

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI RT.component unit body
