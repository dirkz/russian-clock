module RussianClock.App.Clock
  ( Input
  , Slot
  , component
  ) where

import Prelude

import Data.Array (filter, range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import RussianClock.Util.TimeStruct (TimeStruct)

type Input
  = { classContainer :: String
    , time :: TimeStruct
    }

type Slot id
  = forall q o. H.Slot q o id

type State
  = { classContainer :: String
    , time :: TimeStruct
    }

data Action
  = Receive Input

--|A TTS (text to speech, web speech synthesis) voice selector.
component :: forall q o m. MonadEffect m => MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState:
        \input -> input
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  SE.svg
    [ SA.classes [ HH.ClassName st.classContainer ]
    , SA.viewBox 0.0 0.0 width width
    , SA.preserveAspectRatio (Just { x_: SA.Mid, y_: SA.Mid }) SA.Meet
    ]
    ( [ SE.circle
          [ SA.cx center
          , SA.cy center
          , SA.r 45.0
          , SA.stroke color
          , SA.fill $ SA.NoColor
          ]
      , SE.line
          [ SA.x1 center
          , SA.y1 center
          , SA.x2 center
          , SA.y2 20.0
          , SA.stroke color
          , SA.transform [ SA.Rotate rotationHour center center ]
          ]
      , SE.line
          [ SA.x1 center
          , SA.y1 center
          , SA.x2 center
          , SA.y2 10.0
          , SA.stroke color
          , SA.transform [ SA.Rotate rotationMinute center center ]
          ]
      ]
        <> minuteCircles
    )
  where
  width = 100.0

  center = width / 2.0

  color = SA.Named "black"

  hour = if st.time.hour == 12 then 0 else st.time.hour

  anglePerHour = 360.0 / 12.0

  anglePerMinute = 360.0 / 60.0

  rotationHour = anglePerHour * toNumber hour

  rotationMinute = anglePerMinute * toNumber st.time.minute

  allMinutes = filter (\n -> n `mod` 5 == 0) $ range 0 59

  minuteCircle minute =
    SE.circle
      [ SA.cx center
      , SA.cy 7.5
      , SA.r 1.5
      , SA.stroke color
      , SA.fill $ SA.NoColor
      , SA.transform [ SA.Rotate (toNumber minute * anglePerMinute) center center ]
      ]

  minuteCircles = map minuteCircle allMinutes

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ \st ->
      st
        { classContainer = input.classContainer
        , time = input.time
        }
