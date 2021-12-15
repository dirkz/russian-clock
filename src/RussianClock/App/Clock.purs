module RussianClock.App.Clock
  ( Input
  , Slot
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
            }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.article [ HP.classes [ HH.ClassName st.classContainer ] ]
    [ SE.svg
        [ SA.classes [ HH.ClassName st.classContainer ]
        , SA.viewBox 0.0 0.0 100.0 100.0
        , SA.preserveAspectRatio (Just { x_: SA.Mid, y_: SA.Mid }) SA.Meet
        ]
        [ SE.circle
            [ SA.cx 50.0
            , SA.cy 50.0
            , SA.r 40.0
            , SA.stroke $ SA.Named "black"
            , SA.fill $ SA.NoColor
            ]
        ]
    ]

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction _ = pure unit
