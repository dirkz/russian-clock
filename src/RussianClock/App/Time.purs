module RussianClock.App.Time
  ( component
  ) where

import Prelude
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import RussianClock.Util.TimeStruct (TimeStruct, timeStructString)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.Voice as V

unknown :: String
unknown = "unknown"

type State
  = { maybeTime :: Maybe TimeStruct
    , voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    }

data Action
  = Initialize
  | Random

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { maybeTime: Nothing, voices: [], maybeVoice: Nothing }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.article_
    [ HH.h1_ [ HH.text "Russian Time" ]
    , HH.p_ [ HH.text $ fromMaybe unknown $ timeStructString <$> st.maybeTime ]
    , HH.p_
        [ HH.button
            [ HE.onClick \_ -> Random ]
            [ HH.text "Random Time" ]
        ]
    ]

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> handleAction Random
  Random -> do
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = round $ rh * 12.0

      minute = round $ rm * 60.0

      time = { hour, minute }
    H.modify_ \st -> st { maybeTime = Just time }
