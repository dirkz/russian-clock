module RussianClock.App.RandomTime
  ( component
  ) where

import Prelude
import Data.Array (filter, (!!))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import RussianClock.Util.TimeStruct (TimeStruct, timeStructString)
import Web.HTML (window)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.Voice as V
import Effect.Aff.Class (class MonadAff)

unknown :: String
unknown = "unknown"

language ∷ String
language = "ru-RU"

none :: String
none = "none"

type State
  = { maybeTime :: Maybe TimeStruct
    , voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , maybeError :: Maybe String
    }

data Action
  = Initialize
  | Random
  | SelectVoice Int

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { maybeTime: Nothing
          , voices: []
          , maybeVoice: Nothing
          , maybeError: Nothing
          }
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
    , HH.p_
        [ HH.select [ HE.onSelectedIndexChange SelectVoice ]
            (map voiceOption st.voices)
        , HH.text $ fromMaybe none $ V.name <$> st.maybeVoice
        ]
    , HH.p_ [ HH.text $ fromMaybe unknown $ timeStructString <$> st.maybeTime ]
    , case st.maybeError of
        Nothing -> HH.text ""
        Just err -> HH.p [ HP.classes [ HH.ClassName "error" ] ] [ HH.text err ]
    , HH.p_
        [ HH.button
            [ HE.onClick \_ -> Random ]
            [ HH.text "Random Time" ]
        ]
    ]
  where
  voiceName voice = V.name voice <> " (" <> V.lang voice <> ")"

  voiceOption voice = HH.option_ [ HH.text (voiceName voice) ]

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Random
    w <- H.liftEffect window
    maybeSynth <- H.liftEffect $ TTS.tts w
    case maybeSynth of
      Nothing -> H.modify_ \st -> st { maybeError = Just "No TTS support" }
      Just synth -> do
        voices <-
          map (map (filter (\v -> V.lang v == language)))
            H.liftAff
            $ TTS.voices synth
        H.modify_ \st -> st { voices = voices, maybeVoice = voices !! 0 }
  Random -> do
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = round $ rh * 12.0

      minute = round $ rm * 60.0

      time = { hour, minute }
    H.modify_ \st -> st { maybeTime = Just time }
  SelectVoice i -> H.modify_ \st -> st { maybeVoice = st.voices !! i }
