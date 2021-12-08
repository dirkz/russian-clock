module RussianClock.App.RandomTime
  ( component
  ) where

import Prelude
import Data.Array (filter, (!!))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import RussianClock.Util.RussianTime (timeString)
import RussianClock.Util.TimeStruct (TimeStruct, timeStructString)
import Web.HTML (window)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V

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
    , maybeStringTimeToRead :: Maybe String
    }

data Action
  = Initialize
  | Random
  | SelectVoice Int
  | Read

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { maybeTime: Nothing
          , voices: []
          , maybeVoice: Nothing
          , maybeError: Nothing
          , maybeStringTimeToRead: Nothing
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
  HH.article [ HP.classes [ HH.ClassName "container" ] ]
    [ HH.h1 [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "Russian Time" ]
    , HH.p [ HP.classes [ HH.ClassName "voice-name" ] ]
        [ HH.text $ fromMaybe none $ V.name <$> st.maybeVoice ]
    , HH.p [ HP.classes [ HH.ClassName "voice-selection" ] ]
        [ HH.select [ HE.onSelectedIndexChange SelectVoice ]
            (map voiceOption st.voices)
        ]
    , HH.p [ HP.classes [ HH.ClassName "clock" ] ] []
    , HH.p [ HP.classes [ HH.ClassName "time" ] ]
        [ HH.text $ fromMaybe unknown $ timeStructString <$> st.maybeTime ]
    , case st.maybeError of
        Nothing -> HH.text ""
        Just err -> HH.p [ HP.classes [ HH.ClassName "error" ] ] [ HH.text err ]
    , HH.p [ HP.classes [ HH.ClassName "random-time" ] ]
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
      Nothing ->
        H.modify_ \st ->
          st
            { maybeError =
              Just "No TTS support while trying to get the voices"
            }
      Just synth -> do
        voices <-
          map (map (filter (\v -> V.lang v == language)))
            H.liftAff
            $ TTS.voices synth
        H.modify_ \st -> st { voices = voices, maybeVoice = voices !! 0 }
  Random -> do
    eraseError
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = round $ rh * 12.0

      minute = round $ rm * 60.0

      time = { hour, minute }

      russianTime = timeString time
    H.modify_ \st ->
      st
        { maybeTime = Just time
        , maybeStringTimeToRead = Just russianTime
        }
    handleAction Read
  SelectVoice i -> H.modify_ \st -> st { maybeVoice = st.voices !! i }
  Read -> do
    eraseError
    st <- H.get
    case st.maybeStringTimeToRead of
      Nothing -> signalError "Nothing to read"
      Just stringToRead -> do
        utt <- H.liftEffect $ U.create stringToRead
        case st.maybeVoice of
          Nothing -> signalError "Have no voice to read"
          Just voice -> do
            H.liftEffect $ U.setVoice utt voice
            w <- H.liftEffect window
            maybeTts <- H.liftEffect $ TTS.tts w
            case maybeTts of
              Nothing -> signalError "No TTS support while trying to read"
              Just tts -> H.liftEffect $ TTS.speak tts utt
            pure unit
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }

  eraseError = H.modify_ \st -> st { maybeError = Nothing }
