module RussianClock.App.RandomTime
  ( component
  ) where

import Prelude
import Data.Array (filter, (!!))
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString)
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
import Web.Speech.TTS.Utterance (PitchRateVolume, defaultPitchRateVolume)
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V
import RussianClock.App.VoiceSelect as VS
import Type.Proxy (Proxy(..))

unknown :: String
unknown = "unknown"

language ∷ String
language = "ru-RU"

type SlotsVoiceSelect
  = ( voiceSelect :: forall query. H.Slot query Void Int )

_voiceSelect = Proxy :: Proxy "voiceSelect"

type State
  = { maybeTime :: Maybe TimeStruct
    , voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , maybeError :: Maybe String
    , pitchRateVolume :: PitchRateVolume
    }

data Action
  = Initialize
  | Random
  | SelectVoice Int
  | Read
  --| All changes on-the-fly
  | PitchInput String
  --| "Final" change
  | PitchChange String

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { maybeTime: Nothing
          , voices: []
          , maybeVoice: Nothing
          , maybeError: Nothing
          , pitchRateVolume: defaultPitchRateVolume
          }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action SlotsVoiceSelect m
render st =
  HH.article [ HP.classes [ HH.ClassName "container" ] ]
    [ HH.h1 [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "Russian Time" ]
    , HH.slot_ _voiceSelect 0 VS.component
        { language: Just language
        , classContainer: "voice-selection"
        , classError: "voice-selection-error"
        , classVoiceName: "voice-selection-voice"
        }
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
        , case canRead of
            false -> HH.text ""
            true ->
              HH.button
                [ HE.onClick \_ -> Read ]
                [ HH.text "Read" ]
        ]
    ]
  where
  voiceName voice = V.name voice <> " (" <> V.lang voice <> ")"

  voiceOption voice = HH.option_ [ HH.text (voiceName voice) ]

  canRead = isJust st.maybeVoice && isJust st.maybeTime

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
        handleAction Read
  Random -> do
    eraseError
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = round $ rh * 12.0

      minute = round $ rm * 60.0

      time = { hour, minute }
    H.modify_ \st ->
      st
        { maybeTime = Just time
        }
    handleAction Read
  SelectVoice i -> do
    H.modify_ \st -> st { maybeVoice = st.voices !! i }
    handleAction Read
  Read -> do
    eraseError
    st <- H.get
    case timeString <$> st.maybeTime of
      Nothing -> signalError "Nothing to read"
      Just stringToRead -> do
        case st.maybeVoice of
          Nothing -> signalError "Have no voice to read"
          Just voice -> do
            utt <- H.liftEffect $ U.createWithVoiceAndPitch voice st.pitchRateVolume stringToRead
            w <- H.liftEffect window
            maybeTts <- H.liftEffect $ TTS.tts w
            case maybeTts of
              Nothing -> signalError "No TTS support while trying to read"
              Just tts -> H.liftEffect $ TTS.speak tts utt
            pure unit
  PitchInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> H.modify_ \st -> st { pitchRateVolume { pitch = val } }
  PitchChange str -> do
    handleAction $ PitchInput str
    handleAction Read
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }

  eraseError = H.modify_ \st -> st { maybeError = Nothing }
