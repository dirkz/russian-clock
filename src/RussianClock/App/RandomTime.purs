module RussianClock.App.RandomTime
  ( component
  ) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import RussianClock.App.VoiceSelect as VS
import RussianClock.Util.RussianTime (timeString)
import RussianClock.Util.TimeStruct (TimeStruct, timeStructString)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.Utterance (defaultRate)
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V

unknown :: String
unknown = "unknown"

language ∷ String
language = "ru-RU"

type Slots
  = ( voiceSelect :: forall query. H.Slot query VS.Output Int )

_voiceSelect = Proxy :: Proxy "voiceSelect"

type State
  = { maybeTime :: Maybe TimeStruct
    , voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , maybeError :: Maybe String
    , rate :: Number
    }

data Action
  = Initialize
  | Random
  | Read
  | HandleVoiceSelection VS.Output

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { maybeTime: Nothing
          , voices: []
          , maybeVoice: Nothing
          , maybeError: Nothing
          , rate: defaultRate
          }
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.article [ HP.classes [ HH.ClassName "container" ] ]
    [ HH.h1 [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "Russian Time" ]
    , HH.slot _voiceSelect 0 VS.component
        { language: Just language
        , classContainer: "voice-selection"
        , classVoiceName: "voice-selection-voice"
        }
        HandleVoiceSelection
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
  canRead = isJust st.maybeVoice && isJust st.maybeTime

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> handleAction Random
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
  Read -> do
    eraseError
    st <- H.get
    case timeString <$> st.maybeTime of
      Nothing -> signalError "Nothing to read"
      Just stringToRead -> do
        case st.maybeVoice of
          Nothing -> signalError "Have no voice to read"
          Just voice -> do
            utt <- H.liftEffect $ U.createWithVoiceAndRate voice st.rate stringToRead
            w <- H.liftEffect window
            maybeTts <- H.liftEffect $ TTS.tts w
            case maybeTts of
              Nothing -> signalError "No TTS support while trying to read"
              Just tts -> H.liftEffect $ TTS.speak tts utt
            pure unit
  HandleVoiceSelection output -> case output of
    VS.Voice v -> do
      H.modify_ \st -> st { maybeVoice = Just v }
      handleAction Read
    VS.Error str -> H.modify_ \st -> st { maybeError = Just str }
    VS.Rate rate -> do
      H.modify_ \st -> st { rate = rate }
      handleAction Read
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }

  eraseError = H.modify_ \st -> st { maybeError = Nothing }
