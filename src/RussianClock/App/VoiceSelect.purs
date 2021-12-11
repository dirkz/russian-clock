module RussianClock.App.VoiceSelect
  ( component
  ) where

import Prelude
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (filter, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (window)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.Utterance (PitchRateVolume, defaultPitchRateVolume)
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V

language ∷ String
language = "ru-RU"

none :: String
none = "none"

type State
  = { voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , maybeError :: Maybe String
    , pitchRateVolume :: PitchRateVolume
    }

data Action
  = Initialize
  | SelectVoice Int
  --| All changes on-the-fly
  | PitchInput String
  --| "Final" change
  | PitchChange String

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { voices: []
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

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.article [ HP.classes [ HH.ClassName "container" ] ]
    [ HH.h1 [ HP.classes [ HH.ClassName "title" ] ] [ HH.text "Russian Time" ]
    , HH.div [ HP.classes [ HH.ClassName "voice-control" ] ]
        [ HH.p_ [ HH.text $ fromMaybe none $ V.name <$> st.maybeVoice ]
        , HH.p_ []
        , HH.p_
            [ HH.select [ HE.onSelectedIndexChange SelectVoice ]
                (map voiceOption st.voices)
            ]
        , HH.p_ [ HH.text "Pitch" ]
        , HH.p_
            [ HH.input
                [ HP.type_ InputRange
                , HP.min U.pitchMin
                , HP.max U.pitchMax
                , HP.step $ Step 0.1
                , HP.value (show st.pitchRateVolume.pitch)
                , HE.onValueInput PitchInput
                , HE.onValueChange PitchChange
                ]
            ]
        , HH.p_ [ HH.text $ show st.pitchRateVolume.pitch ]
        ]
    , HH.p [ HP.classes [ HH.ClassName "clock" ] ] []
    , case st.maybeError of
        Nothing -> HH.text ""
        Just err -> HH.p [ HP.classes [ HH.ClassName "error" ] ] [ HH.text err ]
    ]
  where
  voiceName voice = V.name voice <> " (" <> V.lang voice <> ")"

  voiceOption voice = HH.option_ [ HH.text (voiceName voice) ]

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    w <- H.liftEffect window
    maybeSynth <- H.liftEffect $ TTS.tts w
    case maybeSynth of
      Nothing -> signalError "No TTS support while trying to get the voices"
      Just synth -> do
        voices <-
          map (map (filter (\v -> V.lang v == language)))
            H.liftAff
            $ TTS.voices synth
        H.modify_ \st -> st { voices = voices, maybeVoice = voices !! 0 }
  SelectVoice i -> do
    H.modify_ \st -> st { maybeVoice = st.voices !! i }
  PitchInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> H.modify_ \st -> st { pitchRateVolume { pitch = val } }
  PitchChange str -> do
    handleAction $ PitchInput str
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }
