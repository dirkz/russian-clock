module RussianClock.App.VoiceSelect
  ( Input
  , Output(..)
  , component
  ) where

import Prelude
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Array (filter, (!!))
import Data.Foldable (traverse_)
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
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V

none :: String
none = "none"

data Output
  --|The voice was selected.
  = Voice V.Voice

--|Configuration for the voice selector.
--|
--| Notes:
--| * `language`: An optional language (e.g., "ru-RU") if you want only voices for that langugage.
--| * `classContainer`: The css class name of the container.
--|   Must be a grid with three columns per row, the elements of which will be paragraphs.
--| * `classError`: The css class name of the error paragraph, which spans a whole row.
--| * `classVoiceName`: The css class name of the voice name paragraph, which should span two columns.
type Input
  = { language :: Maybe String
    , classContainer :: String
    , classError :: String
    , classVoiceName :: String
    }

type State
  = { voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , maybeError :: Maybe String
    , pitchRateVolume :: U.PitchRateVolume
    , language :: Maybe String
    , classContainer :: String
    , classError :: String
    , classVoiceName :: String
    }

data Action
  = Initialize
  | SelectVoiceByIndex Int
  --|All pitch changes on-the-fly
  | PitchInput String
  --|"Final" pitch change
  | PitchChange String
  --|All rate changes on-the-fly
  | RateInput String
  --|"Final" rate change
  | RateChange String
  --|All volume changes on-the-fly
  | VolumeInput String
  --|"Final" volume change
  | VolumeChange String
  --|Communicate the chosen voice to the parent
  | RaiseVoice

--|A TTS (text to speech, web speech synthesis) voice selector.
component :: forall q m. MonadEffect m => MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState:
        \input ->
          { voices: []
          , maybeVoice: Nothing
          , maybeError: Nothing
          , pitchRateVolume: U.defaultPitchRateVolume
          , language: input.language
          , classContainer: input.classContainer
          , classError: input.classError
          , classVoiceName: input.classVoiceName
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
  HH.article [ HP.classes [ HH.ClassName st.classContainer ] ]
    [ HH.p [ HP.classes [ HH.ClassName st.classVoiceName ] ]
        [ HH.text $ fromMaybe none $ V.name <$> st.maybeVoice ]
    , HH.p_
        [ HH.select [ HE.onSelectedIndexChange SelectVoiceByIndex ]
            (map voiceOption st.voices)
        ]
    , case st.maybeError of
        Nothing -> HH.text ""
        Just err -> HH.p [ HP.classes [ HH.ClassName st.classError ] ] [ HH.text err ]
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
    , HH.p_ [ HH.text "Rate" ]
    , HH.p_
        [ HH.input
            [ HP.type_ InputRange
            , HP.min U.rateMin
            , HP.max U.rateMax
            , HP.step $ Step 0.1
            , HP.value (show st.pitchRateVolume.rate)
            , HE.onValueInput RateInput
            , HE.onValueChange RateChange
            ]
        ]
    , HH.p_ [ HH.text $ show st.pitchRateVolume.rate ]
    , HH.p_ [ HH.text "Volume" ]
    , HH.p_
        [ HH.input
            [ HP.type_ InputRange
            , HP.min U.volumeMin
            , HP.max U.volumeMax
            , HP.step $ Step 0.1
            , HP.value (show st.pitchRateVolume.volume)
            , HE.onValueInput VolumeInput
            , HE.onValueChange VolumeChange
            ]
        ]
    , HH.p_ [ HH.text $ show st.pitchRateVolume.volume ]
    ]
  where
  voiceName voice = V.name voice <> " (" <> V.lang voice <> ")"

  voiceOption voice = HH.option_ [ HH.text (voiceName voice) ]

handleAction :: forall cs m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  Initialize -> do
    w <- H.liftEffect window
    maybeSynth <- H.liftEffect $ TTS.tts w
    case maybeSynth of
      Nothing -> signalError "No TTS support while trying to get the voices"
      Just synth -> do
        st <- H.get
        voices <-
          map (map (filter (filterForLanguage st)))
            H.liftAff
            $ TTS.voices synth
        H.modify_ \st2 -> st2 { voices = voices, maybeVoice = voices !! 0 }
        handleAction RaiseVoice
  SelectVoiceByIndex i -> do
    H.modify_ \st -> st { maybeVoice = st.voices !! i }
    handleAction RaiseVoice
  PitchInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> H.modify_ \st -> st { pitchRateVolume { pitch = val } }
  PitchChange str -> do
    handleAction $ PitchInput str
  RateInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> H.modify_ \st -> st { pitchRateVolume { rate = val } }
  RateChange str -> do
    handleAction $ RateInput str
  VolumeInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> H.modify_ \st -> st { pitchRateVolume { volume = val } }
  VolumeChange str -> do
    handleAction $ VolumeInput str
  RaiseVoice -> do
    st <- H.get
    traverse_ (H.raise <<< Voice) st.maybeVoice
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }

  filterForLanguage st voice = case st.language of
    Nothing -> true
    Just lang -> V.lang voice == lang
