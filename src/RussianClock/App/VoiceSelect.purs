module RussianClock.App.VoiceSelect
  ( Input
  , Output(..)
  , Slot
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
  --|An error occurred.
  | Error String
  --|The rate changed.
  | Rate Number

--|Configuration for the voice selector.
--|
--| Notes:
--| * `language`: An optional language (e.g., "ru-RU") if you want only voices for that langugage.
--| * `classContainer`: The css class name of the container.
--|   Must be a grid with three columns per row, the elements of which will be paragraphs.
type Input
  = { language :: Maybe String
    , classContainer :: String
    , rate :: Number
    }

type Slot id
  = forall q. H.Slot q Output id

type State
  = { voices :: Array V.Voice
    , maybeVoice :: Maybe V.Voice
    , rate :: Number
    , language :: Maybe String
    , classContainer :: String
    , showRateSelection :: Boolean
    }

data Action
  = Initialize
  | SelectVoiceByIndex Int
  --|All rate changes on-the-fly
  | RateInput String
  --|"Final" rate change
  | RateChange String
  --|Communicate the chosen voice to the parent
  | RaiseVoice
  | ToggleRateSelection

--|A TTS (text to speech, web speech synthesis) voice selector.
component :: forall q m. MonadEffect m => MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState:
        \input ->
          { voices: []
          , maybeVoice: Nothing
          , rate: input.rate
          , language: input.language
          , classContainer: input.classContainer
          , showRateSelection: false
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
    ( [ HH.p_
          [ HH.text $ fromMaybe none $ V.name <$> st.maybeVoice ]
      , HH.p_
          [ HH.select [ HE.onSelectedIndexChange SelectVoiceByIndex ]
              (map voiceOption st.voices)
          ]
      , HH.p_
          [ HH.button [ HE.onClick \_ -> ToggleRateSelection ]
              [ HH.text "Rate"
              ]
          ]
      ]
        <> rateSelection
    )
  where
  voiceName voice = V.name voice <> " (" <> V.lang voice <> ")"

  voiceOption voice = HH.option_ [ HH.text (voiceName voice) ]

  rateSelection = case st.showRateSelection of
    false -> []
    true ->
      [ HH.p_ [ HH.text "Rate" ]
      , HH.p_
          [ HH.input
              [ HP.type_ InputRange
              , HP.min U.rateMin
              , HP.max U.rateMax
              , HP.step $ Step 0.1
              , HP.value (show st.rate)
              , HE.onValueInput RateInput
              , HE.onValueChange RateChange
              ]
          ]
      , HH.p_ [ HH.text $ show st.rate ]
      ]

handleAction :: forall cs m. MonadEffect m => MonadAff m => Action ??? H.HalogenM State Action cs Output m Unit
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
  RateInput str -> do
    case fromString str of
      Nothing -> pure unit
      Just val -> do
        H.modify_ \st -> st { rate = val }
  RateChange str -> do
    handleAction $ RateInput str
    signalRateChange
  RaiseVoice -> do
    st <- H.get
    traverse_ (H.raise <<< Voice) st.maybeVoice
  ToggleRateSelection -> H.modify_ \st -> st { showRateSelection = not st.showRateSelection }
  where
  signalError string = H.raise $ Error string

  filterForLanguage st voice = case st.language of
    Nothing -> true
    Just lang -> V.lang voice == lang

  signalRateChange = do
    { rate } <- H.get
    H.raise $ Rate rate
