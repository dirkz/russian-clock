module RussianClock.App.RandomTime
  ( component
  ) where

import Prelude
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.String (drop, take)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import RussianClock.App.Clock as CL
import RussianClock.App.VoiceSelect as VS
import RussianClock.Util.RussianTime (timeString)
import RussianClock.Util.TimeStruct (TimeStruct)
import Type.Proxy (Proxy(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.Speech.TTS as TTS
import Web.Speech.TTS.SpeechSynthesisEvent (charIndex)
import Web.Speech.TTS.Utterance (listenToBoundary, listenToEnd)
import Web.Speech.TTS.Utterance as U
import Web.Speech.TTS.Voice as V
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

language ∷ String
language = "ru-RU"

type Slots
  = ( voiceSelect :: VS.Slot Int
    , clock :: CL.Slot Int
    )

_voiceSelect = Proxy :: Proxy "voiceSelect"

_clock = Proxy :: Proxy "clock"

type VoiceState
  = { maybeVoice :: Maybe V.Voice
    , rate :: Number
    }

data GameState
  = NothingYet
  | NewRandomTime
  | ShowSolution

derive instance eqGameState :: Eq GameState

type State
  = { time :: TimeStruct
    , voice :: VoiceState
    , maybeError :: Maybe String
    , gameState :: GameState
    , stringToRead :: String
    , stringAlreadyRead :: String
    , stringToReadLeft :: String
    }

data Action
  = Initialize
  | Random
  | Solve
  | Read
  | HandleVoiceSelection VS.Output
  | HandleClock CL.Output
  | ReadCharIndex Int
  | ReadToTheEnd
  | HandleKeyEvent KE.KeyboardEvent

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState:
        \_ ->
          { time: { hour: 12, minute: 0 }
          , voice:
              { maybeVoice: Nothing
              , rate: 0.8
              }
          , maybeError: Nothing
          , gameState: NothingYet
          , stringToRead: ""
          , stringAlreadyRead: ""
          , stringToReadLeft: ""
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
        , rate: st.voice.rate
        }
        HandleVoiceSelection
    , HH.p [ HP.classes [ HH.ClassName "time" ] ]
        [ HH.text "Try clicking the clock, or pressing <Enter>, <Space> or 'r'" ]
    , HH.slot _clock 0 CL.component
        { classContainer: "clock"
        , time: st.time
        }
        HandleClock
    , HH.p [ HP.classes [ HH.ClassName "time" ] ] case st.gameState of
        ShowSolution ->
          [ HH.span [ HP.classes [ HH.ClassName "already-read" ] ]
              [ HH.text st.stringAlreadyRead ]
          , HH.span [ HP.classes [ HH.ClassName "left-to-read" ] ]
              [ HH.text stringToReadLeft ]
          ]
        _ -> [ HH.text "" ]
    , case st.maybeError of
        Nothing -> HH.text ""
        Just err -> HH.p [ HP.classes [ HH.ClassName "error" ] ] [ HH.text err ]
    , HH.p [ HP.classes [ HH.ClassName "buttons" ] ]
        [ HH.button
            [ HE.onClick \_ -> Random ]
            [ HH.text "Random Time" ]
        , HH.button
            [ HP.enabled $ canSolve st
            , HE.onClick \_ -> Solve
            ]
            [ HH.text "Solve" ]
        , HH.button
            [ HP.enabled $ canRead st
            , HE.onClick \_ -> Read
            ]
            [ HH.text "Read" ]
        ]
    ]
  where
  stringToReadLeft =
    if st.stringToReadLeft /= "" then
      " >" <> st.stringToReadLeft
    else
      ""

canRead ::
  forall r.
  { gameState :: GameState
  | r
  } ->
  Boolean
canRead st = st.gameState == ShowSolution

canSolve ::
  forall r.
  { gameState :: GameState
  | r
  } ->
  Boolean
canSolve st = st.gameState == NewRandomTime

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    doc <- H.liftEffect $ document =<< window
    let
      emitter =
        eventListener
          KET.keydown
          (HTMLDocument.toEventTarget doc)
          (map HandleKeyEvent <<< KE.fromEvent)
    _ <- H.subscribe emitter
    handleAction Random
  Random -> do
    eraseError
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = 1 + (round $ rh * 11.0)

      minute = round $ rm * 59.0

      time = { hour, minute }
    H.modify_ \st ->
      st
        { time = time
        , gameState = NewRandomTime
        }
  Solve -> do
    H.modify_ \st -> st { gameState = ShowSolution }
    handleAction Read
  Read -> do
    eraseError
    st <- H.get
    case st.gameState of
      ShowSolution -> do
        let
          stringToRead = timeString st.time
        H.modify_ \sta ->
          sta
            { stringToRead = stringToRead
            , stringAlreadyRead = ""
            , stringToReadLeft = stringToRead
            }
        case st.voice.maybeVoice of
          Nothing -> signalError "Have no voice to read"
          Just voice -> do
            utt <- H.liftEffect $ U.createWithVoiceAndRate voice st.voice.rate stringToRead
            _ <- H.subscribe =<< charIndexEmitter utt ReadCharIndex
            _ <- H.subscribe =<< endOfReadingEmitter utt ReadToTheEnd
            w <- H.liftEffect window
            maybeTts <- H.liftEffect $ TTS.tts w
            case maybeTts of
              Nothing -> signalError "No TTS support while trying to read"
              Just tts -> do
                H.liftEffect $ TTS.cancel tts
                H.liftEffect $ TTS.speak tts utt
            pure unit
      _ -> pure unit
  HandleVoiceSelection output -> case output of
    VS.Voice v -> do
      H.modify_ \st -> st { voice { maybeVoice = Just v } }
      handleAction Read
    VS.Error str -> H.modify_ \st -> st { maybeError = Just str }
    VS.Rate rate -> do
      H.modify_ \st -> st { voice { rate = rate } }
      handleAction Read
  HandleClock output -> case output of
    CL.ClockClicked -> do
      st <- H.get
      case st.gameState of
        NewRandomTime -> handleAction Solve
        ShowSolution -> handleAction Read
        _ -> pure unit
  ReadCharIndex i ->
    H.modify_ \st ->
      st
        { stringAlreadyRead = take i st.stringToRead
        , stringToReadLeft = drop i st.stringToRead
        }
  ReadToTheEnd ->
    H.modify_ \st ->
      st
        { stringAlreadyRead = st.stringToRead
        , stringToReadLeft = ""
        }
  HandleKeyEvent ev -> do
    case KE.key ev of
      " " -> when noModifiers solveOrRandom
      "Enter" -> when noModifiers solveOrRandom
      "r" ->
        when noModifiers do
          H.liftEffect $ E.preventDefault $ KE.toEvent ev
          tryToRead
      _ -> log "*** unhandled key"
    where
    tryToRead = do
      st <- H.get
      when (canRead st) $ handleAction Read

    solveOrRandom = do
      H.liftEffect $ E.preventDefault $ KE.toEvent ev
      st <- H.get
      if canSolve st then
        handleAction Solve
      else
        handleAction Random

    noModifiers =
      KE.ctrlKey ev == false
        && KE.shiftKey ev
        == false
        && KE.altKey ev
        == false
        && KE.metaKey ev
        == false
        && KE.repeat ev
        == false
        && KE.isComposing ev
        == false
  where
  signalError string = H.modify_ \st -> st { maybeError = Just string }

  eraseError = H.modify_ \st -> st { maybeError = Nothing }

  charIndexEmitter :: forall m1 a. Bind m1 => MonadEffect m1 => U.Utterance -> (Int -> a) -> m1 (HS.Emitter a)
  charIndexEmitter utt constr = do
    { emitter, listener } <- H.liftEffect HS.create
    H.liftEffect $ listenToBoundary utt
      $ Just \ev -> HS.notify listener $ constr $ charIndex ev
    pure emitter

  endOfReadingEmitter :: forall m1 a. Bind m1 => MonadEffect m1 => U.Utterance -> a -> m1 (HS.Emitter a)
  endOfReadingEmitter utt constr = do
    { emitter, listener } <- H.liftEffect HS.create
    H.liftEffect $ listenToEnd utt
      $ Just \_ -> HS.notify listener $ constr
    pure emitter
