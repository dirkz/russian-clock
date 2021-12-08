module RussianClock.App.Time
  ( component
  ) where

import Prelude
import RussianClock.Util.TimeStruct (TimeStruct)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Int (round)

type State
  = { maybeTime :: Maybe TimeStruct }

data Action
  = Initialize

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { maybeTime: Nothing }
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
    ]

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    rh <- H.liftEffect random
    rm <- H.liftEffect random
    let
      hour = round $ rh * 12.0

      minute = round $ rm * 60.0

      time = { hour, minute }
    H.modify_ \st -> st { maybeTime = Just time }
