module RussianClock.Util.TimeStruct
  ( TimeStruct
  , timeStructString
  ) where

import Prelude
import Data.String (length)

type TimeStruct
  = { hour :: Int
    , minute :: Int
    }

timeStructString ::
  forall r.
  { hour :: Int, minute :: Int
  | r
  } ->
  String
timeStructString time = nshow time.hour <> ":" <> nshow time.minute
  where
  nullPrefix str
    | length str == 1 = "0" <> str
    | otherwise = str

  nshow = show >>> nullPrefix
