module RussianClock.Util.RussianTime
  ( timeString
  )
  where

import Prelude
import RussianClock.Util.TimeStruct (TimeStruct)

import Data.Int (rem)

timeString :: TimeStruct -> String
timeString time
  | time.minute == 30 =
  "полови́на" <> " " <> (genitiveOrdinalHour $ plusOneHour time.hour)

timeString time
  | time.hour == 0 && time.minute == 0 = nominativeHour 12 <> " " <> "часо́в"

timeString time
  | time.hour == 1 && time.minute == 0 = "час"

timeString time
  | time.minute == 0 =
  (nominativeHour time.hour) <> " " <> (hourAfterNumber time.hour)

timeString time = if time.minute < 30
  then
    (nominativeMinute time.minute)
    <> " "
    <> minuteAfterNumber time.minute
    <> " "
    <> (genitiveOrdinalHour $ plusOneHour time.hour)
  else
    "без"
    <> " "
    <> genitiveMinute (60 - time.minute)
    <> theMinute
    <> " "
    <> (genitiveOrdinalHour $ plusOneHour time.hour)
 where
  theMinute =
    let correctedMinute = if time.minute < 30 then time.minute else 60 - time.minute
    in  if correctedMinute `rem` 5 == 0
          then ""
          else " " <> minuteAfterNumber correctedMinute

plusOneHour :: Int -> Int
plusOneHour h = (h + 1) `mod` 12

nominativeMinute :: Int -> String
nominativeMinute 1  = "одна́"
nominativeMinute 2  = "две"
nominativeMinute 3  = "три"
nominativeMinute 4  = "четы́ре"
nominativeMinute 5  = "пять"
nominativeMinute 6  = "шесть"
nominativeMinute 7  = "семь"
nominativeMinute 8  = "во́семь"
nominativeMinute 9  = "де́вять"
nominativeMinute 10 = "де́сять"
nominativeMinute 11 = "оди́ннадцать"
nominativeMinute 12 = "двена́дцать"
nominativeMinute 13 = "трина́дцать"
nominativeMinute 14 = "четы́рнадцать"
nominativeMinute 15 = "пятна́дцать"
nominativeMinute 16 = "шестна́дцать"
nominativeMinute 17 = "семна́дцать"
nominativeMinute 18 = "восемна́дцать"
nominativeMinute 19 = "девятна́дцать"
nominativeMinute 20 = "два́дцать"
nominativeMinute 21 = "два́дцать одна́"
nominativeMinute 22 = "два́дцать два"
nominativeMinute 23 = "два́дцать три"
nominativeMinute 24 = "два́дцать четы́ре"
nominativeMinute 25 = "два́дцать пять"
nominativeMinute 26 = "два́дцать шесть"
nominativeMinute 27 = "два́дцать семь"
nominativeMinute 28 = "два́дцать во́семь"
nominativeMinute 29 = "два́дцать де́вять"
nominativeMinute m  = "nominativeMinute: " <> show m <> "?"

genitiveMinute :: Int -> String
genitiveMinute 1  = "одно́й"
genitiveMinute 2  = "двух"
genitiveMinute 3  = "трёх"
genitiveMinute 4  = "четырёх"
genitiveMinute 5  = "пяти́"
genitiveMinute 6  = "шести́"
genitiveMinute 7  = "семи́"
genitiveMinute 8  = "восьми́"
genitiveMinute 9  = "девяти́"
genitiveMinute 10 = "десяти́"
genitiveMinute 11 = "оди́ннадцати"
genitiveMinute 12 = "двена́дцати"
genitiveMinute 13 = "трина́дцати"
genitiveMinute 14 = "четы́рнадцати"
genitiveMinute 15 = "пятна́дцати"
genitiveMinute 16 = "шестна́дцати"
genitiveMinute 17 = "семна́дцати"
genitiveMinute 18 = "восемна́дцати"
genitiveMinute 19 = "девятна́дцати"
genitiveMinute 20 = "двадцати́"
genitiveMinute 21 = "двадцати́ одно́й"
genitiveMinute 22 = "двадцати́ двух"
genitiveMinute 23 = "двадцати́ трёх"
genitiveMinute 24 = "двадцати́ четырёх"
genitiveMinute 25 = "двадцати́ пяти́"
genitiveMinute 26 = "двадцати́ шести́"
genitiveMinute 27 = "двадцати́ семи́"
genitiveMinute 28 = "двадцати́ восьми́"
genitiveMinute 29 = "двадцати́ девяти́"
genitiveMinute m  = "genitiveMinute: " <> show m <> "?"

nominativeHour :: Int -> String
nominativeHour 1  = "оди́н"
nominativeHour 2  = "два"
nominativeHour 3  = "три"
nominativeHour 4  = "четы́ре"
nominativeHour 5  = "пять"
nominativeHour 6  = "шесть"
nominativeHour 7  = "семь"
nominativeHour 8  = "во́семь"
nominativeHour 9  = "де́вять"
nominativeHour 10 = "де́сять"
nominativeHour 11 = "оди́ннадцать"
nominativeHour 12 = "двена́дцать"
nominativeHour h  = "nominativeHour: " <> show h <> "?"

genitiveOrdinalHour :: Int -> String
genitiveOrdinalHour 1  = "пе́рвого"
genitiveOrdinalHour 2  = "второ́го"
genitiveOrdinalHour 3  = "тре́тьего"
genitiveOrdinalHour 4  = "четвёртого"
genitiveOrdinalHour 5  = "пя́того"
genitiveOrdinalHour 6  = "шесто́го"
genitiveOrdinalHour 7  = "седьмо́го"
genitiveOrdinalHour 8  = "восьмо́го"
genitiveOrdinalHour 9  = "девя́того"
genitiveOrdinalHour 10 = "деся́того"
genitiveOrdinalHour 11 = "оди́ннадцатого"
genitiveOrdinalHour 12 = "двена́дцатого"
genitiveOrdinalHour 0  = "двена́дцатого"
genitiveOrdinalHour h  = "genitiveOrdinalHour: " <> show h <> "?"

hourAfterNumber :: Int -> String
hourAfterNumber 2  = "часа́"
hourAfterNumber 3  = "часа́"
hourAfterNumber 4  = "часа́"
hourAfterNumber 5  = "часо́в"
hourAfterNumber 6  = "часо́в"
hourAfterNumber 7  = "часо́в"
hourAfterNumber 8  = "часо́в"
hourAfterNumber 9  = "часо́в"
hourAfterNumber 10 = "часо́в"
hourAfterNumber 11 = "часо́в"
hourAfterNumber 12 = "часо́в"
hourAfterNumber h  = "hourAfterNumber: " <> show h <> "?"

minuteAfterNumber :: Int -> String
minuteAfterNumber 1  = "мину́та"
minuteAfterNumber 2  = "мину́ты"
minuteAfterNumber 3  = "мину́ты"
minuteAfterNumber 4  = "мину́ты"
minuteAfterNumber 5  = "мину́т"
minuteAfterNumber 6  = "мину́т"
minuteAfterNumber 7  = "мину́т"
minuteAfterNumber 8  = "мину́т"
minuteAfterNumber 9  = "мину́т"
minuteAfterNumber 10 = "мину́т"
minuteAfterNumber 11 = "мину́т"
minuteAfterNumber 12 = "мину́т"
minuteAfterNumber 13 = "мину́т"
minuteAfterNumber 14 = "мину́т"
minuteAfterNumber 15 = "мину́т"
minuteAfterNumber 16 = "мину́т"
minuteAfterNumber 17 = "мину́т"
minuteAfterNumber 18 = "мину́т"
minuteAfterNumber 19 = "мину́т"
minuteAfterNumber 20 = "мину́т"
minuteAfterNumber 21 = "мину́та"
minuteAfterNumber 22 = "мину́ты"
minuteAfterNumber 23 = "мину́ты"
minuteAfterNumber 24 = "мину́ты"
minuteAfterNumber 25 = "мину́т"
minuteAfterNumber 26 = "мину́т"
minuteAfterNumber 27 = "мину́т"
minuteAfterNumber 28 = "мину́т"
minuteAfterNumber 29 = "мину́т"
minuteAfterNumber 30 = "мину́т"
minuteAfterNumber m  = "minuteAfterNumber: " <> show m <> "?"
