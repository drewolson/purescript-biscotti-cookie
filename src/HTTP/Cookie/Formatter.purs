module HTTP.Cookie.Formatter
  ( formatDateTime
  , unformatDateTime
  ) where

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter
import Data.List as List

expiresFormatter :: Formatter
expiresFormatter = List.fromFoldable
  [ DayOfWeekNameShort
  , Placeholder ", "
  , DayOfMonthTwoDigits
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , YearFull
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder " GMT"
  ]

formatDateTime :: DateTime -> String
formatDateTime = Formatter.format expiresFormatter

unformatDateTime :: String -> Either String DateTime
unformatDateTime = Formatter.unformat expiresFormatter
