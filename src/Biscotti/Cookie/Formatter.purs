-- | This module is response for formatting and parsing the `Expires`
-- | attribute of a `Cookie`.
module Biscotti.Cookie.Formatter
  ( domainTag
  , expiresTag
  , formatDateTime
  , httpOnlyTag
  , maxAgeTag
  , pathTag
  , sameSiteTag
  , secureTag
  , unformatDateTime
  ) where

import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter
import Data.List as List

domainTag :: String
domainTag = "Domain"

expiresTag :: String
expiresTag = "Expires"

httpOnlyTag :: String
httpOnlyTag = "HttpOnly"

maxAgeTag :: String
maxAgeTag = "Max-Age"

pathTag :: String
pathTag = "Path"

secureTag :: String
secureTag = "Secure"

sameSiteTag :: String
sameSiteTag = "SameSite"

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
