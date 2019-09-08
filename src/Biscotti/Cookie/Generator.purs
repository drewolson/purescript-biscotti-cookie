-- | This module is responsible for generating the string representation
-- | of a `Cookie`.
module Biscotti.Cookie.Generator
  ( stringify
  ) where

import Prelude

import Biscotti.Cookie.Formatter
  ( domainTag
  , expiresTag
  , formatDateTime
  , httpOnlyTag
  , maxAgeTag
  , pathTag
  , sameSiteTag
  , secureTag
  )
import Biscotti.Cookie.Types (Cookie(..))
import Data.Array (catMaybes)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

-- | Return the `String` representation of a `Cookie`.
-- |
-- | ```purescript
-- | > Generator.stringify $ Cookie.setSecure $ Cookie.new "key" "value"
-- | key=value; Secure
-- | ```
stringify :: Cookie -> String
stringify (Cookie cookie) = intercalate "; " $ catMaybes
  [ attr cookie.name $ Just cookie.value
  , attr domainTag cookie.domain
  , attr pathTag cookie.path
  , attr expiresTag $ formatDateTime <$> cookie.expires
  , attr maxAgeTag $ show <$> cookie.maxAge
  , attr sameSiteTag $ show <$> cookie.sameSite
  , flag secureTag cookie.secure
  , flag httpOnlyTag cookie.httpOnly
  ]

attr :: String -> Maybe String -> Maybe String
attr _ Nothing = Nothing
attr name (Just value) = Just $ name <> "=" <> value

flag :: String -> Boolean -> Maybe String
flag _ false = Nothing
flag name true = Just name
