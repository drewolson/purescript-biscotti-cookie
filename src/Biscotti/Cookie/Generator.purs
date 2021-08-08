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
import Biscotti.Cookie.Types
  ( Cookie
  , getDomain
  , getExpires
  , getHttpOnly
  , getMaxAge
  , getName
  , getPath
  , getSameSite
  , getSecure
  , getValue
  )
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
stringify cookie =
  intercalate "; "
    $ catMaybes
      [ attr (getName cookie) $ Just (getValue cookie)
      , attr domainTag $ getDomain cookie
      , attr pathTag $ getPath cookie
      , attr expiresTag $ formatDateTime <$> getExpires cookie
      , attr maxAgeTag $ show <$> getMaxAge cookie
      , attr sameSiteTag $ show <$> getSameSite cookie
      , flag secureTag $ getSecure cookie
      , flag httpOnlyTag $ getHttpOnly cookie
      ]

attr :: String -> Maybe String -> Maybe String
attr name = case _ of
  Nothing -> Nothing
  Just value -> Just $ name <> "=" <> value

flag :: String -> Boolean -> Maybe String
flag name = case _ of
  false -> Nothing
  true -> Just name
