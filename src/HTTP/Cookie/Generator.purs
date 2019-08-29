module HTTP.Cookie.Generator
  ( toString
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import HTTP.Cookie.Formatter (formatDateTime)
import HTTP.Cookie.Types (Cookie(..))

toString :: Cookie -> String
toString (Cookie cookie) = intercalate "; " $ catMaybes
  [ attr cookie.name $ Just cookie.value
  , attr "Domain" cookie.domain
  , attr "Path" cookie.path
  , attr "Expires" (formatDateTime <$> cookie.expires)
  , attr "Max-Age" (show <$> cookie.maxAge)
  , boolAttr "Secure" cookie.secure
  , boolAttr "HttpOnly" cookie.httpOnly
  ]

attr :: String -> Maybe String -> Maybe String
attr _ Nothing = Nothing
attr name (Just value) = Just $ name <> "=" <> value

boolAttr :: String -> Boolean -> Maybe String
boolAttr _ false = Nothing
boolAttr name true = Just name
