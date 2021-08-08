-- | This module contains the `Cookie` type and functions for operating on data within it.
module Biscotti.Cookie.Types
  ( Cookie
  , SameSite(..)
  , _domain
  , _expires
  , _httpOnly
  , _maxAge
  , _name
  , _path
  , _secure
  , _value
  , expire
  , fromFields
  , getDomain
  , getExpires
  , getHttpOnly
  , getMaxAge
  , getName
  , getPath
  , getSameSite
  , getSecure
  , getValue
  , new
  , setDomain
  , setExpires
  , setHttpOnly
  , setMaxAge
  , setPath
  , setSameSite
  , setSecure
  ) where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Lens (Iso', Lens', iso)
import Data.Lens as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Type.Prelude (Proxy(..))

-- | Type representing a `Cookie`'s optional `SameSite` attribute.
data SameSite
  = Strict
  | Lax
  | None

derive instance eqSameSite :: Eq SameSite

derive instance ordSameSite :: Ord SameSite

instance showSameSite :: Show SameSite where
  show :: SameSite -> String
  show Strict = "Strict"
  show Lax = "Lax"
  show None = "None"

type CookieFields =
  { name :: String
  , value :: String
  , domain :: Maybe String
  , path :: Maybe String
  , expires :: Maybe DateTime
  , maxAge :: Maybe Int
  , sameSite :: Maybe SameSite
  , secure :: Boolean
  , httpOnly :: Boolean
  }

-- | The `Cookie` type
newtype Cookie = Cookie CookieFields

derive newtype instance eqCookie :: Eq Cookie

derive newtype instance ordCookie :: Ord Cookie

derive newtype instance showCookie :: Show Cookie

-- | The constructor for `Cookie`
new :: String -> String -> Cookie
new name value =
  Cookie
    { name
    , value
    , domain: Nothing
    , path: Nothing
    , expires: Nothing
    , maxAge: Nothing
    , sameSite: Nothing
    , secure: false
    , httpOnly: false
    }

fromFields :: CookieFields -> Cookie
fromFields = Cookie

-- | Expire an existing `Cookie`. This sets the `Expires` attribute of
-- | the cookie to yesterday's date.
expire :: Cookie -> Effect (Either String Cookie)
expire cookie = do
  now <- nowDateTime
  let maybeDate = DateTime.adjust (Days $ -1.0) now

  case maybeDate of
    Nothing -> pure $ Left "Invalid date"
    Just yesterday -> pure $ Right $ setExpires yesterday cookie

getDomain :: Cookie -> Maybe String
getDomain = Lens.view _domain

getExpires :: Cookie -> Maybe DateTime
getExpires = Lens.view _expires

getHttpOnly :: Cookie -> Boolean
getHttpOnly = Lens.view _httpOnly

getMaxAge :: Cookie -> Maybe Int
getMaxAge = Lens.view _maxAge

getName :: Cookie -> String
getName = Lens.view _name

getPath :: Cookie -> Maybe String
getPath = Lens.view _path

getSameSite :: Cookie -> Maybe SameSite
getSameSite = Lens.view _sameSite

getSecure :: Cookie -> Boolean
getSecure = Lens.view _secure

getValue :: Cookie -> String
getValue = Lens.view _value

setDomain :: String -> Cookie -> Cookie
setDomain = Lens.setJust _domain

setExpires :: DateTime -> Cookie -> Cookie
setExpires = Lens.setJust _expires

setHttpOnly :: Cookie -> Cookie
setHttpOnly = Lens.set _httpOnly true

setMaxAge :: Int -> Cookie -> Cookie
setMaxAge = Lens.setJust _maxAge

setPath :: String -> Cookie -> Cookie
setPath = Lens.setJust _path

setSameSite :: SameSite -> Cookie -> Cookie
setSameSite = Lens.setJust _sameSite

setSecure :: Cookie -> Cookie
setSecure = Lens.set _secure true

_Cookie :: Iso' Cookie CookieFields
_Cookie = iso (\(Cookie fields) -> fields) Cookie

_domain :: Lens' Cookie (Maybe String)
_domain = _Cookie <<< prop (Proxy :: Proxy "domain")

_expires :: Lens' Cookie (Maybe DateTime)
_expires = _Cookie <<< prop (Proxy :: Proxy "expires")

_httpOnly :: Lens' Cookie Boolean
_httpOnly = _Cookie <<< prop (Proxy :: Proxy "httpOnly")

_maxAge :: Lens' Cookie (Maybe Int)
_maxAge = _Cookie <<< prop (Proxy :: Proxy "maxAge")

_name :: Lens' Cookie String
_name = _Cookie <<< prop (Proxy :: Proxy "name")

_path :: Lens' Cookie (Maybe String)
_path = _Cookie <<< prop (Proxy :: Proxy "path")

_sameSite :: Lens' Cookie (Maybe SameSite)
_sameSite = _Cookie <<< prop (Proxy :: Proxy "sameSite")

_secure :: Lens' Cookie Boolean
_secure = _Cookie <<< prop (Proxy :: Proxy "secure")

_value :: Lens' Cookie String
_value = _Cookie <<< prop (Proxy :: Proxy "value")
