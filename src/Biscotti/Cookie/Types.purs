-- | This module contains the `Cookie` type and functions for operating on data within it.
module Biscotti.Cookie.Types
  ( Cookie(..)
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

import Control.Monad.Gen.Common (genMaybe)
import Data.DateTime (DateTime, modifyTime, setMillisecond)
import Data.DateTime as DateTime
import Data.DateTime.Gen (genDateTime)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Lens (Lens', lens)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.String.Gen (genAsciiString)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Days(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements, suchThat)

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
  show Lax    = "Lax"
  show None   = "None"

instance sameSiteArbitrary :: Arbitrary SameSite where
  arbitrary :: Gen SameSite
  arbitrary = elements $ Strict :| [Lax, None]

-- | The `Cookie` type
newtype Cookie = Cookie
  { name     :: String
  , value    :: String
  , domain   :: Maybe String
  , path     :: Maybe String
  , expires  :: Maybe DateTime
  , maxAge   :: Maybe Int
  , sameSite :: Maybe SameSite
  , secure   :: Boolean
  , httpOnly :: Boolean
  }

derive instance newtypeCookie :: Newtype Cookie _

derive newtype instance eqCookie :: Eq Cookie

derive newtype instance ordCookie :: Ord Cookie

derive newtype instance showCookie :: Show Cookie

instance cookieArbitrary :: Arbitrary Cookie where
  arbitrary :: Gen Cookie
  arbitrary = do
    name <- genAsciiString `suchThat` validName
    value <- genAsciiString `suchThat` validValue
    domain <- genMaybe $ pure "https://example.com"
    path <- genMaybe $ pure "/"
    expires <- genMaybe $ zeroMillisconds <$> genDateTime
    maxAge <- arbitrary
    sameSite <- arbitrary
    secure <- arbitrary
    httpOnly <- arbitrary

    pure $ Cookie
      { name
      , value
      , domain
      , path
      , expires
      , maxAge
      , sameSite
      , secure
      , httpOnly
      }
    where
      zeroMillisconds :: DateTime -> DateTime
      zeroMillisconds dateTime =
        case toEnum 0 of
          Just millsecond ->
            modifyTime (setMillisecond millsecond) dateTime
          Nothing ->
            dateTime

      validName :: String -> Boolean
      validName = not <<< Regex.test $ unsafeRegex """[;,\s=]""" noFlags

      validValue :: String -> Boolean
      validValue = not <<< Regex.test $ unsafeRegex """[;,\s]""" noFlags

-- | The constructor for `Cookie`
new :: String -> String -> Cookie
new name value = Cookie
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

-- | Expire an existing `Cookie`. This sets the `Expires` attribute of
-- | the cookie to yesterday's date.
expire :: Cookie -> Effect (Either String Cookie)
expire cookie = do
  now <- nowDateTime
  let maybeDate = DateTime.adjust (Days $ -1.0) now

  case maybeDate of
    Nothing ->
      pure $ Left "Invalid date"

    Just yesterday ->
      pure $ Right $ setExpires yesterday cookie

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

_domain :: Lens' Cookie (Maybe String)
_domain = _Newtype <<< (lens _.domain $ _ { domain = _ })

_expires :: Lens' Cookie (Maybe DateTime)
_expires = _Newtype <<< (lens _.expires $ _ { expires = _ })

_httpOnly :: Lens' Cookie Boolean
_httpOnly = _Newtype <<< (lens _.httpOnly $ _ { httpOnly = _ })

_maxAge :: Lens' Cookie (Maybe Int)
_maxAge = _Newtype <<< (lens _.maxAge $ _ { maxAge = _ })

_name :: Lens' Cookie String
_name = _Newtype <<< (lens _.name $ _ { name = _ })

_path :: Lens' Cookie (Maybe String)
_path = _Newtype <<< (lens _.path $ _ { path = _ })

_sameSite :: Lens' Cookie (Maybe SameSite)
_sameSite = _Newtype <<< (lens _.sameSite $ _ { sameSite = _ })

_secure :: Lens' Cookie Boolean
_secure = _Newtype <<< (lens _.secure $ _ { secure = _ })

_value :: Lens' Cookie String
_value = _Newtype <<< (lens _.value $ _ { value = _ })
