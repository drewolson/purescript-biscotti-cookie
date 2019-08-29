module HTTP.Cookie.Types
  ( Cookie(..)
  , _domain
  , _expires
  , _httpOnly
  , _maxAge
  , _name
  , _path
  , _secure
  , _value
  , new
  , setDomain
  , setExpires
  , setHttpOnly
  , setMaxAge
  , setPath
  , setSecure
  ) where

import Prelude

import Control.Monad.Gen.Common (genMaybe)
import Data.DateTime (DateTime, modifyTime, setMillisecond)
import Data.DateTime.Gen (genDateTime)
import Data.Enum (toEnum)
import Data.Lens (Lens', lens)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Gen (genAsciiString)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat)

newtype Cookie = Cookie
  { name     :: String
  , value    :: String
  , domain   :: Maybe String
  , path     :: Maybe String
  , expires  :: Maybe DateTime
  , maxAge   :: Maybe Int
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
    secure <- arbitrary
    httpOnly <- arbitrary

    pure $ Cookie
      { name
      , value
      , domain
      , path
      , expires
      , maxAge
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

new :: String -> String -> Cookie
new name value = Cookie
  { name
  , value
  , domain: Nothing
  , path: Nothing
  , expires: Nothing
  , maxAge: Nothing
  , secure: false
  , httpOnly: false
  }

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

_secure :: Lens' Cookie Boolean
_secure = _Newtype <<< (lens _.secure $ _ { secure = _ })

_value :: Lens' Cookie String
_value = _Newtype <<< (lens _.value $ _ { value = _ })
