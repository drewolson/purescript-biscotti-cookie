module HTTP.Cookie.Types
  ( Cookie(..)
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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
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
setDomain domain (Cookie cookie) = Cookie $ cookie { domain = Just domain }

setPath :: String -> Cookie -> Cookie
setPath path (Cookie cookie) = Cookie $ cookie { path = Just path }

setExpires :: DateTime -> Cookie -> Cookie
setExpires expires (Cookie cookie) = Cookie $ cookie { expires = Just expires }

setMaxAge :: Int -> Cookie -> Cookie
setMaxAge maxAge (Cookie cookie) = Cookie $ cookie { maxAge = Just maxAge }

setSecure :: Cookie -> Cookie
setSecure = Newtype.wrap <<< _ { secure = true } <<< Newtype.unwrap

setHttpOnly :: Cookie -> Cookie
setHttpOnly = Newtype.wrap <<< _ { httpOnly = true } <<< Newtype.unwrap
