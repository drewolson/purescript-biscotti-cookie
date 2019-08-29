module HTTP.Cookie.Types
  ( Cookie(..)
  , Fields
  , SameSite(..)
  , _Cookie
  , _domain
  , _expires
  , _httpOnly
  , _maxAge
  , _name
  , _path
  , _secure
  , _value
  , empty
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
import Data.DateTime.Gen (genDateTime)
import Data.Enum (toEnum)
import Data.Lens (Prism', Traversal', lens, prism')
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.String.Gen (genAsciiString)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements, suchThat)

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

type Fields =
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

data Cookie
  = Cookie Fields
  | Empty

derive instance eqCookie :: Eq Cookie

derive instance ordCookie :: Ord Cookie

instance showCookie :: Show Cookie where
  show :: Cookie -> String
  show (Cookie r) = "Cookie " <> show r
  show Empty      = "Empty"

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

    let cookie = Cookie
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

    elements $ cookie :| [Empty]
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
  , sameSite: Nothing
  , secure: false
  , httpOnly: false
  }

empty :: Cookie
empty = Empty

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

_Cookie :: Prism' Cookie Fields
_Cookie = prism' Cookie $ case _ of
  (Cookie r) -> Just r
  _          -> Nothing

_domain :: Traversal' Cookie (Maybe String)
_domain = _Cookie <<< (lens _.domain $ _ { domain = _ })

_expires :: Traversal' Cookie (Maybe DateTime)
_expires = _Cookie <<< (lens _.expires $ _ { expires = _ })

_httpOnly :: Traversal' Cookie Boolean
_httpOnly = _Cookie <<< (lens _.httpOnly $ _ { httpOnly = _ })

_maxAge :: Traversal' Cookie (Maybe Int)
_maxAge = _Cookie <<< (lens _.maxAge $ _ { maxAge = _ })

_name :: Traversal' Cookie String
_name = _Cookie <<< (lens _.name $ _ { name = _ })

_path :: Traversal' Cookie (Maybe String)
_path = _Cookie <<< (lens _.path $ _ { path = _ })

_sameSite :: Traversal' Cookie (Maybe SameSite)
_sameSite = _Cookie <<< (lens _.sameSite $ _ { sameSite = _ })

_secure :: Traversal' Cookie Boolean
_secure = _Cookie <<< (lens _.secure $ _ { secure = _ })

_value :: Traversal' Cookie String
_value = _Cookie <<< (lens _.value $ _ { value = _ })
