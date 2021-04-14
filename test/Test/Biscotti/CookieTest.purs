module Test.Biscotti.CookieTest
  ( testSuite
  ) where

import Prelude
import Biscotti.Cookie (Cookie, SameSite(..))
import Biscotti.Cookie as Cookie
import Control.Monad.Gen (elements, suchThat)
import Control.Monad.Gen.Common (genMaybe)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.DateTime (DateTime(..), Time(..), modifyTime, setMillisecond)
import Data.DateTime as DateTime
import Data.DateTime.Gen (genDateTime)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty ((:|))
import Data.String.Gen (genAsciiString)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary, (==?))
import Test.QuickCheck.Gen (Gen)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.QuickCheck (quickCheck)
import Test.Util.Assert (shouldContainString)

newtype TestCookie
  = TestCookie Cookie

instance arbitraryTestCookie :: Arbitrary TestCookie where
  arbitrary :: Gen TestCookie
  arbitrary = do
    name <- genAsciiString `suchThat` validName
    value <- genAsciiString `suchThat` validValue
    domain <- genMaybe $ pure "https://example.com"
    path <- genMaybe $ pure "/"
    expires <- genMaybe $ zeroMillisconds <$> genDateTime
    maxAge <- arbitrary
    sameSite <- genMaybe $ genSameSite
    secure <- arbitrary
    httpOnly <- arbitrary
    pure
      $ TestCookie
      $ Cookie.fromFields
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
    genSameSite :: Gen SameSite
    genSameSite = elements $ fromNonEmpty $ Strict :| [ Lax, None ]

    zeroMillisconds :: DateTime -> DateTime
    zeroMillisconds dateTime = case toEnum 0 of
      Just millsecond -> modifyTime (setMillisecond millsecond) dateTime
      Nothing -> dateTime

    validName :: String -> Boolean
    validName = not <<< Regex.test $ unsafeRegex """[;,\s=]""" noFlags

    validValue :: String -> Boolean
    validValue = not <<< Regex.test $ unsafeRegex """[;,\s]""" noFlags

testSuite :: TestSuite
testSuite = do
  suite "Biscotti.Cookie" do
    suite "properties" do
      test "parse and stringify round trip correctly" do
        quickCheck \(TestCookie cookie) -> do
          let
            new = Cookie.parse $ Cookie.stringify $ cookie
          new ==? Right cookie
    suite "stringify" do
      test "produces a correctly-formated expires attribute" do
        let
          date = unsafePartial $ fromJust $ DateTime.canonicalDate <$> toEnum 2019 <*> toEnum 12 <*> toEnum 1
        let
          time = unsafePartial $ fromJust $ Time <$> toEnum 12 <*> toEnum 1 <*> toEnum 2 <*> toEnum 0
        let
          cookieString = Cookie.stringify $ Cookie.setExpires (DateTime date time) $ Cookie.new "foo" "bar"
        cookieString `shouldContainString` "Expires=Sun, 01 Dec 2019 12:01:02 GMT"
    suite "parse" do
      test "parses a simple cookie" do
        let
          expected =
            Cookie.fromFields
              { name: "key"
              , value: "val"
              , domain: Nothing
              , path: Nothing
              , expires: Nothing
              , maxAge: Nothing
              , sameSite: Nothing
              , secure: false
              , httpOnly: false
              }
        Cookie.parse "key=val" `shouldEqual` Right expected
    suite "parseMany" do
      test "parses multiple name/value pairs only" do
        let
          expected =
            List.fromFoldable
              [ Cookie.fromFields
                  { name: "key1"
                  , value: "val1"
                  , domain: Nothing
                  , path: Nothing
                  , expires: Nothing
                  , maxAge: Nothing
                  , sameSite: Nothing
                  , secure: false
                  , httpOnly: false
                  }
              , Cookie.fromFields
                  { name: "key2"
                  , value: "val2"
                  , domain: Nothing
                  , path: Nothing
                  , expires: Nothing
                  , maxAge: Nothing
                  , sameSite: Nothing
                  , secure: false
                  , httpOnly: false
                  }
              ]
        Cookie.parseMany "key1=val1; key2=val2" `shouldEqual` Right expected
