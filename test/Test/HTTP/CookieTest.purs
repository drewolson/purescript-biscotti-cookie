module Test.HTTP.CookieTest
  ( testSuite
  ) where

import Prelude

import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import HTTP.Cookie as Cookie
import HTTP.Cookie.Types (Cookie(..))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((==?))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.QuickCheck (quickCheck)
import Test.Util.Assert (shouldContainString)

testSuite :: TestSuite
testSuite = do
  suite "HTTP.Cookie" do
    suite "properties" do
      test "parse and stringify round trip correctly" do
        quickCheck \cookie -> do
          let new = Cookie.parse $ Cookie.stringify $ cookie

          new ==? Right cookie

    suite "stringify" do
      test "produces a correctly-formated expires attribute" do
        let date = unsafePartial $ fromJust $ DateTime.canonicalDate <$> toEnum 2019 <*> toEnum 12 <*> toEnum 1
        let time = unsafePartial $ fromJust $ Time <$> toEnum 12 <*> toEnum 1 <*> toEnum 2 <*> toEnum 0
        let cookieString = Cookie.stringify $ Cookie.setExpires (DateTime date time) $ Cookie.new "foo" "bar"

        cookieString `shouldContainString` "Expires=Sun, 01 Dec 2019 12:01:02 GMT"

    suite "parse" do
      test "parses a simple cookie" do
        let expected = Cookie
              { name: "key"
              , value: "val"
              , domain: Nothing
              , path: Nothing
              , expires: Nothing
              , maxAge: Nothing
              , secure: false
              , httpOnly: false
              }

        Cookie.parse "key=val" `shouldEqual` Right expected
