module Test.Biscotti.CookieTest
  ( testSuite
  ) where

import Prelude

import Biscotti.Cookie as Cookie
import Biscotti.Cookie.Types (Cookie(..))
import Data.DateTime (DateTime(..), Time(..))
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((==?))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Test.Unit.QuickCheck (quickCheck)
import Test.Util.Assert (shouldContainString)

testSuite :: TestSuite
testSuite = do
  suite "Biscotti.Cookie" do
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
              , sameSite: Nothing
              , secure: false
              , httpOnly: false
              }

        Cookie.parse "key=val" `shouldEqual` Right expected

    suite "parseMany" do
      test "parses multiple name/value pairs only" do
        let expected = List.fromFoldable
              [ Cookie
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
              , Cookie
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
