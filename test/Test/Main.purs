module Test.Main where

import Prelude

import Effect (Effect)
import Test.Biscotti.CookieTest as CookieTest
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  CookieTest.testSuite
