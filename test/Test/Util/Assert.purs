module Test.Util.Assert
  ( shouldContainString
  ) where

import Prelude

import Data.String (Pattern(..))
import Data.String.CodeUnits as String
import Test.Unit (Test)
import Test.Unit.Assert (assert)

shouldContainString :: String -> String -> Test
shouldContainString haystack needle = do
  assert ("Expected \"" <> haystack <> "\" to contain \"" <> needle <> "\"") $
    String.contains (Pattern needle) haystack
