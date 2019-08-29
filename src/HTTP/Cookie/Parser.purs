module HTTP.Cookie.Parser
  ( parse
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int as Int
import Data.List (foldr)
import Data.Maybe (Maybe(..))
import Data.String as String
import HTTP.Cookie.Formatter (unformatDateTime)
import HTTP.Cookie.Types (Cookie)
import HTTP.Cookie.Types as Cookie
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.CodePoints (eof, noneOf, string)
import Text.Parsing.StringParser.Combinators (many, sepBy)

type Attribute = Cookie -> Cookie

dropSeperator :: Parser Unit
dropSeperator = (string "; " $> unit) <|> (pure unit)

asString :: forall f. Functor f => Foldable f => f Char -> String
asString = String.fromCodePointArray <<< Array.fromFoldable <<< map String.codePointFromChar

stringWithout :: forall f. Foldable f => f Char -> Parser String
stringWithout = map asString <<< many <<< noneOf

attributeValue :: Parser String
attributeValue = stringWithout [';']

whitespaceChars :: Array Char
whitespaceChars = [' ', '\n', '\r', '\t']

parseDomain :: Parser Attribute
parseDomain = do
  domain <- (string "Domain=") *> attributeValue

  pure $ Cookie.setDomain domain

parsePath :: Parser Attribute
parsePath = do
  path <- (string "Path=") *> attributeValue

  pure $ Cookie.setPath path

parseExpires :: Parser Attribute
parseExpires = do
  expiresString <- (string "Expires=") *> attributeValue

  case unformatDateTime expiresString of
    Left e ->
      fail e

    Right expires ->
      pure $ Cookie.setExpires expires

parseMaxAge :: Parser Attribute
parseMaxAge = do
  maxAgeString <- (string "Max-Age=") *> attributeValue

  case Int.fromString maxAgeString of
    Nothing ->
      fail "invalid integer"

    Just maxAge ->
      pure $ Cookie.setMaxAge maxAge

parseSecure :: Parser Attribute
parseSecure = (string "Secure") $> Cookie.setSecure

parseHttpOnly :: Parser Attribute
parseHttpOnly = (string "HttpOnly") $> Cookie.setHttpOnly

parseUnknown :: Parser Attribute
parseUnknown = attributeValue $> identity

parseAttribute :: Parser Attribute
parseAttribute =
  parseDomain
  <|> parsePath
  <|> parseExpires
  <|> parseMaxAge
  <|> parseSecure
  <|> parseHttpOnly
  <|> parseUnknown

parseCookie :: Parser Cookie
parseCookie = do
  name <- stringWithout ([';', ',', '='] <> whitespaceChars) <* string "="
  value <- stringWithout ([';', ','] <> whitespaceChars) <* dropSeperator
  attributes <- sepBy parseAttribute (string "; ") <* eof

  let cookie = foldr ($) (Cookie.new name value) attributes

  pure $ cookie

parse :: String -> Either ParseError Cookie
parse = runParser parseCookie
