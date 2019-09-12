-- | This module is responsible for parsing a string into an
-- | `Either ParseError Cookie`.
module Biscotti.Cookie.Parser
  ( parse
  , parseMany
  ) where

import Prelude

import Biscotti.Cookie.Formatter
  ( domainTag
  , expiresTag
  , httpOnlyTag
  , maxAgeTag
  , pathTag
  , sameSiteTag
  , secureTag
  , unformatDateTime
  )
import Biscotti.Cookie.Types (Cookie, SameSite(..))
import Biscotti.Cookie.Types as Cookie
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
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
  domain <- (string $ domainTag <> "=") *> attributeValue

  pure $ Cookie.setDomain domain

parsePath :: Parser Attribute
parsePath = do
  path <- (string $ pathTag <> "=") *> attributeValue

  pure $ Cookie.setPath path

parseExpires :: Parser Attribute
parseExpires = do
  expiresString <- (string $ expiresTag <> "=") *> attributeValue

  case unformatDateTime expiresString of
    Left e ->
      fail e

    Right expires ->
      pure $ Cookie.setExpires expires

parseMaxAge :: Parser Attribute
parseMaxAge = do
  maxAgeString <- (string $ maxAgeTag <> "=") *> attributeValue

  case Int.fromString maxAgeString of
    Nothing ->
      fail "invalid integer"

    Just maxAge ->
      pure $ Cookie.setMaxAge maxAge

parseSecure :: Parser Attribute
parseSecure = (string secureTag) $> Cookie.setSecure

parseHttpOnly :: Parser Attribute
parseHttpOnly = (string httpOnlyTag) $> Cookie.setHttpOnly

parseSameSite :: Parser Attribute
parseSameSite = do
  sameSiteString <- (string $ sameSiteTag <> "=") *> attributeValue

  case sameSiteString of
    "Strict" -> pure $ Cookie.setSameSite Strict
    "Lax"    -> pure $ Cookie.setSameSite Lax
    "None"   -> pure $ Cookie.setSameSite None
    val      -> fail $ "unknown SiteSite value " <> val

parseAttribute :: Parser Attribute
parseAttribute =
  parseDomain
  <|> parsePath
  <|> parseExpires
  <|> parseMaxAge
  <|> parseSecure
  <|> parseHttpOnly
  <|> parseSameSite

parseSimpleCookie :: Parser Cookie
parseSimpleCookie = do
  name <- stringWithout ([';', ',', '='] <> whitespaceChars) <* string "="
  value <- stringWithout ([';', ','] <> whitespaceChars)

  pure $ Cookie.new name value

parseCookie :: Parser Cookie
parseCookie = do
  cookie <- parseSimpleCookie <* dropSeperator
  attributes <- sepBy parseAttribute (string "; ") <* eof

  pure $ foldl (#) cookie attributes

parseCookies :: Parser (List Cookie)
parseCookies = sepBy parseSimpleCookie (string "; ") <* eof

-- | Parses a `String` into an `Either ParseError Cookie`. This
-- | function is primarily intended to parse the `Set-Cookie`
-- | header on the client.
-- |
-- | ```purescript
-- | > Parser.parse "key=value; Secure"
-- | (Right { name: "key", value: "value", secure: true, ... })
-- | ```
parse :: String -> Either ParseError Cookie
parse = runParser parseCookie

-- | Parses a `String` into an `Either ParseError (List Cookie)`.
-- | HTTP requests can include multiple cookies in a single
-- | `Cookie` header consisting of only name/value pairs. This
-- | function can be used to parse this header.
-- |
-- | ```purescript
-- | > parseMany "key1=value1; key2=value2"
-- | (Right ({ name: "key1", value: "value1", ... } : { name: "key2", value: "value2", ... } : Nil))
-- | ```
parseMany :: String -> Either ParseError (List Cookie)
parseMany = runParser parseCookies
