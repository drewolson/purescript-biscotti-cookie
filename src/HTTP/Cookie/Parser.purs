module HTTP.Cookie.Parser
  ( parse
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Function (applyFlipped)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import HTTP.Cookie.Formatter (domainTag, expiresTag, httpOnlyTag, maxAgeTag, pathTag, sameSiteTag, secureTag, unformatDateTime)
import HTTP.Cookie.Types (Cookie(..), SameSite(..))
import HTTP.Cookie.Types as Cookie
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser, try)
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

parseFields :: Parser Cookie
parseFields = do
  name <- stringWithout ([';', ',', '='] <> whitespaceChars) <* string "="
  value <- stringWithout ([';', ','] <> whitespaceChars) <* dropSeperator
  attributes <- sepBy parseAttribute (string "; ") <* eof
  let cookie = foldl applyFlipped (Cookie.new name value) attributes

  pure cookie

parseEmpty :: Parser Cookie
parseEmpty = string "" *> eof $> Empty

parseCookie :: Parser Cookie
parseCookie = (try parseFields) <|> parseEmpty

parse :: String -> Either ParseError Cookie
parse = runParser parseCookie
