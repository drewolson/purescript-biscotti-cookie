-- | This module allows parsing and generating cookie headers. You'll generally use
-- | the `Cookie.new` function to create a cookie from a name/value pair and the
-- | `Cookie.set*` functions to set attributes on a cookie.
-- |
-- | `Cookie.stringify` generates the string representation of a cookie, suitable for
-- | writing to an HTTP header.
-- |
-- | `Cookie.parse` parses the string representation of a cookie, returning an
-- | `Either ParseError Cookie`.
-- |
-- | ```purescript
-- | import Biscotti.Cookie as Cookie
-- |
-- | > Cookie.stringify $ Cookie.setSecure $ Cookie.new "key" "value"
-- | key=value; Secure
-- |
-- | > Cookie.parse "key=value; Secure"
-- | (Right { name: "key", value: "value", secure: true, ... })
-- | ```
module Biscotti.Cookie
  ( module Biscotti.Cookie.Generator
  , module Biscotti.Cookie.Parser
  , module Biscotti.Cookie.Types
  ) where

import Biscotti.Cookie.Generator (stringify)
import Biscotti.Cookie.Parser (parse, parseMany)
import Biscotti.Cookie.Types
  ( Cookie
  , SameSite(..)
  , _domain
  , _expires
  , _httpOnly
  , _maxAge
  , _name
  , _path
  , _secure
  , _value
  , expire
  , getDomain
  , getExpires
  , getHttpOnly
  , getMaxAge
  , getName
  , getPath
  , getSecure
  , getValue
  , new
  , setDomain
  , setExpires
  , setHttpOnly
  , setMaxAge
  , setPath
  , setSecure
  )
