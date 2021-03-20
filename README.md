# purescript-biscotti-cookie

[![Build
Status](https://github.com/drewolson/purescript-biscotti-cookie/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/drewolson/purescript-biscotti-cookie/actions/workflows/test.yml)

Documentation is available on
[Pursuit](https://pursuit.purescript.org/packages/purescript-biscotti-cookie).

This library allows parsing and generating cookie headers. You'll generally use
the `Cookie.new` function to create a cookie from a name/value pair and the
`Cookie.set*` functions to set attributes on a cookie.

`Cookie.stringify` generates the string representation of a cookie, suitable for
writing to an HTTP header.

`Cookie.parse` parses the string representation of a cookie, returning an
`Either ParseError Cookie`.

```purescript
import Biscotti.Cookie as Cookie

> Cookie.stringify $ Cookie.setSecure $ Cookie.new "key" "value"
key=value; Secure

> Cookie.parse "key=value; Secure"
(Right { domain: Nothing, expires: Nothing, httpOnly: false, maxAge: Nothing, name: "key", path: Nothing, secure: true, value: "value" })
```

## Running the tests

```text
spago test
```

## License

MIT
