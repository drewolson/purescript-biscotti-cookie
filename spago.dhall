{ name =
    "biscotti-cookie"
, license =
    "MIT"
, repository =
    "https://github.com/drewolson/purescript-biscotti-cookie"
, dependencies =
    [ "datetime"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "formatters"
    , "gen"
    , "newtype"
    , "now"
    , "prelude"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "record"
    , "string-parsers"
    , "strings"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
