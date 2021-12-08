{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "integers"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "web-html"
  , "web-speech"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
