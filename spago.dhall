{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "integers"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "web-speech"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
