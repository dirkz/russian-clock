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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
