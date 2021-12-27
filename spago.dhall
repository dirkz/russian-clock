{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  , "web-events"
  , "web-html"
  , "web-speech"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
