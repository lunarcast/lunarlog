{ name = "lunarlog-core"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "sized-vectors"
  , "event"
  , "lunarlog-prelude"
  , "lunarlog-geometry"
  , "record"
  , "unordered-collections"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
