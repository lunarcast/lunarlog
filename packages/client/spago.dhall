{ name = "lunarlog-client"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "sized-vectors"
  , "web-uievents"
  , "event"
  , "canvas"
  , "record"
  , "web-html"
  , "debug"
  , "control"
  , "run"
  , "debugged"
  , "lunarlog-prelude"
  , "lunarlog-geometry"
  , "lunarlog-core"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
