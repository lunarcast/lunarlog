{ name = "lunarlog-prelude"
, dependencies =
  [ "console", "effect", "prelude", "psci-support", "debugged", "event" ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
