{ name = "lunarlog-prelude"
, dependencies = [ "console", "effect", "prelude", "psci-support", "event" ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
