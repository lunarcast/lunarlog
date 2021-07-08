{ name = "lunarlog-geometry"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "sized-vectors"
  , "unordered-collections"
  , "undefined-is-not-a-problem"
  , "web-uievents"
  , "event"
  , "lunarlog-prelude"
  , "canvas"
  , "behaviors"
  , "foreign"
  , "foreign-object"
  , "record"
  , "type-equality"
  , "web-html"
  , "profunctor-lenses"
  , "run"
  , "zipperarray"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
