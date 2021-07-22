let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210613/packages.dhall sha256:5f10380b3ca7d3a32ea5c2b7535e4814a5e3f3590c70692f76e596d6ab0687b3

let additions =
      { strictlypositiveint =
        { repo = "https://github.com/jamieyung/purescript-strictlypositiveint"
        , version = "master"
        , dependencies = [ "prelude" ]
        }
      , zipperarray =
        { repo = "https://github.com/jamieyung/purescript-zipperarray"
        , version = "master"
        , dependencies =
          [ "arrays", "maybe", "prelude", "naturals", "strictlypositiveint" ]
        }
      , event =
        { repo = "https://github.com/mikesol/purescript-event"
        , version = "master"
        , dependencies =
          [ "arrays"
          , "console"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "filterable"
          , "foldable-traversable"
          , "js-timers"
          , "maybe"
          , "now"
          , "ordered-collections"
          , "prelude"
          , "psci-support"
          , "refs"
          , "tuples"
          , "unsafe-reference"
          ]
        }
      , behaviors =
        { repo = "https://github.com/mikesol/purescript-behaviors"
        , version = "master"
        , dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        }
      , debugged =
        { dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "datetime"
          , "enums"
          , "unordered-collections"
          , "fixed-points"
          ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-debugged"
        , version = "633220f91f87c9acbc4eebbf87628e6cdc658b7b"
        }
      }

let packages =
      upstream
      with lunarlog-prelude = ./packages/loglude/spago.dhall as Location
      with lunarlog-geometry = ./packages/geometry/spago.dhall as Location
      with lunarlog-core = ./packages/core/spago.dhall as Location

in  packages // additions
