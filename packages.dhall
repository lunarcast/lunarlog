let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210613/packages.dhall sha256:5f10380b3ca7d3a32ea5c2b7535e4814a5e3f3590c70692f76e596d6ab0687b3

let additions =
      { event =
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
      }

let packages =
      upstream
      with lunarlog-prelude = ./packages/loglude/spago.dhall as Location

in  packages // additions
