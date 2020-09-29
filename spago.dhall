{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "html-parser"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "quickcheck"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
