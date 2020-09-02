{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-typescript-genforeign"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "integers"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
