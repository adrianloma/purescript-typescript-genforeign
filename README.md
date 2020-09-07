# purescript-typescript-genforeign

This package contains set of functions for parsing TypeScript declaration files (*.d.ts) and generating PureScript code from it.

## Installation
This library is still in development. I haven't figured out how to publish, and I planned on testing it out with a real codebase to find obvious problems. I'm not even sure if the generated code works!

Because of that, installation must be manual.

Spago reference is [here](https://github.com/purescript/spago#add-a-package-to-the-package-set)

```nix
let upstream = -- <package set URL here, (don't change this)>
in  upstream
  with typescript-genforeign =
    { dependencies =
        [ "argonaut"
        , "argonaut-codecs"
        , "argonaut-core"
        , "argonaut-generic"
        , "console"
        , "effect"
        , "foldable-traversable"
        , "generics-rep"
        , "integers"
        ]
    , repo =
        "https://github.com/adrianloma/purescript-typescript-genforeign""
    , version =
        "master"  -- branch, tag, or commit hash
    }
```

```shell
spago install typescript-genforeign
```

## Documentation

You can build the documentation by running `spago docs`.


## Todos
- static methods
- untagged union type import (investigate viability)
- callbacks / functions
- namespaced code (especially, a module calling another namespace in the same file)
