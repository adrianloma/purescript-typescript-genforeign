-- | # Introduction
-- |
-- | This library aims to be simple to use, but at the same time flexible. The intended usage of
-- | this library is to convert TypeScript definition files (*.d.ts) easily and quickly,
-- | but if things go wrong or the defaults are not ideal, the library exposes it's API
-- | to make using its component parts possible.
-- |
-- | There are examples of usages in this module, and the function names are extremely verbose
-- | in hopes that it would make things easier to understand. The library does not read or write
-- | files, it merely processes strings. It uses the TypeScript compiler as a backend. That code
-- | is found in `Parse.js`.
-- |
-- | The situations where this library works perfectly are probably very slim, and the number of
-- | TypeScript features this library supports is also small. But the idea of this library is to
-- | make it easier for a developer who wishes to do bindings for a large library without having
-- | to do it all manually and have something to work with initially.
-- |
-- | Further, this library is an exploration into what can be done with regards to automatic code
-- | generation of bindings to PureScript using TypeScript, which could potentially make
-- | generating bindings easier and hence open the number of libraries available in the
-- | PureScript ecosystem. It is by no means elegant, complete, or bug-free.
-- |
-- | There is a huge repository of typescript definition files for a huge amount of popular npm
-- | packages here: https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types
-- |
-- | How I envision this library used is to get the types using:
-- | ```
-- | npm install --save-dev @types/7zip-min
-- | // which gets: https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/7zip-min/
-- | ```
-- | Then write a program that reads the node_modules/@types/7zip-min/index.d.ts and finally
-- | creates the appropriate files with a library like this.
-- |
-- | # Basic usage
-- |
-- | Use `tryParseSourceFile` function re-exported from the `Parse` module to convert a string
-- | of `d.ts` code into a `SourceFile` instance.
-- |
-- | Use the functions `tsSourceModuleTo*sModule` to transform a SourceFile into a `PsModule` or
-- | a `JsModule`
-- |
-- | Optionally, transform the PsModule or JsModule records.
-- |
-- | Finally, use the `psModuleToString` or `jsModuleToString` to transform the `PsModule` or
-- | `JsModule` into a string that contains the source code for the FFI module, ready to be saved
-- | to disk.
-- |
-- |
-- | Alternatively, the function with the least configuration is the
-- | `tryCreateModulesFromStringSource` which can be used to directly convert a TypeScript d.ts
-- | string source into a pair of string sources (PureScript and JavaScript), ready to be saved
-- | to disk.
module Text.TypeScript.GenForeign
      ( tsSourceFileToJsModuleString
      , tsSourceFileToPsModuleString
      , changeFuncReturnTypeWith
      , tryCreateModulesFromStringSource
      , GenSourcePair
      , module Gen
      , module Parse
      )
       where

import Prelude

import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array (null) as A
import Data.Array.NonEmpty as NE
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Text.TypeScript.GenForeign.GenJs (jsModuleToString, tsSourceModuleToJsModule) as Gen
import Text.TypeScript.GenForeign.GenPs (PsModule, psModuleToString, tsSourceFileToPsModule) as Gen
import Text.TypeScript.Parse as Parse
import Text.TypeScript.Type (SourceFile)

-- | Takes the module name for the required library (the 'require("your_required_library") in the JS'), a `SourceFile`, returns a JavaScript source code string
tsSourceFileToJsModuleString :: String -> SourceFile -> String
tsSourceFileToJsModuleString requiredLibrary sourceFile =
  Gen.jsModuleToString $ Gen.tsSourceModuleToJsModule requiredLibrary sourceFile

-- | Takes the module name for the PureScript module, a `SourceFile`, returns a PureScript source code string
tsSourceFileToPsModuleString :: String -> SourceFile -> String
tsSourceFileToPsModuleString moduleName sourceFile =
  Gen.psModuleToString $ Gen.tsSourceFileToPsModule moduleName sourceFile

type PsModuleFilter = Gen.PsModule -> Gen.PsModule

-- | Takes the module name for the PureScript module, a `SourceFile`, an array of filters (of type `PsModule -> PsModule`), returns a PureScript source code string
tsSourceFileToPsModuleStringWithFilters :: String -> SourceFile -> Array PsModuleFilter -> String
tsSourceFileToPsModuleStringWithFilters moduleName sourceFile filters =
  Gen.psModuleToString $
  flip (foldl (\mod filter -> filter mod)) filters $
  (Gen.tsSourceFileToPsModule moduleName sourceFile)

-- | This is an example PsModule filter function. It takes a function from string to string
-- | and transforms the last type of all functions with that function.
-- | ## Example usage
-- | ```purescript
-- | addEffectFilter = changeFuncReturnTypeWith (\str -> "Effect " <> str)
-- | generatePsFunctionsWithEffectTypes =
-- |    tsSourceFileToPsModuleStringWithFilters "My.Module" mySourceFile [addEffectFilter]
-- | ```
changeFuncReturnTypeWith :: (String -> String) -> PsModuleFilter
changeFuncReturnTypeWith stringTransform = \psModule ->
  psModule {functions = map changeReturnType psModule.functions}
  where
    transformLast { init: init, last: last } = NE.snoc' init (stringTransform last)
    changeReturnType func = func {types = transformLast $ NE.unsnoc func.types}

type GenSourcePair =
  { psString :: String
  , jsString :: String
  }

-- | This function takes the source code, the module name as it will appear in the generated
-- | purescript string (`module My.Module where ..`), and the source library that will go
-- | in the `require("your_required_library")` part of the generated JS code.
tryCreateModulesFromStringSource :: String -> String -> String -> Either String GenSourcePair
tryCreateModulesFromStringSource jsSourceLibrary psModuleName sourceCode = do
  sourceFile <- lmap show $ Parse.tryParseSourceFile sourceCode
  let psModule = Gen.tsSourceFileToPsModule psModuleName sourceFile
  if (A.null psModule.exports) then
    Left "Error: source code produced no exposable functions."
  else
    Right { psString: Gen.psModuleToString psModule
          , jsString: tsSourceFileToJsModuleString jsSourceLibrary sourceFile
          }

doSomething = Parse.test 
