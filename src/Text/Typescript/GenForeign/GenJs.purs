-- | This module works by reading a `SourceFile` and then translating that
-- | into "intermediate representations". Namely, `JsModule`, and `JsFunction`.
-- | Then, it turns that into a string using the `jsFunctionToString` and `jsModuleToString`.
-- | Simply put, the flow looks like the following:
-- | ```
-- |    SourceFile -> tsSourceModuleToJsModule -> JsModule -> jsModuleToString -> String
-- | ```
-- | Which `tsSourceModuleToJsModule` calls the functions that turn the records inside SourceFile into `JsFunction`s
-- | ```
-- |    ts*toJsFunction -> JsFunction
-- | ```
-- | And `jsModuleToString` calls the `jsFunctionToString` to convert each function to string.
-- | This way, you can selectively call the functions and "plug in" functions that transform
-- | `JSModule -> JSModule` or `JSFunction -> JSFunction` before calling the `js*ToString`
-- | functions on them.
-- | ## Example:
-- | ```purescript
-- | sourceToString :: SourceFile -> String
-- | sourceToString sourceFile =
-- |   jsModuleToString $ -- finally, convert the JsModule to string.
-- |   \jsModule -> jsModule {requiredLibrary = "preact"} $ -- changes it to "preact"
-- |   tsSourceModuleToJsModule "react" sourceFile -- sets requiredLibrary to "react"
-- | ```
-- | Likewise, the other records can also be changed, added to, etc.
module Text.TypeScript.GenForeign.GenJs
       ( jsFunctionToString
       , tsConstructorToJsFunction
       , tsMethodToJsFunction
       , tsFunctionToJsFunction
       , tsClassToJsFunctions
       , tsSourceModuleToJsModule
       , jsModuleToString
       , JsModule
       , JsFunction
       )
       where

import Prelude

import Data.Array (cons)
import Data.Foldable (foldMap, intercalate)
import Text.TypeScript.Type (Class, ClassConstructor, ClassMethod, TsFunction, SourceFile)

_n :: String
_n = "\n"
tab :: String
tab = "\t"


-- Javascript source generation
----------------------------------------------------------------------------
-- | A record that holds the name of the library being called
-- | (used as `require('your_required_library')`) and an array of `JsFunction`.
type JsModule =
  { requiredLibrary :: String
  , functions :: Array JsFunction
  }

-- | Type that represents the information that the printer will use to print an
-- | exported function.
-- |
-- | ## General idea of how they will be applied
-- | ```javascript
-- | exports.outerFuncName = function(outerParamName1, outerParamName2, ..){
-- |   return expressionPrefixinnerFuncName(innerParamName1, innerParamName2, ..)
-- | }
-- | ```
type JsFunction =
  { outerFuncName :: String
  , innerFuncName :: String
  , outerParamNames :: Array String
  , innerParamNames :: Array String
  , expressionPrefix :: String
  }


jsFunctionToString :: JsFunction -> String
jsFunctionToString func =
  "exports." <> func.outerFuncName <> " = function(" <> commas func.outerParamNames <> "){" <> _n <>
  tab <> "return " <> returnExpression <> ";" <> _n <>
  "};" <> _n
  where
    returnExpression = func.expressionPrefix <> func.innerFuncName <> "(" <> commas func.innerParamNames <> ")"
    commas = intercalate ", "

tsConstructorToJsFunction :: ClassConstructor -> JsFunction
tsConstructorToJsFunction tsCons =
  let
    paramNames = (map _.name tsCons.params)
  in
  { outerFuncName: "constructor" <> tsCons.name <> "Impl"
  , innerFuncName: tsCons.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "new sourceModule."
  }

tsMethodToJsFunction :: ClassMethod -> JsFunction
tsMethodToJsFunction tsMethod =
  let
    paramNames = (map _.name tsMethod.params)
  in
  { outerFuncName: tsMethod.name <> "Impl"
  , innerFuncName: tsMethod.name
  , outerParamNames: cons "classInstance" paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "classInstance."
  }

tsFunctionToJsFunction :: TsFunction -> JsFunction
tsFunctionToJsFunction tsFunc =
  let
    paramNames = (map _.name tsFunc.params)
  in
  { outerFuncName: tsFunc.name <> "Impl"
  , innerFuncName: tsFunc.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "sourceModule."
  }

-- | Turns methods and constructors of a class into an array of `JsFunction`
tsClassToJsFunctions :: Class -> Array JsFunction
tsClassToJsFunctions tsClass =
  (map tsMethodToJsFunction tsClass.methods) <>
  (map tsConstructorToJsFunction tsClass.constructors)

tsSourceModuleToJsModule :: String -> SourceFile -> JsModule
tsSourceModuleToJsModule requiredLibrary sourceFile =
  { requiredLibrary: requiredLibrary
  , functions: foldMap tsClassToJsFunctions sourceFile.classes <>
                map tsFunctionToJsFunction sourceFile.functions
  }

jsModuleToString :: JsModule -> String
jsModuleToString fm =
  useStrict <> _n <> _n <>
  requires <> _n <> _n <>
  functions <> _n
   where
     useStrict = "'use strict';"
     requires = "const sourceModule = require('" <> fm.requiredLibrary <> "');"
     functions = foldMap jsFunctionToString fm.functions

