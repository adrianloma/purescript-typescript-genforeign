module Text.TypeScript.GenForeign.GenJs where

import Data.Array (cons)
import Data.Foldable (foldMap, intercalate)
import Prelude
import Text.TypeScript.Type (Class, ClassConstructor, ClassMethod, SourceFile, TsFunction)


type JsString = String

_n :: String
_n = "\n"
tab :: String
tab = "\t"


-- Javascript source generation
----------------------------------------------------------------------------
type JsModule =
  { requiredModule :: String
  , functions :: Array JsFunction
  }

type JsFunction =
  { name :: String
  , outerParamNames :: Array String
  , innerParamNames :: Array String
  , expressionPrefix :: String
  }


jsFunctionToString :: JsFunction -> JsString
jsFunctionToString func =
  "exports." <> func.name <> "Impl = function(" <> commas func.outerParamNames <> "){" <> _n <>
  tab <> "return " <> returnExpression <> ";" <> _n <>
  "}" <> _n
  where
    returnExpression = func.expressionPrefix <> func.name <> "(" <> commas func.innerParamNames <> ")"
    commas = intercalate ", "

tsConstructorToJsFunction :: ClassConstructor -> JsFunction
tsConstructorToJsFunction tsCons =
  let
    paramNames = (map _.name tsCons.params)
  in
  { name: "constructor" <> tsCons.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "new sourceModule."
  }

tsMethodToJsFunction :: ClassMethod -> JsFunction
tsMethodToJsFunction tsMethod =
  let
    paramNames = (map _.name tsMethod.params)
  in
  { name: tsMethod.name
  , outerParamNames: cons "classInstance" paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "classInstance."
  }

tsFunctionToJsFunction :: TsFunction -> JsFunction
tsFunctionToJsFunction tsFunc =
  let
    paramNames = (map _.name tsFunc.params)
  in
  { name: tsFunc.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "sourceModule."
  }

tsClassToJsFunctions :: Class -> Array JsFunction
tsClassToJsFunctions tsClass =
  (map tsMethodToJsFunction tsClass.methods) <>
  (map tsConstructorToJsFunction tsClass.constructors)

tsSourceModuleToJsModule :: String -> SourceFile -> JsModule
tsSourceModuleToJsModule requiredModule sourceFile =
  { requiredModule: requiredModule
  , functions: foldMap tsClassToJsFunctions sourceFile.classes <>
                map tsFunctionToJsFunction sourceFile.functions
  }

jsModuleToString :: JsModule -> JsString
jsModuleToString fm =
  useStrict <> _n <> _n <>
  requires <> _n <> _n <>
  functions <> _n
   where
     useStrict = "'use strict';"
     requires = "const sourceModule = require('" <> fm.requiredModule <> "');"
     functions = foldMap jsFunctionToString fm.functions

