module Text.TypeScript.GenForeign.GenJs where

import Data.Argonaut
import Data.Array
import Data.Foldable hiding (length)
import Data.Maybe
import Data.Semigroup
import Prelude
import Text.TypeScript.Type

import Data.Array.NonEmpty as NE

type JsString = String
type PsString = String

_n :: String
_n = "\n"
tab :: String
tab = "\t"


-- Javascript source generation
----------------------------------------------------------------------------
type JSForeignModule =
  { requiredModule :: String
  , functions :: Array JSForeignFunction
  }

type JSForeignFunction =
  { name :: String
  , outerParamNames :: Array String
  , innerParamNames :: Array String
  , expressionPrefix :: String
  }


makeFFJs :: JSForeignFunction -> JsString
makeFFJs func =
  "exports." <> func.name <> "Impl = function(" <> commas func.outerParamNames <> "){" <> _n <>
  tab <> "return " <> returnExpression <> ";" <> _n <>
  "}" <> _n
  where
    returnExpression = func.expressionPrefix <> func.name <> "(" <> commas func.innerParamNames <> ")"
    commas = intercalate ", "

constructorToFF :: ClassConstructor -> JSForeignFunction
constructorToFF tsCons =
  let
    paramNames = (map _.name tsCons.parameters)
  in
  { name: "constructor" <> tsCons.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "new sourceModule."
  }

methodToFF :: ClassMethod -> JSForeignFunction
methodToFF tsMethod =
  let
    paramNames = (map _.name tsMethod.parameters)
  in
  { name: tsMethod.name
  , outerParamNames: cons "classInstance" paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "classInstance."
  }

tsFunctionToFF :: TSFunction -> JSForeignFunction
tsFunctionToFF tsFunc =
  let
    paramNames = (map _.name tsFunc.parameters)
  in
  { name: tsFunc.name
  , outerParamNames: paramNames
  , innerParamNames: paramNames
  , expressionPrefix: "sourceModule."
  }

tsClassToFF :: Class -> Array JSForeignFunction
tsClassToFF tsClass =
  (map methodToFF tsClass.methods) <>
  (map constructorToFF tsClass.constructors)

sourceFileToFModule :: String -> SourceFile -> JSForeignModule
sourceFileToFModule requiredModule sourceFile =
  { requiredModule: requiredModule
  , functions: foldMap tsClassToFF sourceFile.classes <>
                map tsFunctionToFF sourceFile.functions
  }

genModuleJs :: JSForeignModule -> JsString
genModuleJs fm =
  useStrict <> _n <> _n <>
  requires <> _n <> _n <>
  functions <> _n
   where
     useStrict = "'use strict';"
     requires = "const sourceModule = require('" <> fm.requiredModule <> "');"
     functions = foldMap makeFFJs fm.functions

genJsModuleFromSourceFile :: String -> SourceFile -> JsString
genJsModuleFromSourceFile sourceOrigin sourceFile = genModuleJs $ sourceFileToFModule sourceOrigin sourceFile
