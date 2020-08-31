module Text.TypeScript.GenForeign where

import Data.Argonaut
import Data.Array
import Data.Foldable hiding (length)
import Data.Semigroup
import Prelude
import Text.TypeScript.Type

type JsString = String
type PsString = String

genFfiFunctionJs :: TSFunction -> JsString
genFfiFunctionJs tsFunc =
  "exports." <> tsFunc.name <> params <> "{" <> _n <>
  tab <> "return sourceModule." <> tsFunc.name <> params <> ";" <> _n <>
  "}" <> _n
  where
    params = "(" <> interName tsFunc.parameters <> ")"

genFfiClassMethodsJs :: Class -> JsString
genFfiClassMethodsJs tsClass =
  fold $ map genFfiMethodJs tsClass.methods

genFfiMethodJs :: ClassMethod -> JsString
genFfiMethodJs tsMethod =
  "exports." <> tsMethod.name <> paramsWithInstance <> "{" <> _n <>
  tab <> "return classInstance." <> tsMethod.name <> params <> ";" <> _n <>
  "}" <> _n <> _n
  where
    paramsWithInstance =
      "(" <> intercalate ", " ( cons "classInstance" (map _.name tsMethod.parameters)) <> ")"
    params = "(" <>  interName tsMethod.parameters <> ")"

interName :: forall r. Array { name :: String | r } -> JsString
interName parameters = intercalate (", ") (map _.name parameters)

genModuleJs :: String -> SourceFile -> JsString
genModuleJs originModule sourceFile =
  useStrict <> _n <> _n <>
  requires <> _n <> _n <>
  functions <> _n <>
  classes <> _n
   where
     useStrict = "'use strict';"
     requires = "const sourceModule = require('" <> originModule <> "');"
     functions = fold $ map genFfiFunctionJs sourceFile.functions
     classes = fold $ map genFfiClassMethodsJs sourceFile.classes

_n :: String
_n = "\n"
tab :: String
tab = "\t"

type FunctionPair =
  { ffi :: String
  , runFn :: String
  }
  
genFfiFunctionPs :: String -> Array TSType -> JsString
genFfiFunctionPs functionName [] = genFfiFunctionPs functionName [PrimitiveType TSVoid]
genFfiFunctionPs functionName tsTypes =
  funcDecl <> " :: " <> functionKind <> " " <> psTypesAsString  <> _n
  where
    funcDecl = "foreign import " <> functionName <> "Impl"
    functionKind = "Fn" <> (show $ (length tsTypes) - 1)
    psTypes = map tsTypeToPsType tsTypes
    psTypesAsString = intercalate " " psTypes

genRunFunctionPs :: String -> Array TSType -> JsString
genRunFunctionPs functionName [] = genRunFunctionPs functionName [PrimitiveType TSVoid]
genRunFunctionPs functionName tsTypes =
  functionName <> " :: " <> psTypesAsString <> _n <>
  functionName <> " = " <> runFn <> " " <> functionName <> "Impl" <> _n
  where
    psTypes = map tsTypeToPsType tsTypes
    psTypesAsString = intercalate " -> " psTypes
    runFn = "runFn" <> (show $ (length tsTypes) - 1)

genFunctionPs :: String -> Array TSType -> FunctionPair
genFunctionPs functionName tsTypes =
  { ffi: genFfiFunctionPs functionName tsTypes
  , runFn: genRunFunctionPs functionName tsTypes
  }

genClassPs :: Class -> Array FunctionPair
genClassPs tsClass =
  map (genMethodPs tsClass) tsClass.methods <>
  map (genConstructorPs tsClass) tsClass.constructors

genConstructorPs:: Class -> ClassConstructor -> FunctionPair
genConstructorPs tsClass tsConstructor =
  genFunctionPs ("constructor" <> tsConstructor.name) constructorTypes
  where
    -- The last paramter of class constructors is the instance of the class.
    constructorTypes = snoc (map _.typeScriptType tsConstructor.parameters) (TypeReference tsClass.name)

genMethodPs:: Class -> ClassMethod -> FunctionPair
genMethodPs tsClass tsMethod =
  genFunctionPs tsMethod.name methodTypes
  where
    -- The first paramter of class methods is the instance of the class.
    -- The last parameter is the return type,
    methodTypes = snoc (TypeReference tsClass.name : (map _.typeScriptType tsMethod.parameters)) tsMethod.typeScriptType


genModulePs :: String -> SourceFile -> PsString
genModulePs psModuleName sourceFile =
  "module " <> psModuleName <> _n <>
  exports <> _n <>
  imports <> _n <> _n <>
  foldMap (_.ffi) functions <> _n <>
  foldMap (_.ffi) classes <> _n <> _n <>
  intercalate _n (map (_.runFn) functions) <> _n <>
  intercalate _n (map (_.runFn) classes) <> _n
  where
    exports = "  (" <> intercalate "\n  , " (getExportedFunctionsPs sourceFile) <> "\n  )\n  where"
    imports = "import Foreign" <> "import Data.Function.Uncurried"
    functions = genFunctionsPs sourceFile.functions
    classes = join $ map genClassPs sourceFile.classes

genFunctionsPs :: Array TSFunction -> Array FunctionPair
genFunctionsPs = map (\func -> genFunctionPs func.name (getParamTypes func))
  where
    getParamTypes func = map _.typeScriptType func.parameters

getExportedFunctionsPs :: SourceFile -> Array String
getExportedFunctionsPs sourceFile =
  functions <> methods <> constructors
  where
    functions = map _.name sourceFile.functions
    methods = map _.name $ foldMap _.methods sourceFile.classes
    constructors = map ((<>) "constructor" <<< _.name) $ foldMap _.constructors sourceFile.classes

tsTypeToPsType :: TSType -> PsString
tsTypeToPsType = case _ of
  (CompositeType a) -> tsConstructorTypeToPs a
  (PrimitiveType a) -> tsPrimitiveTypeToPs a
  (TypeReference a) -> a

tsConstructorTypeToPs a = "(" <> a.typeConstructor <> " " <> tsTypeParameterToPs a.typeParameters <> ")"

tsTypeParameterToPs = case _ of
  (SingletonTypeParameter b) -> tsTypeToPsType b
  (ArrayTypeParameters b) -> intercalate " " $ map tsTypeToPsType b

tsPrimitiveTypeToPs = case _ of
    TSAny   -> "Foreign"
    TSNumber   -> "Number"
    TSVoid   ->  "Unit"
    TSString   -> "String"
    TSUndefined -> "Unit"
    TSObject -> "Foreign"
    TSBoolean -> "Boolean"

