module Text.TypeScript.GenForeign.GenPs where

import Data.Argonaut
import Data.Array
import Data.Foldable hiding (length)
import Data.Maybe
import Data.Semigroup
import Prelude
import Text.TypeScript.Type

import Data.Array.NonEmpty as NE
import Data.String (Pattern(..), Replacement(..), length, null)
import Data.String (replaceAll)

type JsString = String
type PsString = String

_n :: String
_n = "\n"
tab :: String
tab = "\t"


-- Purescript source generation
----------------------------------------------------------------------------

type PsFunction =
  { name :: String
  , typeDeclPrefix :: String
  , types :: NE.NonEmptyArray String
  , documentation :: String
  }

type PsForeignData =
  { name :: String
  , documentation :: String
  , psType :: String
  }

type PSFfiModule =
  { name :: String
  , imports :: Array String
  , functions :: Array PsFunction
  , foreignData :: Array PsForeignData
  }

genPsModuleFromSourceFile :: String -> SourceFile -> PsString
genPsModuleFromSourceFile moduleName sourceFile = genModulePs $ sourceFileToModulePs moduleName sourceFile

sourceFileToModulePs :: String -> SourceFile -> PSFfiModule
sourceFileToModulePs moduleName sourceFile =
  { name: moduleName
  , imports:  ["import Foreign", "import Data.Function.Uncurried"]
  , functions: foldMap classToPs sourceFile.classes <>
                map functionToPs sourceFile.functions
  , foreignData: map classToPsForeignData sourceFile.classes
  }

classToPsForeignData :: Class -> PsForeignData
classToPsForeignData tsClass =
  { name: tsClass.name
  , documentation: tsDocumentationToPs tsClass.documentation
  , psType : "Type"
  }

methodToPs :: Class -> ClassMethod -> PsFunction
methodToPs tsClass tsMethod =
  let
   methodPs = functionToPs tsMethod
  in
   methodPs {types = NE.cons tsClass.name methodPs.types}

constructorToPs :: Class -> ClassConstructor -> PsFunction
constructorToPs tsClass tsConstructor =
  let
   constructorPs = functionToPs tsConstructor
  in
   constructorPs { name = "constructor" <> constructorPs.name
                 , types = NE.snoc constructorPs.types tsClass.name
                 }

functionToPs :: TSFunction -> PsFunction
functionToPs tsFunc =
  { name: tsFunc.name
  , typeDeclPrefix: ""
  , types: convertTsTypesToPsTypes $ map (_.typeScriptType) tsFunc.parameters
  , documentation: tsDocumentationToPs $ tsFunc.documentation <> surroundMap _n getParamDocs tsFunc.parameters
  }
  where
   getParamDocs param = "Parameter" <> ": `" <> param.name <> "`" <>
                        (if null param.documentation then "" else ": " ) <> param.documentation 


classToPs :: Class -> Array PsFunction
classToPs tsClass =
  map (methodToPs tsClass) tsClass.methods <>
  map (constructorToPs tsClass) tsClass.constructors


genModulePs :: PSFfiModule -> PsString
genModulePs psModule =
  "module " <> psModule.name <> _n <>
  "  ( " <> exports <> "\n  )" <> _n <>
  "  where" <> _n <> _n <>
  intercalate _n psModule.imports <> _n <> _n <>
  foreignFunctionsImports <> _n <>
  runFunctions <> _n <>
  foreignData <> _n
  where
    exports = intercalate "\n  , " $ map _.name psModule.functions
    foreignFunctionsImports = foldMap genImportFunctionPs psModule.functions
    runFunctions = intercalate _n $ map genRunFunctionPs psModule.functions
    foreignData = foldMap psForeignDataToString psModule.foreignData

psForeignDataToString :: PsForeignData -> PsString
psForeignDataToString psData =
  psData.documentation <> _n <>
  "foreign import data " <> psData.name <> " :: " <> psData.psType

genImportFunctionPs :: PsFunction -> PsString
genImportFunctionPs func =
  funcDecl <> typeDeclPrefix <> functionConstructor <> types <> _n
  where
    funcDecl = "foreign import " <> func.name <> "Impl"
    typeDeclPrefix = " :: " <> func.typeDeclPrefix
    functionConstructor = "Fn" <> (show $ (NE.length func.types) - 1) <> " "
    types = intercalate " " func.types

genRunFunctionPs :: PsFunction -> PsString
genRunFunctionPs func =
  func.documentation <> _n <>
  func.name <> " :: " <> func.typeDeclPrefix <> types <> _n <>
  func.name <> " = " <> runFn <> " " <> func.name <> "Impl" <> _n
  where
    types = intercalate " -> " func.types
    runFn = "runFn" <> (show $ (NE.length func.types) - 1)

convertTsTypesToPsTypes :: Array TSType -> NE.NonEmptyArray String
convertTsTypesToPsTypes types = case uncons types of
  Just { head: x, tail: xs } -> map tsTypeToPsType $ NE.appendArray (NE.singleton x) xs
  Nothing -> map tsTypeToPsType $ NE.singleton (PrimitiveType TSVoid)

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

tsDocumentationToPs :: String -> String
tsDocumentationToPs = 
  filter3 <<< filter2 <<< filter1
  where
    filter1 = append "--| "
    filter2 = replaceAll (Pattern "\n") (Replacement "\n--| ")
    filter3 = replaceAll (Pattern "--| \n") (Replacement "")
