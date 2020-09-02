module Text.TypeScript.GenForeign.GenPs where

import Prelude

import Data.Array (uncons)
import Data.Array.NonEmpty as NE
import Data.Foldable (foldMap, intercalate, surroundMap)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replaceAll, trim, null)
import Text.TypeScript.Type (Class, ClassConstructor, ClassMethod, ConstructorType, PrimitiveTsType(..), SourceFile, TsFunction, TsType(..), TypeParam(..), Interface)

type PsString = String

_n :: String
_n = "\n"


-- Purescript source generation
----------------------------------------------------------------------------

type PsFunction =
  { name :: String
  , typeDeclPrefix :: String
  , types :: NE.NonEmptyArray String
  , docs :: String
  }

type PsForeignData =
  { name :: String
  , docs :: String
  , psType :: String
  }

type PsModule =
  { name :: String
  , imports :: Array String
  , functions :: Array PsFunction
  , foreignData :: Array PsForeignData
  , interfaces :: Array PsType
  }


tsSourceFileToPsModule :: String -> SourceFile -> PsModule
tsSourceFileToPsModule moduleName sourceFile =
  { name: moduleName
  , imports:  ["import Foreign", "import Data.Function.Uncurried"]
  , functions: foldMap tsClassToPsFunctions sourceFile.classes <>
                map tsFunctionToPsFunction sourceFile.functions
  , foreignData: map tsClassToPsForeignData sourceFile.classes
  , interfaces : map tsInterfaceToPsType sourceFile.interfaces
  }

tsInterfaceToPsType :: Interface -> PsType
tsInterfaceToPsType tsInterface =
  { name: tsInterface.name
  , fields: map (\param -> {name: param.name, psType: tsTypeToString param.tsType}) tsInterface.properties
  }
type PsType = { name :: String
              , fields :: Array { name :: String
                                , psType :: String
                                }
              }


tsClassToPsForeignData :: Class -> PsForeignData
tsClassToPsForeignData tsClass =
  { name: tsClass.name
  , docs: tsDocsToString tsClass.docs
  , psType : "Type"
  }

tsMethodToPsFunction :: Class -> ClassMethod -> PsFunction
tsMethodToPsFunction tsClass tsMethod =
  let
   methodPs = tsFunctionToPsFunction tsMethod
  in
   methodPs {types = NE.cons tsClass.name methodPs.types}

tsConstructorToPsFunction :: Class -> ClassConstructor -> PsFunction
tsConstructorToPsFunction tsClass tsConstructor =
  let
   constructorPs = tsFunctionToPsFunction tsConstructor
  in
   constructorPs { name = "constructor" <> constructorPs.name
                 , types = NE.snoc constructorPs.types tsClass.name
                 }

tsFunctionToPsFunction :: TsFunction -> PsFunction
tsFunctionToPsFunction tsFunc =
  { name: tsFunc.name
  , typeDeclPrefix: ""
  , types: tsTypesToStrings $ map (_.tsType) tsFunc.params
  , docs: tsDocsToString $ tsFunc.docs <> surroundMap _n getParamDocs tsFunc.params
  }
  where
   getParamDocs param = "Param" <> ": `" <> param.name <> "`" <>
                        (if null param.docs then "" else ": " ) <> param.docs 


tsClassToPsFunctions :: Class -> Array PsFunction
tsClassToPsFunctions tsClass =
  map (tsMethodToPsFunction tsClass) tsClass.methods <>
  map (tsConstructorToPsFunction tsClass) tsClass.constructors

{--
 Ps* -> String
--}

psModuleToString :: PsModule -> PsString
psModuleToString psModule =
  "module " <> psModule.name <> _n <>
  "  ( " <> exports <> "\n  )" <> _n <>
  "  where" <> _n <> _n <>
  intercalate _n psModule.imports <> _n <> _n <>
  foreignFunctionsImports <> _n <>
  runFunctions <> _n <>
  foreignData <> _n <>
  interfaces <> _n
  where
    exports = intercalate "\n  , " $
              map _.name psModule.functions <>
              map _.name psModule.foreignData <>
              map _.name psModule.interfaces
    foreignFunctionsImports = foldMap psFunctionToImportString psModule.functions
    runFunctions = intercalate _n $ map psFunctionToRunFunctionString psModule.functions
    foreignData = surroundMap _n psForeignDataToString psModule.foreignData
    interfaces = foldMap psTypeToString psModule.interfaces

psTypeToString :: PsType -> String
psTypeToString psType =
  "type " <> psType.name <> " =\n" <>
  "  { " <> intercalate "\n  , " (map fieldToString psType.fields) <> "\n  }"
  where
    fieldToString field = field.name <> " :: " <> field.psType

psForeignDataToString :: PsForeignData -> PsString
psForeignDataToString psData =
  psData.docs <> _n <>
  "foreign import data " <> psData.name <> " :: " <> psData.psType

psFunctionToImportString :: PsFunction -> PsString
psFunctionToImportString func =
  funcDecl <> typeDeclPrefix <> functionConstructor <> types <> _n
  where
    funcDecl = "foreign import " <> func.name <> "Impl"
    typeDeclPrefix = " :: " <> func.typeDeclPrefix
    functionConstructor = "Fn" <> (show $ (NE.length func.types) - 1) <> " "
    types = intercalate " " func.types

psFunctionToRunFunctionString :: PsFunction -> PsString
psFunctionToRunFunctionString func =
  func.docs <> _n <>
  func.name <> " :: " <> func.typeDeclPrefix <> types <> _n <>
  func.name <> " = " <> runFn <> " " <> func.name <> "Impl" <> _n
  where
    types = intercalate " -> " func.types
    runFn = "runFn" <> (show $ (NE.length func.types) - 1)

{--
Ts components to string
--}
tsTypesToStrings :: Array TsType -> NE.NonEmptyArray String
tsTypesToStrings types = case uncons types of
  Just { head: x, tail: xs } -> map tsTypeToString $ NE.appendArray (NE.singleton x) xs
  Nothing -> map tsTypeToString $ NE.singleton (PrimitiveType TsVoid)

tsTypeToString :: TsType -> PsString
tsTypeToString = case _ of
  (CompositeType a) -> tsConstructorTypeToString a
  (PrimitiveType a) -> tsPrimitiveTypeToString a
  (TypeReference a) -> a

tsConstructorTypeToString :: ConstructorType -> String
tsConstructorTypeToString tsType = "(" <> tsType.typeConstructor <> " " <> tsTypeParamToString tsType.typeParams <> ")"

tsTypeParamToString :: TypeParam -> String
tsTypeParamToString = case _ of
  (SingletonTypeParam b) -> tsTypeToString b
  (ArrayTypeParams b) -> intercalate " " $ map tsTypeToString b

tsPrimitiveTypeToString :: PrimitiveTsType -> String
tsPrimitiveTypeToString = case _ of
    TsAny   -> "Foreign"
    TsNumber   -> "Number"
    TsVoid   ->  "Unit"
    TsString   -> "String"
    TsUndefined -> "Unit"
    TsObject -> "Foreign"
    TsBoolean -> "Boolean"

tsDocsToString :: String -> String
tsDocsToString =
  filter4 <<< filter3 <<< filter2 <<< filter1 <<< trim
  where
    filter1 = append "--| "
    filter2 = replaceAll (Pattern "\n") (Replacement "\n--| ")
    filter3 = replaceAll (Pattern "--| \n") (Replacement "")
    filter4 = \str -> if (length str < 5) then "" else str

