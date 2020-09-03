-- | Similar to how the GenJs module functions, this module works by turning a `SourceFile`
-- | into an "intermediate representation", namely `PsModule`, `PsFunction`, `PsForeignData`,
-- | and `PsType`. `PsModule` contains all the intermediate representations and once converted
-- | to string, will be a PureScript module.
-- | The flow of `SourceFile` to a String that contains the PureScript module is roughly as follows:
-- | ```
-- | SourceFile -> tsSourceFileToPsModule -> PsModule -> psModuleToString -> String
-- | in wich tsSourceFileToPsModule internally calls
-- | tsClassToPsFunctions -> PsFunction
-- | tsMethodToPsFunction -> PsFunction
-- | tsClassToPsForeignData -> PsForeignData
-- | ...[the rest of the ts*ToPs* functions]
-- | and psModuleToString internally calls
-- | psForeignDataToString -> String
-- | psFunctionToImportString -> String
-- | ... [the rest of the ps*To*String functions]
-- | ```
-- | This means that to plug in and create changes in the intermediate step, you use the `ts*ToPs*`
-- | functions, make modifications, and then call the `ps*To*String` functions.
-- | ## Example
-- | This function uses the `changeFuncReturnTypeWith` function to add `Effect` to the last type
-- | of all functions in the PsModule. Replacing `func :: Int -> Int` into `func :: Int -> Effect Int`
-- | ```purescript
-- | sourceToString :: String -> SourceFile -> String
-- | sourceToString moduleName sourceFile =
-- |   psModuleToString $ -- Finally, convert PsModule to String
-- |   changeFuncReturnTypeWith (\str -> "Effect " <> str) $ -- Then apply transformations to PsModule
-- |   tsSourceFileToPsModule moduleName sourceFile -- First, SourceFile -> PsModule
-- |
-- | type PsModuleFilter = PsModule -> PsModule
-- | changeFuncReturnTypeWith :: (String -> String) -> PsModuleFilter
-- | changeFuncReturnTypeWith stringTransform = \psModule ->
-- |   psModule {functions = map changeReturnType psModule.functions}
-- |   where
-- |     transformLast { init: init, last: last } = NE.snoc' init (stringTransform last)
-- |     changeReturnType func = func {types = transformLast $ NE.unsnoc func.types}
-- | ```
-- | Likewise, you are free to change the type names, add functions to the PsModule, remove data types, etc.
module Text.TypeScript.GenForeign.GenPs
       ( PsFunction
       , PsForeignData
       , PsModule
       , PsType
       , tsSourceFileToPsModule
       , tsInterfaceToPsType
       , tsClassToPsForeignData
       , tsMethodToPsFunction
       , tsConstructorToPsFunction
       , tsFunctionToPsFunction
       , tsClassToPsFunctions
       , psModuleToString
       , psTypeToString
       , psForeignDataToString
       , psFunctionToImportString
       , psFunctionToRunFunctionString
       , tsTypesToStrings
       , tsTypeToString
       , tsConstructorTypeToString
       , tsTypeParamToString
       , tsPrimitiveTypeToString
       , tsDocsToString
       )
       where

import Prelude
import Control.MonadZero (guard)
import Data.Boolean 
import Data.Array (uncons, cons, filter, deleteBy, nubByEq)
import Data.Array.NonEmpty as NE
import Data.Foldable (foldMap, intercalate, surroundMap, foldr)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replaceAll, trim, null)
import Text.TypeScript.Type (Class, ClassConstructor, ClassMethod, ConstructorType, PrimitiveTsType(..), SourceFile, TsFunction, TsType(..), TypeParam(..), Interface)
import Prim.Boolean
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

type PsType = { name :: String
              , fields :: Array { name :: String
                                , psType :: String
                                }
              }

tsSourceFileToPsModule :: String -> SourceFile -> PsModule
tsSourceFileToPsModule moduleName sourceFile =
  { name: moduleName
  , imports:  [ "import Foreign"
              , "import Data.Function.Uncurried"
              , "import Data.Maybe"
              ]
  , functions: foldMap tsClassToPsFunctions sourceFile.classes <>
                map tsFunctionToPsFunction sourceFile.functions
  , foreignData: tsSourceFileToPsForeignData sourceFile -- map tsClassToPsForeignData sourceFile.classes
  , interfaces : map tsInterfaceToPsType sourceFile.interfaces
  }


tsSourceFileToPsForeignData :: SourceFile -> Array PsForeignData
tsSourceFileToPsForeignData sourceFile =
  classesForeign <> diff
  where
    paramsToPsForeignData = do
      func <- foldMap (_.methods) sourceFile.classes <>
              foldMap (_.constructors) sourceFile.classes <>
              sourceFile.functions
      tsType <- cons func.tsType $
                map _.tsType func.params
      typeReference <- getAllTypeReferencesForType tsType
      ----  guard $ isTypeReference param.tsType
      pure { name: tsTypeToString typeReference
           , docs: ""
           , psType: "Type"
           }
    classesForeign :: Array PsForeignData
    classesForeign = map tsClassToPsForeignData sourceFile.classes
    diff :: Array PsForeignData
    diff = foldr (deleteBy psEq) (nubByEq psEq paramsToPsForeignData) classesForeign
    psEq = (\a b -> a.name == b.name)
    getAllTypeReferencesForType :: TsType -> Array TsType
    getAllTypeReferencesForType = case _ of
      (CompositeType {typeConstructor: b, typeParams: a}) -> getAllTypeReferencesForTypeParam a
      (PrimitiveType a) -> []
      (TypeReference a) -> [TypeReference a]
    getAllTypeReferencesForTypeParam = case _ of
      SingletonTypeParam c -> getAllTypeReferencesForType c
      ArrayTypeParams c -> foldMap getAllTypeReferencesForType c




tsInterfaceToPsType :: Interface -> PsType
tsInterfaceToPsType tsInterface =
  { name: tsInterface.name
  , fields: map (\param -> {name: param.name, psType: tsTypeToString param.tsType}) tsInterface.properties
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
  , types: tsTypesToStrings $ map (_.tsType) tsFunc.params <> [tsFunc.tsType]
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

psModuleToString :: PsModule -> String
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

psForeignDataToString :: PsForeignData -> String
psForeignDataToString psData =
  psData.docs <> _n <>
  "foreign import data " <> psData.name <> " :: " <> psData.psType

psFunctionToImportString :: PsFunction -> String
psFunctionToImportString func =
  funcDecl <> typeDeclPrefix <> functionConstructor <> types <> _n
  where
    funcDecl = "foreign import " <> func.name <> "Impl"
    typeDeclPrefix = " :: " <> func.typeDeclPrefix
    functionConstructor = "Fn" <> (show $ (NE.length func.types) - 1) <> " "
    types = intercalate " " func.types

psFunctionToRunFunctionString :: PsFunction -> String
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

tsTypeToString :: TsType -> String
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
    filter1 = append "-- | "
    filter2 = replaceAll (Pattern "\n") (Replacement "\n-- | ")
    filter3 = replaceAll (Pattern "-- | \n") (Replacement "")
    filter4 = \str -> if (length str < 6) then "" else str

isTypeReference :: TsType -> Boolean
isTypeReference (TypeReference _) = true
isTypeReference _ = false
