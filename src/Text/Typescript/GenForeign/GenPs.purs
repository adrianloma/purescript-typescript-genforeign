-- | Similar to how the GenJs module functions, this module works by turning a `SourceFile`
-- | into an "intermediate representation", namely `PsModule`, `PsFunction`, `PsForeignData`,
-- | and `PsRecord`. `PsModule` contains all the intermediate representations and once converted
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
       , PsRecord
       , PsUnionType
       , PsTypeWrapper
       , tsSourceFileToPsModule
       , tsInterfaceToPsRecord
       , tsClassToPsForeignData
       , tsMethodToPsFunction
       , tsConstructorToPsFunction
       , tsFunctionToPsFunction
       , tsClassToPsFunctions
       , psModuleToString
       , psRecordToString
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

import Data.Boolean
import Prelude
import Text.TypeScript.Type

import Control.MonadZero (guard)
import Data.Array (cons, deleteBy, filter, intercalate, nub, nubByEq, uncons)
import Data.Array as A
import Data.Array.NonEmpty as NE
import Data.Foldable (foldMap, intercalate, surroundMap, foldr, fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replaceAll, trim, null, splitAt, toUpper, toLower)

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
  , exports :: Array String
  , imports :: Array String
  , functions :: Array PsFunction
  , foreignData :: Array PsForeignData
  , interfaces :: Array PsRecord
  , unionTypes :: Array PsUnionType
  , typeWrappers :: Array PsTypeWrapper
  }

type PsTypeWrapper = {name :: String}

type PsUnionType =
  { name :: String
  , unionTypes :: NE.NonEmptyArray String
  }

type PsRecord = { name :: String
              , fields :: Array { name :: String
                                , psType :: String
                                }
              }

tsSourceFileToPsModule :: String -> SourceFile -> PsModule
tsSourceFileToPsModule moduleName sourceFile =
  { name: moduleName
  , exports: map _.name functions <>
              map _.name foreignData <>
              map _.name interfaces <>
              map _.name unionTypes <>
              map _.name typeWrappers
  , imports:  [ "import Foreign"
              , "import Data.Function.Uncurried"
              , "import Data.Maybe"
              , "import Prelude"
              , "import Untagged.Union"
              , "import Effect"
              ]
  , functions : functions
  , foreignData : foreignData
  , interfaces  : interfaces
  , unionTypes  : unionTypes
  , typeWrappers : typeWrappers
  }
  where
    functions = nubByEq (\a b -> a.name == b.name) $ 
      foldMap tsClassToPsFunctions sourceFile.classes <>
                  map tsFunctionToPsFunction sourceFile.functions
    foreignData = tsSourceFileToPsForeignData sourceFile
    interfaces = map tsInterfaceToPsRecord sourceFile.interfaces
    unionTypes = nub $ map tsUnionTypeToPsUnionType $
                 A.mapMaybe getUnionTypeFromTsType $
                 getAllTypesInSourceFile sourceFile
    typeWrappers = nub $ map tsConstructorTypeToPsTypeWrapper $
                    differenceBy (\a b -> a.name /= b.typeConstructor) foreignData $
                    A.mapMaybe getConstructorTypeFromTsType $
                    getAllTypesInSourceFile sourceFile

tsConstructorTypeToPsTypeWrapper tsConstructor = {name: tsConstructor.typeConstructor}


getConstructorTypeFromTsType (CompositeType a) =
  if (a.typeConstructor == "Array" || a.typeConstructor == "Maybe") then
    Nothing
  else
    Just a
getConstructorTypeFromTsType _ = Nothing

getUnionTypeFromTsType (TsTypeUnionType a) = Just a
getUnionTypeFromTsType _ = Nothing

tsUnionTypeToPsUnionType :: UnionType -> PsUnionType
tsUnionTypeToPsUnionType tsUnionType =
      { name: tsUnionTypeToString tsUnionType
      , unionTypes : tsTypesToStrings tsUnionType.unionTypes
      }

getAllTypesInSourceFile :: SourceFile -> Array TsType
getAllTypesInSourceFile sourceFile = do
    let funcs = foldMap (_.methods) sourceFile.classes <>
                foldMap (_.constructors) sourceFile.classes <>
                sourceFile.functions
    let funcTypes = foldMap (\func -> cons func.tsType $ map _.tsType func.params) funcs
    let interfaceTypes = map _.tsType $ foldMap _.properties sourceFile.interfaces
    tsType <- funcTypes <> interfaceTypes
    getAllSubTypes tsType
  where
    getAllSubTypes aType =
      let
        subTypes :: Array TsType
        subTypes = case aType of
          (CompositeType {typeConstructor: b, typeParams: a}) -> case a of
                                        SingletonTypeParam c -> getAllSubTypes c
                                        ArrayTypeParams c -> foldMap getAllSubTypes c
          (TsTypeUnionType unionType) -> foldMap getAllSubTypes unionType.unionTypes
          (TsTypeTypeLiteral typeLiteral) -> foldMap getAllSubTypes
                                            $ map _.tsType typeLiteral.properties
          (TsTypeFunctionType a) -> foldMap getAllSubTypes
                                            (cons a.tsType (map _.tsType a.params))
          (PrimitiveType a) -> []
          (TypeReference a) -> []
      in
        cons aType subTypes

tsSourceFileToPsForeignData :: SourceFile -> Array PsForeignData
tsSourceFileToPsForeignData sourceFile =
  classesForeign <> diff paramsToPsForeignData
  where
    paramsToPsForeignData = do
      tsType <- filter isTypeReference $ getAllTypesInSourceFile sourceFile
      pure { name: tsTypeToString tsType
           , docs: ""
           , psType: "Type" -- Make references which are not declared in this module foreign
           }

 
    classesForeign :: Array PsForeignData
    classesForeign = map tsClassToPsForeignData sourceFile.classes


    psEq :: forall a b. {name :: String | a} -> {name :: String | b} -> Boolean
    psEq = (\a b -> a.name == b.name)

    diff :: Array PsForeignData -> Array PsForeignData
    diff = differenceBy (not <<< psEq) classesForeign <<< -- class declaration has documentation.
           differenceBy (not <<< psEq) sourceFile.interfaces <<< -- interfaces will be declared as Records
           nubByEq psEq -- nub duplicates

    
differenceBy :: forall a b. (a -> b -> Boolean) -> Array a -> Array b -> Array b
differenceBy predicate filterList toFilter =
  foldr (\a -> filter (predicate a)) toFilter filterList

tsInterfaceToPsRecord :: Interface -> PsRecord
tsInterfaceToPsRecord tsInterface =
  { name: tsInterface.name
  , fields: map (\param -> { name: lowerCaseFirst param.name
                           , psType: tsTypeToString param.tsType
                           }
                ) tsInterface.properties
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
tsConstructorToPsFunction tsClass tsConstructor = tsFunctionToPsFunction tsConstructor

tsFunctionToPsFunction :: TsFunction -> PsFunction
tsFunctionToPsFunction tsFunc =
  { name: lowerCaseFirst tsFunc.name
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
  interfaces <> _n <>
  unionTypes <> _n <>
  typeWrappers
  where
    exports = intercalate "\n  , " psModule.exports
    foreignFunctionsImports = foldMap psFunctionToImportString psModule.functions
    runFunctions = intercalate _n $ map psFunctionToRunFunctionString psModule.functions
    foreignData = surroundMap _n psForeignDataToString psModule.foreignData
    interfaces = intercalate _n $ map psRecordToString psModule.interfaces
    unionTypes = intercalate _n $ map psUnionTypeToString psModule.unionTypes
    typeWrappers = intercalate _n $ map psTypeWrappersToString psModule.typeWrappers

psTypeWrappersToString :: PsTypeWrapper -> String
psTypeWrappersToString tWrapper =
  "type " <> tWrapper.name <> " a = a"

psUnionTypeToString :: PsUnionType -> String
psUnionTypeToString unionType =
  "type " <> unionType.name <> " = " <> intercalate " |+| " unionType.unionTypes <> _n

psRecordToString :: PsRecord -> String
psRecordToString psType =
  "type " <> psType.name <> " =\n" <>
  "  { " <> intercalate "\n  , " (map fieldToString psType.fields) <> "\n  }" <> _n
  where
    fieldToString field = field.name <> " :: " <> field.psType

psForeignDataToString :: PsForeignData -> String
psForeignDataToString psData =
  psData.docs <> _n <>
  "type " <> psData.name <> " = Foreign"
  -- Alternative implementation:
  -- "foreign import data " <> psData.name <> " :: " <> psData.psType

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
  (TsTypeUnionType a) -> tsUnionTypeToString a
  (TsTypeTypeLiteral a) -> tsTypeLiteralToString a
  (PrimitiveType a) -> tsPrimitiveTypeToString a
  (TsTypeFunctionType a) -> tsFunctionTypeToString a
  (TypeReference a) -> a

tsFunctionTypeToString :: FunctionType -> String
tsFunctionTypeToString tsFunction =
  let
    typesAsString = (tsTypesToStrings (map _.tsType tsFunction.params))
  in
    "(Fn" <> show (NE.length typesAsString) <>
    " " <>
    intercalate " " typesAsString <>
    " (Effect " <> tsTypeToString tsFunction.tsType <> ")" <>
    ")"

-- This is an alternative implementation to using row types directly in type signatures
-- -- | This function turns a TypeLiteral into a string that represents its
-- -- | type signature in PureScript. This is not the actual type because type
-- -- | literals are like ad-hoc interfaces (like defining a record type in a function signature)
-- tsTypeLiteralToTypeNameString typeLiteral = "TL_" <>
--                                     upperCaseFirst typeLiteral.name <>
--                                     "_" <>
--                                     (squishTypesToString $ map _.tsType typeLiteral.properties)

tsTypeLiteralToString typeLiteral =
  " { " <> intercalate ", " (map fieldToString psType.fields) <> " }"
    where
      fieldToString field = field.name <> " :: " <> field.psType
      psType = tsInterfaceToPsRecord typeLiteral

-- | Makes the first letter of a string upper case
changeFirstLetter :: (String -> String) -> String -> String
changeFirstLetter transformation =
  joinFirstLetterWithRest <<< applyTransformationToFirstLetter <<< splitFirstLetterAndRest
  where
    splitFirstLetterAndRest = splitAt 1
    applyTransformationToFirstLetter =
      (\strPair -> strPair {before = transformation strPair.before})
    joinFirstLetterWithRest = (\strPair -> strPair.before <> strPair.after)

-- | Makes the first letter of a string lower case
lowerCaseFirst :: String -> String
lowerCaseFirst = changeFirstLetter toLower

-- | Makes the first letter of a string upper case
upperCaseFirst :: String -> String
upperCaseFirst = changeFirstLetter toUpper

-- | Turns an array of TsType into a "squashed" string.
-- | (Array SomeType) -> ArraySomeType
squishTypesToString :: Array TsType -> String
squishTypesToString = fold <<<
                        map upperCaseFirst <<<
                        map (removeSubstringFromString " ") <<<
                        map (removeSubstringFromString ")") <<<
                        map (removeSubstringFromString "(") <<<
                        map (\x -> tsTypeToString x) -- these might have spaces or parenthesis


removeSubstringFromString substr = replaceAll (Pattern substr) (Replacement "")

-- | This turns a union type into a string for signatures of functions.
-- | The type still has to be created using tsUnionTypeToPsUnionType and printed.
tsUnionTypeToString unionType = "UT_" <>
                                upperCaseFirst unionType.name <>
                                "_" <>
                                (squishTypesToString unionType.unionTypes)


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
    TsNull   ->  "Unit"
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
