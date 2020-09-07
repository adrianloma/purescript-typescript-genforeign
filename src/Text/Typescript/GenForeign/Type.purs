module Text.TypeScript.Type where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

type SourceFile =
  { functions :: Array TsFunction
  , classes :: Array Class
  , enums :: Array Enum
  , interfaces :: Array Interface
  }

type Class =
  { name :: String
  , docs :: String
  , constructors :: Array ClassConstructor
  , methods :: Array ClassMethod
  }

type Param =
  { name :: String
  , docs :: String
  , tsType :: TsType
  }

type Enum =
  { name :: String
  , members :: Array String
  }

type TypeLiteral = Interface
type Interface =
  { name :: String
  , properties :: Array Param
  }

type ClassMethod = TsFunction
type ClassConstructor = TsFunction
type FunctionType = TsFunction
type TsFunction =
  { name :: String
  , docs :: String
  , params :: Array Param
  , tsType :: TsType
  }

type ConstructorType =
  { typeConstructor :: String
  , typeParams :: TypeParam
  }

data TypeParam =
  SingletonTypeParam TsType
  | ArrayTypeParams (Array TsType)


type UnionType =
  { name :: String
  , unionTypes :: Array TsType
  }

data TsType =
  CompositeType ConstructorType
  | PrimitiveType PrimitiveTsType
  | TsTypeUnionType UnionType
  | TsTypeTypeLiteral TypeLiteral
  | TsTypeFunctionType FunctionType
  | TypeReference String

data PrimitiveTsType =
    TsAny
  | TsNumber
  | TsVoid
  | TsString
  | TsUndefined
  | TsObject
  | TsBoolean

derive instance genericTypeParam :: Generic TypeParam _
instance encodeJsonTypeParam :: EncodeJson TypeParam where
  encodeJson wt = case wt of
    (SingletonTypeParam val) -> encodeJson val
    (ArrayTypeParams val) -> encodeJson val

derive instance genericTsType :: Generic TsType _
instance encodeJsonTsType :: EncodeJson TsType where
  encodeJson tsType = case tsType of
    CompositeType a -> encodeJson a
    PrimitiveType a -> encodeJson a
    TsTypeUnionType a -> encodeJson a
    TsTypeTypeLiteral a -> encodeJson a
    TsTypeFunctionType a -> encodeJson a
    TypeReference a -> encodeJson a

derive instance genericPrimitiveTsType :: Generic PrimitiveTsType _
instance encodeJsonPrimitiveTsType :: EncodeJson PrimitiveTsType where
  encodeJson = genericEncodeJson


instance decodeJsonTsType :: DecodeJson TsType where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TsType") $
          CompositeType <$> decodeJson obj
      <|> PrimitiveType <$> decodeJson obj
      <|> TsTypeUnionType <$> decodeJson obj
      <|> TsTypeFunctionType <$> decodeJson obj
      <|> TsTypeTypeLiteral <$> decodeJson obj
      <|> TypeReference <$> decodeJson obj

instance decodeJsonTypeParam :: DecodeJson TypeParam where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TypeParam") $
          SingletonTypeParam <$> decodeJson obj
      <|> ArrayTypeParams <$> decodeJson obj

instance decodeJsonPrimitiveTsType :: DecodeJson PrimitiveTsType where
  decodeJson json = do
    obj <- decodeJson json
    note (TypeMismatch "PrimitiveTsType") (decodePrimitiveType obj)
    where
      decodePrimitiveType a = case a of
        "any" -> Just TsAny
        "number" -> Just TsNumber
        "void" -> Just TsVoid
        "string" -> Just TsString
        "undefined" -> Just TsUndefined
        "object" -> Just TsObject
        "boolean" -> Just TsBoolean
        _       -> Nothing
