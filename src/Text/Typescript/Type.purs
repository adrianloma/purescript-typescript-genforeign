module Text.TypeScript.Type
       (module Text.TypeScript.Type)
       where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Core as RAC
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (note, Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

type SourceFile =
  { functions :: Array TSFunction
  , classes :: Array Class
  , enums :: Array Enum
  , interfaces :: Array Interface
  }

type Class =
  { name :: String
  , documentation :: String
  , constructors :: Array ClassConstructor
  , methods :: Array ClassMethod
  }

type Parameter =
  { name :: String
  , documentation :: String
  , typeScriptType :: TSType
  }

type Enum =
  { name :: String
  , members :: Array String
  }

type Interface =
  { name :: String
  , properties :: Array Parameter
  }

type ClassMethod = TSFunction
type ClassConstructor = TSFunction
type TSFunction =
  { name :: String
  , documentation :: String
  , parameters :: Array Parameter
  , typeScriptType :: TSType
  }

type ConstructorType =
  { typeConstructor :: String
  , typeParameters :: TypeParameter
  }

data TypeParameter =
  SingletonTypeParameter TSType
  | ArrayTypeParameters (Array TSType)

data TSType =
  CompositeType ConstructorType
  | PrimitiveType PrimitiveTSType
  | TypeReference String

data PrimitiveTSType =
    TSAny
  | TSNumber
  | TSVoid
  | TSString
  | TSUndefined
  | TSObject
  | TSBoolean
  -- | TSBigInt -- No support


derive instance genericTypeParameter :: Generic TypeParameter _
instance encodeJsonTypeParameter :: EncodeJson TypeParameter where
  encodeJson wt = case wt of
    (SingletonTypeParameter val) -> encodeJson val
    (ArrayTypeParameters val) -> encodeJson val

derive instance genericTSType :: Generic TSType _
instance encodeJsonTSType :: EncodeJson TSType where
  encodeJson = genericEncodeJson

derive instance genericPrimitiveTSType :: Generic PrimitiveTSType _
instance encodeJsonPrimitiveTSType :: EncodeJson PrimitiveTSType where
  encodeJson = genericEncodeJson


instance decodeJsonTSType :: DecodeJson TSType where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TSType") $
          PrimitiveType <$> decodeJson obj
      <|> TypeReference <$> decodeJson obj
      <|> CompositeType <$> decodeJson obj

instance decodeJsonTypeParameter :: DecodeJson TypeParameter where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TypeParameter") $
          SingletonTypeParameter <$> decodeJson obj
      <|> ArrayTypeParameters <$> decodeJson obj

instance decodeJsonPrimitiveTSType :: DecodeJson PrimitiveTSType where
  decodeJson json = do
    obj <- decodeJson json
    note (TypeMismatch "PrimitiveTSType") (decodePrimitiveType obj)
    where
      decodePrimitiveType a = case a of
        "any" -> Just TSAny
        "number" -> Just TSNumber
        "void" -> Just TSVoid
        "string" -> Just TSString
        "undefined" -> Just TSUndefined
        "object" -> Just TSObject
        "boolean" -> Just TSBoolean
        _       -> Nothing
