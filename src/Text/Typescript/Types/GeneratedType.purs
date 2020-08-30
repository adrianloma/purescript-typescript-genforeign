module Text.Typesript.Types.GeneratedType (module Text.Typesript.Types.GeneratedType)
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
  { functions :: Array TypeScriptFunction
  , classes :: Array TypeScriptClass
  , enums :: Array Enum
  , interfaces :: Array Interface
  }

type TypeScriptClass =
  { name :: String
  , documentation :: String
  , constructors :: Array ClassConstructor
  , methods :: Array Method
  }

type ClassConstructor = TypeScriptFunction

type Parameter =
  { name :: String
  , documentation :: String
  , typeScriptType :: TypeScriptType
  }


type Method = TypeScriptFunction

type Enum =
  { name :: String
  , members :: Array String
  }

type TypeScriptFunction =
  { name :: String
  , documentation :: String
  , parameters :: Array Parameter
  , typeScriptType :: TypeScriptType
  }

type ConstructorType =
  { typeConstructor :: String
  , typeParameters :: TypeParameter
  }

type Interface =
  { name :: String
  , properties :: Array Parameter
  }

data TypeParameter =
  SingletonTypeParameter TypeScriptType
  | ArrayTypeParameters (Array TypeScriptType)

data TypeScriptType =
  CompositeType ConstructorType
  | PrimitiveType PrimitiveTypeScriptType
  | TypeReference String

data PrimitiveTypeScriptType =
    TypeScriptAny
  | TypeScriptNumber
  | TypeScriptVoid
  | TypeScriptString

derive instance genericTypeParameter :: Generic TypeParameter _
instance encodeJsonTypeParameter :: EncodeJson TypeParameter where
  encodeJson wt = case wt of
    (SingletonTypeParameter val) -> encodeJson val
    (ArrayTypeParameters val) -> encodeJson val

derive instance genericTypeScriptType :: Generic TypeScriptType _
instance encodeJsonTypeScriptType :: EncodeJson TypeScriptType where
  encodeJson = genericEncodeJson

derive instance genericPrimitiveTypeScriptType :: Generic PrimitiveTypeScriptType _
instance encodeJsonPrimitiveTypeScriptType :: EncodeJson PrimitiveTypeScriptType where
  encodeJson = genericEncodeJson


-- This is a recursive data type with TypeParameter
-- So the recursion is unrolled here.
instance decodeJsonTypeScriptType :: DecodeJson TypeScriptType where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TypeScriptType") $
          PrimitiveType <$> decodeJson obj
      <|> TypeReference <$> decodeJson obj
      <|> CompositeType <$> decodeJson obj

instance decodeJsonTypeParameter :: DecodeJson TypeParameter where
  decodeJson json = do
    obj <- decodeJson json
    lmap (const $ TypeMismatch "TypeParameter") $
          SingletonTypeParameter <$> decodeJson obj
      <|> ArrayTypeParameters <$> decodeJson obj

instance decodeJsonPrimitiveTypeScriptType :: DecodeJson PrimitiveTypeScriptType where
  decodeJson json = do
    obj <- decodeJson json
    note (TypeMismatch "PrimitiveTypeScriptType") (decodePrimitiveType obj)
    where
      decodePrimitiveType a = case a of
        "any" -> Just TypeScriptAny
        "number" -> Just TypeScriptNumber
        "void" -> Just TypeScriptVoid
        "string" -> Just TypeScriptString
        _       -> Nothing
