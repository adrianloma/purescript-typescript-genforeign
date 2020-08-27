module Text.Typesript.Types.Generate (module Text.Typesript.Types.Generate)
       where

import Data.Maybe

foreign import generateDocumentationForInputString :: String -> String

newtype DocEntry = DocEntry
  { name :: Maybe String
  , fileName :: Maybe String
  , documentation :: Maybe String
  , elementType :: Maybe String
  , constructors :: Maybe (Array DocEntry)
  , parameters :: Maybe (Array DocEntry)
  , returnType :: Maybe String
  }
