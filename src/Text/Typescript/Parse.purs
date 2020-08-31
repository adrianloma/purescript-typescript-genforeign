module Text.TypeScript.Parse (module Text.TypeScript.Parse)
       where



import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Either (Either)
import Prelude
import Text.TypeScript.Type (SourceFile)

foreign import generateDocumentationForInputString :: String -> String

tryParseSourceFile :: String -> Either JsonDecodeError SourceFile
tryParseSourceFile sourceString =  do
                      let jsonToProcess = generateDocumentationForInputString sourceString
                      json <- parseJson jsonToProcess
                      decodeJson json
