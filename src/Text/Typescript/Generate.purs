module Text.Typesript.Generate (module Text.Typesript.Generate)
       where



import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Either (Either)
import Prelude
import Text.Typesript.Types.GeneratedType (SourceFile)

foreign import generateDocumentationForInputString :: String -> String

tryParseSourceFile :: String -> Either JsonDecodeError SourceFile
tryParseSourceFile sourceString =  do
                      let jsonToProcess = generateDocumentationForInputString sourceString
                      json <- parseJson jsonToProcess
                      decodeJson json
