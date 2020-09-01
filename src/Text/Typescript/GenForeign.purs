module Text.TypeScript.GenForeign where

import Text.TypeScript.GenForeign.GenJs
import Text.TypeScript.GenForeign.GenPs
import Text.TypeScript.Type
import Prelude

tsSourceFileToJsModuleString :: String -> SourceFile -> JsString
tsSourceFileToJsModuleString sourceOrigin sourceFile = jsModuleToString $ tsSourceModuleToJsModule sourceOrigin sourceFile

tsSourceFileToPsModuleString :: String -> SourceFile -> PsString
tsSourceFileToPsModuleString moduleName sourceFile = psModuleToString $ tsSourceFileToPsModule moduleName sourceFile
