module Text.TypeScript.GenForeign where

import Text.TypeScript.GenForeign.GenJs (JsString, jsModuleToString, tsSourceModuleToJsModule)
import Text.TypeScript.GenForeign.GenPs (PsString, psModuleToString, tsSourceFileToPsModule)
import Text.TypeScript.Type (SourceFile)
import Prelude

tsSourceFileToJsModuleString :: String -> SourceFile -> JsString
tsSourceFileToJsModuleString sourceOrigin sourceFile = jsModuleToString $ tsSourceModuleToJsModule sourceOrigin sourceFile

tsSourceFileToPsModuleString :: String -> SourceFile -> PsString
tsSourceFileToPsModuleString moduleName sourceFile = psModuleToString $ tsSourceFileToPsModule moduleName sourceFile




