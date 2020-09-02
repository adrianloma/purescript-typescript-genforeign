module Text.TypeScript.GenForeign where

import Prelude

import Data.Array.NonEmpty as NE
import Text.TypeScript.GenForeign.GenJs (JsString, jsModuleToString, tsSourceModuleToJsModule)
import Text.TypeScript.GenForeign.GenPs (PsString, PsModule, psModuleToString, tsSourceFileToPsModule)
import Text.TypeScript.Type (SourceFile)

tsSourceFileToJsModuleString :: String -> SourceFile -> JsString
tsSourceFileToJsModuleString sourceOrigin sourceFile = jsModuleToString $ tsSourceModuleToJsModule sourceOrigin sourceFile

tsSourceFileToPsModuleString :: String -> SourceFile -> PsString
tsSourceFileToPsModuleString moduleName sourceFile =
  psModuleToString $
  changeFuncReturnTypeWith (\str -> "Effect " <> str) $ 
  tsSourceFileToPsModule moduleName sourceFile

type PsModuleFilter = PsModule -> PsModule

changeFuncReturnTypeWith :: (String -> String) -> PsModuleFilter
changeFuncReturnTypeWith stringTransform = \psModule ->
  psModule {functions = map changeReturnType psModule.functions}
  where
    transformLast { init: init, last: last } = NE.snoc' init (stringTransform last)
    changeReturnType func = func {types = transformLast $ NE.unsnoc func.types}



