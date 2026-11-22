module PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.VM.CompileDictEntriesAtConfig (CompileDictEntriesAtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))

compileDictEntriesAt :: CompileDictEntriesAtConfig -> Either String CompileExprResult
compileDictEntriesAt config =
  let compileExpr = compileDictEntriesAtCompileExpr config
      baseIndex = compileDictEntriesAtBaseIndex config
      entries = compileDictEntriesAtEntries config
   in case entries of
        [] -> Right CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex}
        DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr} : rest -> do
          keyResult <- compileExpr baseIndex keyExpr
          valueResult <- compileExpr (compileExprResultEndIndex keyResult) valueExpr
          restResult <- compileDictEntriesAt config {compileDictEntriesAtCompileExpr = compileExpr, compileDictEntriesAtBaseIndex = compileExprResultEndIndex valueResult, compileDictEntriesAtEntries = rest}
          pure CompileExprResult {compileExprResultCode = compileExprResultCode keyResult ++ compileExprResultCode valueResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult}
