module PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileDictEntriesAtConfig (CompileDictEntriesAtConfig (..))

compileDictEntriesAt :: CompileDictEntriesAtConfig a -> Either String ([a], Int)
compileDictEntriesAt config =
  let compileExpr = compileDictEntriesAtCompileExpr config
      baseIndex = compileDictEntriesAtBaseIndex config
      entries = compileDictEntriesAtEntries config
   in case entries of
        [] -> Right ([], baseIndex)
        (keyExpr, valueExpr) : rest -> do
          (keyCode, keyEnd) <- compileExpr baseIndex keyExpr
          (valueCode, valueEnd) <- compileExpr keyEnd valueExpr
          (restCode, restEnd) <- compileDictEntriesAt config {compileDictEntriesAtCompileExpr = compileExpr, compileDictEntriesAtBaseIndex = valueEnd, compileDictEntriesAtEntries = rest}
          pure (keyCode ++ valueCode ++ restCode, restEnd)
