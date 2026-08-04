module PythonHS.VM.CompileDictEntriesAtConfig (CompileDictEntriesAtConfig (..)) where

import PythonHS.AST.DictEntry (DictEntry)
import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileDictEntriesAtConfig = CompileDictEntriesAtConfig
  { compileDictEntriesAtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDictEntriesAtBaseIndex :: Int,
    compileDictEntriesAtEntries :: [DictEntry Expr]
  }
