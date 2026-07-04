module PythonHS.VM.CompileDictEntriesAtConfig (CompileDictEntriesAtConfig (..)) where

import PythonHS.AST.Expr (Expr)

data CompileDictEntriesAtConfig a = CompileDictEntriesAtConfig
  { compileDictEntriesAtCompileExpr :: Int -> Expr -> Either String ([a], Int),
    compileDictEntriesAtBaseIndex :: Int,
    compileDictEntriesAtEntries :: [(Expr, Expr)]
  }
