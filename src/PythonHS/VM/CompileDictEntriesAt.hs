module PythonHS.VM.CompileDictEntriesAt (compileDictEntriesAt) where

import PythonHS.AST.Expr (Expr)

compileDictEntriesAt ::
  (Int -> Expr -> Either String ([a], Int)) ->
  Int ->
  [(Expr, Expr)] ->
  Either String ([a], Int)
compileDictEntriesAt compileExprAt baseIndex entries =
  case entries of
    [] -> Right ([], baseIndex)
    (keyExpr, valueExpr) : rest -> do
      (keyCode, keyEnd) <- compileExprAt baseIndex keyExpr
      (valueCode, valueEnd) <- compileExprAt keyEnd valueExpr
      (restCode, restEnd) <- compileDictEntriesAt compileExprAt valueEnd rest
      pure (keyCode ++ valueCode ++ restCode, restEnd)
