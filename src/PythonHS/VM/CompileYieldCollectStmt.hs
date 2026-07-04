module PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

compileYieldCollectStmt :: (Int -> Expr -> Either String CompileExprResult) -> Int -> String -> Expr -> Position -> Either String CompileExprResult
compileYieldCollectStmt compileExprAt baseIndex operation yieldedExpr pos = do
  collectResult <-
    compileExprAt
      baseIndex
      (CallExpr {callExprName = operation, callExprArgs = [IdentifierExpr {identifierExprName = "__yield_acc__", identifierExprPos = pos}, yieldedExpr], callExprPos = pos})
  pure (CompileExprResult {compileExprResultCode = compileExprResultCode collectResult ++ [StoreName {storeNameName = "__yield_acc__"}], compileExprResultEndIndex = compileExprResultEndIndex collectResult + 1})
