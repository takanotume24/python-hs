module PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt) where

import PythonHS.AST.Expr (Expr (CallExpr, IdentifierExpr))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (StoreName))

compileYieldCollectStmt :: (Int -> Expr -> Either String CompileExprResult) -> Int -> String -> Expr -> Position -> Either String CompileExprResult
compileYieldCollectStmt compileExprAt baseIndex operation yieldedExpr pos = do
  collectResult <-
    compileExprAt
      baseIndex
      (CallExpr operation [IdentifierExpr "__yield_acc__" pos, yieldedExpr] pos)
  pure (CompileExprResult (compileExprResultCode collectResult ++ [StoreName "__yield_acc__"]) (compileExprResultEndIndex collectResult + 1))
