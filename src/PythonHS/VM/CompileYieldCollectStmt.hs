module PythonHS.VM.CompileYieldCollectStmt (compileYieldCollectStmt) where

import PythonHS.AST.Expr (Expr (CallExpr, IdentifierExpr))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (StoreName))

compileYieldCollectStmt :: (Int -> Expr -> Either String ([Instruction], Int)) -> Int -> String -> Expr -> Position -> Either String ([Instruction], Int)
compileYieldCollectStmt compileExprAt baseIndex operation yieldedExpr pos = do
  (collectCode, collectEnd) <-
    compileExprAt
      baseIndex
      (CallExpr operation [IdentifierExpr "__yield_acc__" pos, yieldedExpr] pos)
  pure (collectCode ++ [StoreName "__yield_acc__"], collectEnd + 1)
