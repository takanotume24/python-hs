module PythonHS.VM.CompileCompoundAssign (compileCompoundAssign) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (ApplyBinary, LoadName, StoreName), Instruction)

compileCompoundAssign :: (Int -> Expr -> Either String ([Instruction], Int)) -> Int -> String -> Expr -> Position -> BinaryOperator -> Either String ([Instruction], Int)
compileCompoundAssign compileExprAt baseIndex name expr pos op = do
  (exprCode, exprEnd) <- compileExprAt (baseIndex + 1) expr
  let code = [LoadName name pos] ++ exprCode ++ [ApplyBinary op pos, StoreName name]
  pure (code, exprEnd + 2)
