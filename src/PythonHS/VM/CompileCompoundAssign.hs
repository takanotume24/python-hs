module PythonHS.VM.CompileCompoundAssign (compileCompoundAssign) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (ApplyBinary, LoadName, StoreName))

compileCompoundAssign :: (Int -> Expr -> Either String CompileExprResult) -> Int -> String -> Expr -> Position -> BinaryOperator -> Either String CompileExprResult
compileCompoundAssign compileExprAt baseIndex name expr pos op = do
  exprResult <- compileExprAt (baseIndex + 1) expr
  let code = [LoadName name pos] ++ compileExprResultCode exprResult ++ [ApplyBinary op pos, StoreName name]
  pure (CompileExprResult code (compileExprResultEndIndex exprResult + 2))
