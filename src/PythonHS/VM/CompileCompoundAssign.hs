module PythonHS.VM.CompileCompoundAssign (compileCompoundAssign) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

compileCompoundAssign :: (Int -> Expr -> Either String CompileExprResult) -> Int -> String -> Expr -> Position -> BinaryOperator -> Either String CompileExprResult
compileCompoundAssign compileExprAt baseIndex name expr pos op = do
  exprResult <- compileExprAt (baseIndex + 1) expr
  let code = [LoadName {loadNameName = name, loadNamePos = pos}] ++ compileExprResultCode exprResult ++ [ApplyBinary {applyBinaryOp = op, applyBinaryPos = pos}, StoreName {storeNameName = name}]
  pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 2})
