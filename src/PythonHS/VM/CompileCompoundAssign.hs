module PythonHS.VM.CompileCompoundAssign (compileCompoundAssign) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileCompoundAssignConfig (CompileCompoundAssignConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (..))

compileCompoundAssign :: CompileCompoundAssignConfig -> Either String CompileExprResult
compileCompoundAssign config =
  let compileExpr = compileCompoundAssignCompileExpr config
      baseIndex = compileCompoundAssignBaseIndex config
      name = compileCompoundAssignName config
      expr = compileCompoundAssignExpr config
      pos = compileCompoundAssignPos config
      op = compileCompoundAssignOp config
   in do
        exprResult <- compileExpr (baseIndex + 1) expr
        let code = [LoadName {loadNameName = name, loadNamePos = pos}] ++ compileExprResultCode exprResult ++ [ApplyBinary {applyBinaryOp = op, applyBinaryPos = pos}, StoreName {storeNameName = name}]
        pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = compileExprResultEndIndex exprResult + 2})
